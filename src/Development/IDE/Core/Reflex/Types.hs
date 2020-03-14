module Development.IDE.Core.Reflex.Types where

import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NL
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Diagnostics
import qualified Data.SortedList as SL
import qualified Data.Text as T
import Data.List
import qualified Data.HashMap.Strict as HMap
import Control.Monad.Fix
import Data.Functor
import Data.Functor.Barbie
import Data.Functor.Product
import Language.Haskell.LSP.Core
import Data.Kind
import Reflex.Host.Class
import qualified Data.ByteString.Char8 as BS
import Development.IDE.Core.RuleTypes
import Control.Monad.Extra
import Control.Monad.Reader
import Data.GADT.Show
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Control.Error.Util
import qualified Data.Dependent.Map as D
import Data.Dependent.Map (DMap, DSum(..))
import Data.These(These(..))
import Reflex.Time
import Reflex.Network
import System.Directory
import Reflex
import GHC hiding (parseModule, mkModule, typecheckModule, getSession)
import qualified GHC
import Reflex.PerformEvent.Class
import Development.IDE.Core.Compile
import Data.Default
import Control.Monad.IO.Class
import Development.IDE.Types.Location
import StringBuffer
import Development.IDE.Types.Options
import Data.Dependent.Map (GCompare)
import Data.GADT.Compare
import qualified Data.Map as M
import Unsafe.Coerce
import Reflex.Host.Basic
import Development.IDE.Import.FindImports
import Control.Monad
import HscTypes
import Data.Either
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Module hiding (mkModule)
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad.State.Strict
import Development.IDE.Types.Diagnostics
import Development.IDE.Import.DependencyInformation
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Coerce
import Data.Traversable
import qualified GHC.LanguageExtensions as LangExt
import DynFlags
import Development.IDE.Spans.Type
import Development.IDE.Spans.Calculate
--import HIE.Bios
--import HIE.Bios.Environment
import System.Environment
import System.IO
import Linker
--import qualified GHC.Paths
import Control.Concurrent
import Reflex.Profiled
import Debug.Trace
import Control.Monad.Ref
import Reflex.Host.Class
import Data.Time.Clock

import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as LSP
import qualified Language.Haskell.LSP.Types.Capabilities as LSP
import Development.IDE.Types.Logger
import Development.IDE.Core.Debouncer

import Development.IDE.Core.Reflex.Thunk
import Development.IDE.Core.Reflex.Early
import Development.IDE.Core.Reflex.Diagnostics
import Development.IDE.Core.Reflex.Constraints

type IdeResult v = ([FileDiagnostic], Maybe v)

-- Normal action without early cutoff
type Action t m a = NormalizedFilePath -> ActionM t m (IdeResult a)

-- An action with early cutoff
type EarlyAction t m a = NormalizedFilePath
                       -> ActionM t m (Maybe BS.ByteString, IdeResult a)


type ActionM t m a = (ReaderT (REnv t) (MaybeT (EventWriterT t DependencyType [EType] m)) a)

type DynamicM t m a = (ReaderT (REnv t) m (Dynamic t a))

type BasicM t m a = (ReaderT (REnv t) m a)


data REnv t = REnv { global :: GlobalEnv t
                   , module_map :: ModuleMapWithUpdater t
                   }


liftActionM :: Monad m => m a -> ActionM t m a
liftActionM = lift . lift . lift





data ForallAction a where
  ForallAction :: (forall t . C t => ActionM t (HostFrame t) a) -> ForallAction a

data ForallDynamic a where
  ForallDynamic :: (forall t . C t => DynamicM t (BasicGuestWrapper t) a) -> ForallDynamic a

data ForallBasic a where
  ForallBasic :: (forall t . C t => BasicM t (BasicGuestWrapper t) a) -> ForallBasic a

newtype WrappedAction m t a =
          WrappedAction { getAction :: Action t m a }

newtype WrappedEarlyAction m t a =
         WrappedEarlyAction { getEarlyAction :: EarlyAction t m a }

data WrappedEarlyActionWithTrigger m t a =
  WrappedEarlyActionWithTrigger { getEarlyActionWithTrigger :: EarlyAction t (Performable m) a
                                -- The trigger can depend on the state but
                                -- can't fail or write events
                                , actionTrigger :: NormalizedFilePath -> BasicM t m (Event t ())
                                }

newtype WrappedActionM m t a =
          WrappedActionM { getActionM :: ActionM t m a }

newtype WrappedDynamicM m t a =
          WrappedDynamicM { getDynamicM :: DynamicM t m a }

type Definition rt f t  = D.DSum rt (f t)

newtype BasicGuestWrapper t a =
          BasicGuestWrapper { unwrapBG :: (forall m . BasicGuestConstraints t m => BasicGuest t m a) }

instance Reflex t => Adjustable t (BasicGuestWrapper t) where
  runWithReplace m e = BasicGuestWrapper (runWithReplace (unwrapBG m) (fmap unwrapBG e))
  traverseIntMapWithKeyWithAdjust k m e
    = BasicGuestWrapper (traverseIntMapWithKeyWithAdjust (\key val -> unwrapBG (k key val))
                                                         m e)
  traverseDMapWithKeyWithAdjust kf dm e
    = BasicGuestWrapper (traverseDMapWithKeyWithAdjust (\k v -> unwrapBG (kf k v)) dm e)
  traverseDMapWithKeyWithAdjustWithMove kf dm e
    = BasicGuestWrapper (traverseDMapWithKeyWithAdjustWithMove (\k v -> unwrapBG (kf k v)) dm e)

instance Monad (BasicGuestWrapper t) where
  return x = BasicGuestWrapper (return x)
  (BasicGuestWrapper a) >>= f = BasicGuestWrapper (a >>= unwrapBG . f)

instance Applicative (BasicGuestWrapper t) where
  pure x = BasicGuestWrapper (pure x)
  (BasicGuestWrapper a) <*> (BasicGuestWrapper b) = BasicGuestWrapper (a <*> b)

instance Functor (BasicGuestWrapper t) where
  fmap f (BasicGuestWrapper a) = BasicGuestWrapper (fmap f a)

instance MonadHold t (BasicGuestWrapper t) where
  buildDynamic a e = BasicGuestWrapper (buildDynamic a e)
  headE e = BasicGuestWrapper (headE e)
  hold a e = BasicGuestWrapper (hold a e)
  holdDyn a e = BasicGuestWrapper (holdDyn a e)
  holdIncremental a e = BasicGuestWrapper (holdIncremental a e)

instance MonadSample t (BasicGuestWrapper t) where
  sample b = BasicGuestWrapper (sample b)

instance MonadFix (BasicGuestWrapper t) where
  mfix f = BasicGuestWrapper (mfix (unwrapBG . f))

instance MonadIO (BasicGuestWrapper t) where
  liftIO m = BasicGuestWrapper (liftIO m)

instance TriggerEvent t (BasicGuestWrapper t) where
  newTriggerEvent = BasicGuestWrapper newTriggerEvent

instance (Monad (HostFrame t), Reflex t) => PerformEvent t (BasicGuestWrapper t) where
    type Performable (BasicGuestWrapper t) = HostFrame t
    performEvent m = BasicGuestWrapper (performEvent m)
    performEvent_ m = BasicGuestWrapper (performEvent_ m)
