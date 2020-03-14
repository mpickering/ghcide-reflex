{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Development.IDE.Core.Reflex.Thunk where

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
import System.IO
import Linker
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


-- Like a Maybe, but has to be activated the first time we try to access it
data Thunk a = Value a | Awaiting | Seed (IO ()) deriving Functor

joinThunk :: Thunk (Maybe a) -> Thunk a
joinThunk (Value m) = maybe Awaiting Value m
joinThunk Awaiting = Awaiting
joinThunk (Seed trig) = (Seed trig)

splitThunk :: a -> Thunk (a, b)  -> (a, Thunk b)
splitThunk _ (Value (l, a)) = (l, Value a)
splitThunk a (Awaiting)  = (a, Awaiting)
splitThunk a (Seed trig) = (a, Seed trig)

thunk :: _ => Event t (Maybe a) -> m (Dynamic t (Thunk a), Event t ())
thunk e  = do
  (start, grow) <- newTriggerEvent
  -- This headE is very important.
  -- If you remove it then at the star of the program the event gets
  -- triggered many times which leads to a lot of computation happening.
  -- It used to be "batchOccurences" which worked somewhat but not on
  -- a bigger code base like GHC. It is best to enforce that the start
  -- event only fires once using headE.
  start' <- headE start
  let trig = grow Awaiting
  d <- holdDyn (Seed trig) (leftmost [maybe Awaiting Value <$> e
                                                , Awaiting <$ start' ])
  -- Only allow the thunk to improve
  d' <- improvingResetableThunk d
  return (d', () <$ start')

forceThunk :: _ => Dynamic t (Thunk a) -> m ()
forceThunk d = do
  t <- sample (current d)
  case t of
    Seed start -> liftIO start
    _ -> return ()

sampleThunk :: _ => Dynamic t (Thunk a) -> m (Maybe a)
sampleThunk d = do
  t <- sample (current d)
  case t of
    Seed start -> liftIO start >> return Nothing
    Awaiting   -> return Nothing
    Value a    -> return (Just a)


-- Like improvingMaybe, but for the Thunk type
improvingResetableThunk  :: _ => Dynamic t (Thunk a) -> m (Dynamic t (Thunk a))
improvingResetableThunk = scanDynMaybe id upd
  where
    -- ok, if you insist, write the new value
    upd (Value a) _ = --trace "UPDATING: New Value" $
                      Just (Value a)
    -- Wait, once the trigger is pressed
    upd Awaiting  (Seed {}) = --trace "UPDATING: Awaiting" $
                              Just Awaiting
    -- Allows the thunk to be reset to GC
--    upd s@(Seed {}) _ = Just s
    -- NOPE
    upd _ _ = Nothing
