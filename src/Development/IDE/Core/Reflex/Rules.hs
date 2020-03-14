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
module Development.IDE.Core.Reflex.Rules where

import qualified Data.List.NonEmpty as NL
--import {-# SOURCE #-} Language.Haskell.Core.FileStore (getModificationTime)
import Control.Monad.Fix
import Language.Haskell.LSP.Core
import Reflex.Host.Class
import qualified Data.ByteString.Char8 as BS
import Development.IDE.Core.RuleTypes
import Control.Monad.Reader
import Data.GADT.Show
import Control.Error.Util
import qualified Data.Dependent.Map as D
import Data.Dependent.Map (DMap, DSum(..))
import Reflex.Time
import System.Directory
import Reflex
import Development.IDE.Types.Location
import qualified Data.Map as M
import Reflex.Host.Basic
import Data.Either
import Control.Monad.Trans.Maybe
import Development.IDE.Types.Diagnostics
--import Debug.Trace
import Data.Time.Clock
import HscTypes

import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as LSP
import Development.IDE.Types.Logger
import Development.IDE.Types.Options

import Development.IDE.Core.Reflex.Thunk
import Development.IDE.Core.Reflex.Early
import Development.IDE.Core.Reflex.Diagnostics
import Development.IDE.Core.Reflex.Constraints

{- What is a rule and the ways to define them -}

data Rule t = ModRule (ShakeDefinition (BasicGuestWrapper t) t)
            -- A rule which depends on a particular module
            | GlobalRule (GlobalDefinition (BasicGuestWrapper t) t)
            -- Something which doesn't depend on the FilePath, a global
            -- variable
            | UnitAction (BasicM t (BasicGuestWrapper t) ())
            -- Some other action, such as logging which doesn't produce a dynamic
            -- Also hover, goto definition
            -- Probably also progress as well


data WRule where
  WRule :: (forall t . C t => Rule t) -> WRule

type Rules = [WRule]


partitionRules :: C t => Rules ->
                    ([ShakeDefinition (BasicGuestWrapper t) t]
                    , [GlobalDefinition (BasicGuestWrapper t) t]
                    , [BasicM t (BasicGuestWrapper t) ()] )
partitionRules [] = ([], [], [])
partitionRules (WRule (ModRule r) : xs) = let (rs, gs, us) = partitionRules xs in  (r:rs, gs, us)
partitionRules (WRule (GlobalRule g) : xs) = let (rs, gs, us) = partitionRules xs in  (rs, g:gs, us)
partitionRules (WRule (UnitAction u) : xs) = let (rs, gs, us) = partitionRules xs in  (rs, gs, u:us)

-- The ways to define rules

-- | A rule which always triggers recompilation and is only triggered by
-- dependencies updating.
define :: RuleType a -> (forall t . C t => Action t (HostFrame t) a) -> WRule
define rn a = WRule (ModRule (rn :=> WrappedEarlyActionWithTrigger (fmap (fmap (Nothing,)) a) (const $ return never)))

-- | A rule which includes an additional hash which can be used to cutoff
-- further updates.
defineEarlyCutoff :: RuleType a -> (forall t . C t => EarlyAction t (HostFrame t) a) -> WRule
defineEarlyCutoff rn a = WRule (ModRule (rn :=> WrappedEarlyActionWithTrigger a (const $ return never)))

-- | A rule which has early cutoff and also an additional external trigger
-- which dictates when to update.
defineGen :: RuleType a -> (forall t . C t => NormalizedFilePath -> BasicM t (BasicGuestWrapper t) (Event t ()))
                        -> (forall t . C t => EarlyAction t (HostFrame t) a)
                        -> WRule
defineGen rn trig a = WRule (ModRule (rn :=> WrappedEarlyActionWithTrigger a trig))

-- | An action which is run on initialisation but contributes nothing to
-- the global state.
unitAction :: (forall t . C t => BasicM t (BasicGuestWrapper t) ())
           -> WRule
unitAction a = WRule (UnitAction a)

-- Like define but doesn't depend on the filepath, usually depends on the
-- global input events somehow
addIdeGlobal :: GlobalType a -> (forall t . C t => DynamicM t (BasicGuestWrapper t) a) -> WRule
addIdeGlobal gn a = WRule (GlobalRule (gn :=> WrappedDynamicM a))

-- Normal action without early cutoff
type Action t m a = NormalizedFilePath -> ActionM t m (IdeResult a)

-- An action with early cutoff
type EarlyAction t m a = NormalizedFilePath
                       -> ActionM t m (Maybe BS.ByteString, IdeResult a)



-- EType is mainly used for debugging why an event is firing too often.
data EType = DepTrigger (D.Some RuleType, NormalizedFilePath)
              | MissingTrigger NormalizedFilePath
              | StartTrigger
              | UserTrigger
      deriving Show

-- This datatype partitions the events reported by an action into two
-- types
-- * A blocking dependency is one which MUST fire before we can made any
-- more progress, so we wait for all blocking dependencies before trying
-- again
-- * A trigger dependency is a dependency which suceeded but should trigger
-- recompilation.
data DependencyType a = BlockDependency a
                      | TriggerDependency a

partitionDepTypes :: [DependencyType a]-> ([a], [a])
partitionDepTypes = partitionEithers . map dToEither
  where
    dToEither (BlockDependency x) = Left x
    dToEither (TriggerDependency x) = Right x


tellBlock :: (Reflex t, Monad m) => Event t EType -> ActionM t m ()
tellBlock e = lift $ lift $ tellEventF (BlockDependency ((:[]) <$> e))

tellTrig :: (Reflex t, Monad m) => Event t EType -> ActionM t m ()
tellTrig e = lift $ lift  $ tellEventF (TriggerDependency ((:[]) <$> e))

type ActionM t m a = (ReaderT (REnv t) (MaybeT (EventWriterT t DependencyType [EType] m)) a)

runActionM :: (Reflex t, Monad m) =>
                    ReaderT r1 (MaybeT (EventWriterT t f r2 m)) a
                    -> r1 -> m (Maybe a, [f (Event t r2)])
runActionM act renv = (runEvents  (runMaybeT (flip runReaderT renv act)))

type DynamicM t m a = (ReaderT (REnv t) m (Dynamic t a))

type BasicM t m a = (ReaderT (REnv t) m a)

liftActionM :: Monad m => m a -> ActionM t m a
liftActionM = lift . lift . lift

-- Local environment
data REnv t = REnv { global :: GlobalEnv t
                   , module_map :: ModuleMapWithUpdater t
                   }

data IdeState = IdeState

--data Priority = Priority Int
--setPriority a = return a

data HoverMap = HoverMap
type Diagnostics = String
type RMap t a = M.Map NormalizedFilePath (Dynamic t a)


newtype MDynamic t a = MDynamic { getMD :: Dynamic t (Early (Thunk a)) }

-- All the stuff in here is state which depends on the specific module
data ModuleState t = ModuleState
      { rules :: DMap RuleType (MDynamic t)
      , diags :: Event t (NL.NonEmpty DiagsInfo) }


-- Should also have a variant of a global var which is an incremental
-- rather than a Dynamic for the situation where the global var is storing
-- something like a map and we don't need to trigger updates for everything
-- on every update.
data GlobalVar t a = GlobalVar { dyn :: Dynamic t a
                               , updGlobal :: (a -> a) -> IO ()
                               -- A function which can be used
                               -- to update the dynamic
                               }


data GlobalEnv t = GlobalEnv { globalEnv :: D.DMap GlobalType (GlobalVar t)
                             -- The global state of the application, stuff
                             -- which doesn't depend on the current file.
                             , handlers  :: GlobalHandlers t
                             -- These events fire when the server recieves a message
                             , init_e    :: Event t InitParams
                             -- This event fires when the server is
                             -- initialised
                             , ideLogger :: Logger
                             }

type GlobalHandlers t = HandlersG (Event t)


type ModuleMap t = (Incremental t (PatchMap NormalizedFilePath (ModuleState t)))

data ModuleMapWithUpdater t =
  MMU {
    getMap :: ModuleMap t
    , updateMap :: [(D.Some RuleType, NormalizedFilePath)] -> IO ()
    }

currentMap :: Reflex t =>
                    ModuleMapWithUpdater t
                    -> Behavior t (M.Map NormalizedFilePath (ModuleState t))
currentMap = currentIncremental . getMap
updatedMap :: Reflex t =>
                    ModuleMapWithUpdater t
                    -> Event t (PatchMap NormalizedFilePath (ModuleState t))
updatedMap = updatedIncremental . getMap


type IdeResult v = ([FileDiagnostic], Maybe v)

-- Per module rules are only allowed to sample dynamics, so can be
-- triggered by external events but the external events also trigger
-- some global state to be updated.
type ShakeDefinition m t = Definition RuleType (WrappedEarlyActionWithTrigger m) t

-- Global definitions build dynamics directly as they need to populate
-- the global state
type GlobalDefinition m t = Definition GlobalType (WrappedDynamicM m) t


runEvents :: (Reflex t, Monad m) =>
                   EventWriterT t f r m a -> m (a, [f (Event t r)])
runEvents = runEventWriterTWithComb

--

traceAction :: MonadIO m => [Char] -> m a -> m a
traceAction _ident a = a
{-
traceAction ident a = do
  liftIO $ traceEventIO ("START:" ++ ident)
  r <- a
  liftIO $ traceEventIO ("END:" ++ ident)
  return r
  -}

use_ :: (Monad m
       , Reflex t
       , MonadIO m
       , MonadSample t m)
       => RuleType a
       -> NormalizedFilePath
       -> ActionM t m a
use_ sel fp = do
  r <- trigToBlock sel fp
  lift $ MaybeT (return r)

-- If the result is present then return a trigger otherwise report
-- a blocker.
trigToBlock :: (Reflex t, MonadIO m, MonadSample t m)
              => RuleType a -> NormalizedFilePath -> ActionM t m (Maybe a)
trigToBlock sel fp = do
  (r, dep) <- (useX sel fp)
  case r of
    Just _v -> tellTrig dep
    Nothing -> tellBlock dep
  return r

-- It is important that uses_ is not implemented in terms of use_ as MaybeT
-- short circuits. We want to try all the files in the list so that we can
-- suitably block for all of the events at once rather than one at a time.
uses_ :: (Reflex t, MonadIO m, MonadSample t m) => RuleType a
              -> [NormalizedFilePath]
              -> ActionM t m [a]
uses_ sel fps = do
  ms <- mapM (trigToBlock sel) fps
  lift $ MaybeT (return $ sequence ms)


-- Uses doens't need to be implememented specially because it will just
-- return trigger events.
uses :: (Reflex t, MonadIO m, MonadSample t m) => RuleType a
             -> [NormalizedFilePath]
             -> ActionM t m [Maybe a]
uses sel fps = mapM (use sel) fps

useWithStale :: (Reflex t, MonadIO m, MonadSample t m) =>
                      RuleType a -> NormalizedFilePath -> ActionM t m (Maybe a)
useWithStale = use

use :: (Monad m
       , Reflex t
       , MonadIO m
       , MonadSample t m)
         => RuleType a
         -> NormalizedFilePath
         -> ActionM t m (Maybe a)
use sel fp = do
  (res, e) <- useX sel fp
  tellTrig e
  return res

useBlock :: (Monad m
       , Reflex t
       , MonadIO m
       , MonadSample t m)
         => RuleType a
         -> NormalizedFilePath
         -> ActionM t m (Maybe a)
useBlock sel fp = do
  (res, e) <- useX sel fp
  tellBlock e
  return res

-- This is the key function used to define rules which samples part of the
-- state and reports the dependency. The function also deals with
-- initialising parts of the state which have never been triggered yet.
useX :: (Monad m
       , Reflex t
       , MonadIO m
       , MonadSample t m)
         => RuleType a
         -> NormalizedFilePath
         -> ActionM t m (Maybe a, Event t EType)
useX sel fp = do
  m <- askModuleMap
  mm <- liftActionM $ sample (currentMap m)
  case M.lookup fp mm of
    Just ms -> do
      d <- lift $ hoistMaybe (getMD <$> D.lookup sel (rules ms))
      -- Only trigger a rebuild if the dependency gets filled in with
      -- a value, not the seed to arising transition
      let dep_e = (DepTrigger (D.Some sel, fp)) <$! updatedThunk (unearly <$> d)
      res <- liftActionM $ sampleThunk (unearly <$> d)
      return (res, dep_e)
    Nothing -> do
      let check (PatchMap m) = M.member fp m
      let dep_e = (MissingTrigger fp <$! ffilter check (updatedMap m))
      liftIO $ updateMap m [(D.Some sel, fp)]
      return (Nothing, dep_e)

useNoTrack :: (Reflex t, MonadIO m, MonadSample t m) =>  RuleType a -> NormalizedFilePath -> BasicM t m (Maybe a)
useNoTrack sel fp = noTrack (use_ sel fp)

noTrack :: (Reflex t, Monad m) => ActionM t m a -> BasicM t m (Maybe a)
noTrack act = ReaderT $ (\renv -> do
                      let x = fst <$> runEvents (runMaybeT (runReaderT act renv))
                      x)

liftBasic :: Monad m => BasicM t m a -> ActionM t m a
liftBasic act = do
  renv <- ask
  liftActionM (runReaderT act renv)

useNoFile_ :: (Reflex t, MonadSample t m) => GlobalType a
           -> ActionM t m a
useNoFile_ sel = useNoFile sel

useNoFile :: (Reflex t, MonadSample t m) => GlobalType a
          -> ActionM t m a

useNoFile sel = do
  m <- globalEnv <$> askGlobal
  liftActionM $ sample (current $ dyn $ D.findWithDefault (error $ "MISSING RULE:" ++ gshow sel) sel m)

useNoFileBasic :: (Reflex t, MonadSample t m)
          => GlobalType a
          -> BasicM t m a

useNoFileBasic sel = do
  v <- getGlobalVar sel
  lift $ sample (current (dyn v))

getHandlerEvent :: MonadReader (REnv t) f =>
                         (GlobalHandlers t -> b) -> f b
getHandlerEvent sel = do
  sel . handlers <$> askGlobal

getInitEvent :: MonadReader (REnv t) f => f (Event t InitParams)
getInitEvent = do
  init_e <$> askGlobal


-- Don't turn on an event until the init event has happened
waitInit :: (MonadReader (REnv t) m, Reflex t, MonadHold t m)
          => Event t a -> m (Event t a)
waitInit e = do
  init_e <- getInitEvent
  switchHold never (e <$ init_e)

getGlobalVar :: Monad m => GlobalType a -> BasicM t m (GlobalVar t a)
getGlobalVar sel = do
  m <- globalEnv <$> askGlobal
  return $ D.findWithDefault (error $ "MISSING RULE:" ++ gshow sel) sel m


withNotification :: Reflex t => Event t (LSP.NotificationMessage m a) -> Event t a
withNotification = fmap (\(LSP.NotificationMessage _ _ a) -> a)

performEventB :: PerformEvent t m => Event t (BasicM t (Performable m) a) -> BasicM t m (Event t a)
performEventB b = do
  renv <- ask
  let run = flip runReaderT renv <$> b
  lift $ performEvent run


performEventB_ :: PerformEvent t m => Event t (BasicM t (Performable m) ()) -> BasicM t m ()
performEventB_ b = do
  renv <- ask
  let run = flip runReaderT renv <$> b
  lift $ performEvent_ run

withResponse :: (PerformEvent t m, TriggerEvent t m,
                       MonadIO (Performable m), MonadHold t m,
                       MonadSample t (Performable m), MonadFix m) =>
                Maybe (NominalDiffTime, resp)
             -> (LSP.ResponseMessage resp -> LSP.FromServerMessage)
             -> Event t (LSP.RequestMessage cm params resp)
             -> (params -> ActionM t (Performable m) (Either LSP.ResponseError resp))
             -> BasicM t m ()
withResponse timeout wrap input f = mdo
  timer <- case timeout of
             -- Wait this time period
             Just (i, r) -> mkTimeout i r input
             -- Wait forever
             Nothing -> return never
  -- Run the action once, to either get the result or the dependencies we
  -- need to wait for.
  e1 <- add_dep_trigger <$> performEventB (go <$> input)

  -- A dynamic which holds the most recent request
  d  <- holdDyn Nothing (Just <$> e1)
  -- An event which fires when the dependency which failed for the most
  -- recent request failed.
  e2 <- switchHoldPromptly never
          (leftmost [(getDeps <$> (fmapMaybe id (updated d))), never <$ e4])
  -- When e2 fires, run the event again, the dependencies are now updated.
  -- It may fail again, so perhaps should combine the dependencies with e2
  -- recursively.
  e3 <- add_dep_trigger <$> performEventB (go <$> e2)
  -- Now race e1 and e3 until one returns Just
  -- When e4 fires we reset e2 so we don't send duplicate responses.
  -- I don't think resetting d is necessary.
  let e4 = leftmost (map (fmapMaybe check) [e1, e3])
  -- And finally send the output for this event
  performEventB_ (output <$> (leftmost [formatOutput <$> e4, timer]))

  where
    add_dep_trigger =  pushAlways (\(a, d, i) -> do
                              d' <- mkDepTrigger d
                              return (a, i <$ d', i)
                             )
    check :: (Maybe a, b, c) -> Maybe (c, a)
    check (m, _, i) = (i,) <$> m
    getDeps (_, deps, _) = deps
--    go :: LSP.RequestMessage cm params resp -> BasicM t m LSP.FromServerMessage
    go i@(LSP.RequestMessage _t _tid _ p) = do
      renv <- ask
      (mr, deps) <- lift $ runActionM (f p) renv
      return (mr, deps, i)

    mkFailure err (LSP.RequestMessage t tid _ _p) =
      LSP.ResponseMessage t (LSP.responseId tid) Nothing (Just err)

    mkSuccess r (LSP.RequestMessage t tid _ _p) =
      LSP.ResponseMessage t (LSP.responseId tid) (Just r) Nothing

    formatOutput (req, r) =
      case r of
          Left err -> mkFailure err req
          Right r -> mkSuccess r req

    output msg = do
      e <- askEventer
      liftIO . e . wrap $ msg

    -- When the event fires, map the failure case and delay by the timeout
    mkTimeout interval r e = do
--      let err = LSP.ResponseError LSP.UnknownErrorCode "Timeout" Nothing
      delay interval (mkSuccess r <$> e)


-- Given a list of dependency events, create an event which fires when
-- * Any of the trigger events fire
-- * All of the blocking events have fired.
mkDepTrigger :: (Reflex t, MonadHold t m, MonadFix m)
              => [DependencyType (Event t [EType])] -> m (Event t [EType])
mkDepTrigger ds = do
  let (blocks, trigs) = partitionDepTypes ds
  -- Make the dynamic which only fires when all the blocking events have
  -- fired
  case blocks of
    -- The fast case, otherwise we end up making a dynamic which never
    -- updates.
    [] -> return $ leftmost trigs
    _ -> do
      block_e <- waitEvents blocks

      -- Create a behaviour which indicates when the blocking is finished to
      -- use with gate
      -- Not sure why using this with gate doesn't work, perhaps use
      -- ffilter instead?
      --block_b <- hold False (True <$ block_e)

      -- Trigger recompile, once all the blocks have updated if either blocking
      -- or trigger events fire.
      return $ (leftmost ((concat <$> block_e) : trigs))


-- Make an event which waits for all the events to fire before firing
waitEvents :: (Reflex t, MonadHold t m, MonadFix m)
            => [Event t a] -> m (Event t [a])
waitEvents es = do
  let num = length es
  -- Use headE so if an event fires multiple times it is not counted
  -- multiple times
  hs <- mapM headE es
  -- Make a dynamic which counts and accumulates occurences
  d <- --traceDyn ("waiting for" ++ show num) <$>
        foldDyn (\as (n, res) -> (length as + n, as ++ res)) (0, [])
          (mergeWithCheap' (:[]) (++) hs)
  -- Now gate the Dynamic to only fire when n = num
  -- Also apply headE for good measure as we know it will only fire once
  let gate (n, as) | n == num = Just as
                   | otherwise = Nothing
  headE $ fmapMaybe gate (updated d)

--performAction :: REnv t -> Performable m a -> Event t () -> m (Event t (Maybe a, [
performAction :: (Reflex t1, PerformEvent t2 m) =>
                       r1
                       -> ReaderT r1 (MaybeT (EventWriterT t1 f r2 (Performable m))) a
                       -> Event t2 b
                       -> m (Event t2 (Maybe a, [f (Event t1 r2)]))
performAction renv act act_trig =
    performEvent eActions
     where eActions = act' <$ act_trig
           act' = (runEvents (runMaybeT (flip runReaderT renv act)))




whenUriFile :: Uri -> r -> (NormalizedFilePath -> r) ->  r
whenUriFile uri def act = maybe def (act . toNormalizedFilePath) (LSP.uriToFilePath uri)



(<$!) :: Functor f => b -> f t -> f b
(<$!) v fa = fmap (\a -> a `seq` v) fa

getIdeOptions :: (Reflex t, MonadSample t m) =>
                       ActionM t m IdeOptions
getIdeOptions = useNoFile_ GetIdeOptions

getSession :: (Reflex t, MonadSample t m) =>
                    ActionM t m HscEnv
getSession = useNoFile_ GetEnv

askModuleMap :: MonadReader (REnv t) m =>
                      m (ModuleMapWithUpdater t)
askModuleMap = asks (module_map)

askGlobal :: MonadReader (REnv t) m => m (GlobalEnv t)
askGlobal = asks global

askEventer :: (Reflex t, MonadSample t m) =>
                    ReaderT (REnv t) m (LSP.FromServerMessage -> IO ())
askEventer = do
  (_, _, _, eventer) <- useNoFileBasic GetInitFuncs
  return eventer

doesExist :: MonadIO m => NormalizedFilePath -> m Bool
doesExist nfp = let fp = fromNormalizedFilePath nfp in liftIO $ doesFileExist fp




{- Various Wrappers -}

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
  newTriggerEventWithOnComplete = BasicGuestWrapper newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete f = BasicGuestWrapper (newEventWithLazyTriggerWithOnComplete f)


instance (Monad (HostFrame t), Reflex t) => PerformEvent t (BasicGuestWrapper t) where
    type Performable (BasicGuestWrapper t) = HostFrame t
    performEvent m = BasicGuestWrapper (performEvent m)
    performEvent_ m = BasicGuestWrapper (performEvent_ m)
