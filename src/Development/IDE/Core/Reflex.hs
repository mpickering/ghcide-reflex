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
module Development.IDE.Core.Reflex(module Development.IDE.Core.Reflex, HostFrame) where

import Control.Monad.Fix
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

import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as LSP
import qualified Language.Haskell.LSP.Types.Capabilities as LSP
import Development.IDE.Types.Logger
import Development.IDE.Core.Debouncer

data IdeState = IdeState

data Priority = Priority Int
setPriority a = return a

type IdeResult v = ([FileDiagnostic], Maybe v)
data HoverMap = HoverMap
type Diagnostics = String
type RMap t a = M.Map NormalizedFilePath (Dynamic t a)


-- Like a Maybe, but has to be activated the first time we try to access it
data Thunk a = Value a | Awaiting | Seed (IO ())

splitThunk :: a -> Thunk (a, Maybe b)  -> (a, Thunk b)
splitThunk _ (Value (l, a)) = (l, maybe Awaiting Value a)
splitThunk a (Awaiting)  = (a, Awaiting)
splitThunk a (Seed trig) = (a, Seed trig)

thunk :: _ => Event t (Maybe a) -> m (Dynamic t (Thunk a), Event t ())
thunk e  = do
  (start, grow) <- newTriggerEvent
  start' <- batchOccurrences 0.01 start
  let trig = print "growing" >> grow Awaiting
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
    upd (Value a) _ = Just (Value a)
    -- Wait, once the trigger is pressed
    upd Awaiting  (Seed {}) = Just Awaiting
    -- Allows the thunk to be reset to GC
--    upd s@(Seed {}) _ = Just s
    -- NOPE
    upd _ _ = Nothing

newtype MDynamic t a = MDynamic { getMD :: Dynamic t (Thunk a) }

-- All the stuff in here is state which depends on the specific module
data ModuleState t = ModuleState { rules :: DMap RuleType (MDynamic t) }

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
                             }


type ModuleMap t = (Dynamic t (M.Map NormalizedFilePath (ModuleState t)))

data ModuleMapWithUpdater t =
  MMU {
    getMap :: ModuleMap t
    , updateMap :: [(D.Some RuleType, NormalizedFilePath)] -> IO ()
    }

currentMap = current . getMap
updatedMap = updated . getMap


modules = map toNormalizedFilePath ["A.hs", "B.hs"]

singleton x = [x]

mkModuleMap :: forall t m . (Adjustable t m
                            , Reflex t
                            , PerformEvent t m
                            , Monad m
                            , TriggerEvent t m
                            , MonadIO m
                            , MonadFix m
                            , MonadIO (Performable m)
                            , MonadHold t m
                            ) => GlobalEnv t
                 -> [ShakeDefinition (Performable m) t]
                 -> Event t NormalizedFilePath
                 -> m (ModuleMapWithUpdater t)
mkModuleMap genv  rules_raw input = mdo

  -- An event which is triggered to update the incremental
  (map_update, update_trigger) <- newTriggerEvent
  map_update' <- fmap concat <$> batchOccurrences 0.01 map_update
  let mk_patch ((fp, v), _) = v
      mkM (sel, fp) = (fp, Just (mkModule genv rules_raw mmu sel fp))
      mk_module fp act _ = mk_patch <$> act
      sing fp = [(D.Some GetTypecheckedModule, fp)]
      inp = M.fromList . map mkM <$> mergeWith (++) [(sing <$> input), map_update']
--  let input_event = (fmap mk_patch . mkModule o e mod_map <$> input)

  mod_map <- listWithKeyShallowDiff M.empty inp mk_module  --(mergeWith (<>) [input_event, map_update])
  let mmu = (MMU mod_map update_trigger)
  return mmu

type Action t m a = NormalizedFilePath -> ActionM t m (IdeResult a)

type CAction t m a = NormalizedFilePath -> ActionM t m (Maybe BS.ByteString, IdeResult a)

type ActionM t m a = (ReaderT (REnv t) (MaybeT (EventWriterT t () m)) a)

type DynamicM t m a = (ReaderT (REnv t) m (Dynamic t a))

type BasicM t m a = (ReaderT (REnv t) m a)

liftActionM :: Monad m => m a -> ActionM t m a
liftActionM = lift . lift . lift

lr1 :: Monad m => ActionM t m (Maybe a) -> ActionM t m a
lr1 ac = do
  r <- ac
  lift $ MaybeT (return r)



data REnv t = REnv { global :: GlobalEnv t
                   , module_map :: ModuleMapWithUpdater t
                   }

data ForallAction a where
  ForallAction :: (forall t . C t => ActionM t (HostFrame t) a) -> ForallAction a

data ForallDynamic a where
  ForallDynamic :: (forall t . C t => DynamicM t (BasicGuestWrapper t) a) -> ForallDynamic a

data ForallBasic a where
  ForallBasic :: (forall t . C t => BasicM t (BasicGuestWrapper t) a) -> ForallBasic a

newtype WrappedAction m t a =
          WrappedAction { getAction :: Action t m a }

newtype WrappedActionM m t a =
          WrappedActionM { getActionM :: ActionM t m a }

newtype WrappedDynamicM m t a =
          WrappedDynamicM { getDynamicM :: DynamicM t m a }

type Definition rt f t  = D.DSum rt (f t)

newtype BasicGuestWrapper t a =
          BasicGuestWrapper { unwrapBG :: (forall m . BasicGuestConstraints t m => BasicGuest t m a) }

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



-- Per module rules are only allowed to sample dynamics, so can be
-- triggered by external events but the external events also trigger
-- some global state to be updated.
type ShakeDefinition m t = Definition RuleType (WrappedAction m) t

-- Global definitions build dynamics directly as they need to populate
-- the global state
type GlobalDefinition m t = Definition GlobalType (WrappedDynamicM m) t

data Rule t = ModRule (ShakeDefinition (HostFrame t) t) | GlobalRule (GlobalDefinition (BasicGuestWrapper t) t)

type C t = (Monad (HostFrame t), MonadIO (HostFrame t), Ref (HostFrame t) ~ Ref IO
           , MonadRef (HostFrame t), Reflex t, MonadSample t (HostFrame t))

data WRule where
  WRule :: (forall t . C t => Rule t) -> WRule

type Rules = [WRule]

partitionRules :: C t => Rules -> ([ShakeDefinition (HostFrame t) t], [GlobalDefinition (BasicGuestWrapper t) t])
partitionRules [] = ([], [])
partitionRules (WRule (ModRule r) : xs) = let (rs, gs) = partitionRules xs in  (r:rs, gs)
partitionRules (WRule (GlobalRule g) : xs) = let (rs, gs) = partitionRules xs in  (rs, g:gs)

define :: RuleType a -> (forall t . C t => Action t (HostFrame t) a) -> WRule
define rn a = WRule (ModRule (rn :=> WrappedAction a))

-- TODO: Doesn't implement early cutoff, need to generalise ModRule
defineEarlyCutoff :: RuleType a -> (forall t . C t => CAction t (HostFrame t) a) -> WRule
defineEarlyCutoff rn a = WRule (ModRule (rn :=> WrappedAction (fmap (fmap snd) a)))

-- Like define but doesn't depend on the filepath, usually depends on the
-- global input events somehow
addIdeGlobal :: GlobalType a -> (forall t . C t => DynamicM t (BasicGuestWrapper t) a) -> WRule
addIdeGlobal gn a = WRule (GlobalRule (gn :=> WrappedDynamicM a))

mkModule :: forall t m . (MonadIO m, PerformEvent t m, TriggerEvent t m, Monad m, Reflex t, MonadFix m, _) =>
                 GlobalEnv t
              -> [ShakeDefinition (Performable m) t]
              -> ModuleMapWithUpdater t
              -> D.Some RuleType
              -> NormalizedFilePath
              -> m ((NormalizedFilePath, ModuleState t), Dynamic t [FileDiagnostic])
mkModule genv rules_raw mm (D.Some sel) f = runDynamicWriterT $ do
  -- List of all rules in the application

  rule_dyns <- mapM rule rules_raw
  let rule_dyns_map = D.fromList rule_dyns
  -- Run all the rules once to get them going
  case D.lookup sel rule_dyns_map of
    Just (MDynamic d) -> forceThunk d
    Nothing -> return ()

--  let diags = distributeListOverDyn [pm_diags]
  let m = ModuleState { rules = D.fromList rule_dyns }
  return (f, m)

  where
    rule (name :=> (WrappedAction act)) = mdo
        act_trig <- switchHoldPromptly trigger (fmap (\e -> leftmost [trigger, e]) deps)
        pm <- performEvent (traceAction ident (runEventWriterT (runMaybeT (flip runReaderT renv (act f)))) <$! act_trig)
        let (act_res, deps) = splitE pm
        (act_des_d, trigger) <- thunk act_res
        let d = splitThunk [] <$> act_des_d
        let (pm_diags, res) = splitDynPure d
        tellDyn pm_diags
        let ident = show f ++ ": " ++ gshow name
        return (name :=> (MDynamic $ traceDynE ("D:" ++ ident) res))
--        return (name :=> MDynamic res')




    renv = REnv genv mm


traceAction ident a = a
traceAction ident a = do
  liftIO $ traceEventIO ("START:" ++ ident)
  r <- a
  liftIO $ traceEventIO ("END:" ++ ident)
  return r

traceDynE p d = traceDynWith (const $ Debug.Trace.traceEvent p p) d

{-
--test :: Dynamic t IdeOptions -> Dynamic t HscEnv -> [NormalizedFilePath] -> Event t NormalizedFilePath -> BasicGuest t m (ModuleMap t, Dynamic t [FileDiagnostic])
--test o e m i = _ $ mkModuleMap o e m i
--
-- Set the GHC libdir to the nix libdir if it's present.
getLibdir :: IO FilePath
getLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"


cradleToSession :: Cradle -> IO HscEnv
cradleToSession cradle = do
    cradleRes <- getCompilerOptions "" cradle
    ComponentOptions opts _deps <- case cradleRes of
        CradleSuccess r -> pure r
        CradleFail err -> error (show err)
        -- TODO Rather than failing here, we should ignore any files that use this cradle.
        -- That will require some more changes.
        CradleNone -> fail "'none' cradle is not yet supported"
    libdir <- getLibdir
    env <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        -- Perhaps need to enable -fignore-interface-pragmas to not
        -- recompie due to changes to unfoldings and so on
        (dflags', _targets) <- addCmdOpts opts dflags
        _ <- setSessionDynFlags dflags'
        GHC.getSession

    initDynLinker env
    return env


loadSession :: FilePath -> FilePath -> IO HscEnv
loadSession dir v = do
    cradleLoc <- findCradle v
    c <- loadCradle v
    cradleToSession c
    -}

instance TraversableB HandlersG where
instance ApplicativeB HandlersG where
instance FunctorB HandlersG where

type GlobalHandlers t = HandlersG (Event t)

-- Make the handlers and events which fire when a handler fires
mkHandlers :: forall m t . _ => m (Handlers, GlobalHandlers t)
mkHandlers = do
  res <- btraverse go h
  return $ bunzip res
  where
    h :: Handlers
    h = def

    go :: f a -> m ((MHandler `Product` (Event t)) a)
    go _ = do
      (input, trig) <- newTriggerEvent
      return (Pair (MJust trig) (input))

reflexOpen :: Logger
           -> Debouncer LSP.NormalizedUri
           -> IdeOptions
           -> (Handlers ->
                ((VFSHandle, LSP.ClientCapabilities, IO LSP.LspId, (LSP.FromServerMessage -> IO ())) -> IO ())
              -> (NormalizedFilePath -> IO ())
              -> ForallBasic ())
           -> Rules
           -> IO ()
reflexOpen logger debouncer opts startServer init_rules = do
--  session <- loadSession "/home/matt/reflex-ghc/ghcide/" "/home/matt/reflex-ghc/ghcide/hie.yaml"
--  setCurrentDirectory "/home/matt/reflex-ghc/ghcide"


  basicHostWithQuit $ mdo
    let (mod_rules, global_rules) = partitionRules init_rules
    pb <- getPostBuild
    (input, input_trigger) <- newTriggerEvent
--    (sample, sample_trigger) <- newTriggerEvent

    -- TODO: Deduplicate, global rules need to be MDynamic as well?
    -- Doesn't typecheck if you have the signature (haha)
    let --rule :: Event t () -> GlobalDefinition (Performable m) t -> m (DSum GlobalType (Dynamic t))
        rule trigger (name :=> (WrappedDynamicM act)) = mdo
          {-
          act_trig <- switchHoldPromptly trigger (fmap (\e -> leftmost [trigger, e]) deps)
          pm <- performEvent (traceAction ident (runEventWriterT (runMaybeT (flip runReaderT renv act))) <$! act_trig)
          -}
          (upd_e, upd) <- newTriggerEvent
          d <- unwrapBG (flip runReaderT renv act)
          cur <- sample (current d)
          let go (This a) = Just (a cur)
              go (That b) = Just b
              go (These a b) = Just (a b)
          d' <- holdDyn cur (alignEventWithMaybe go  upd_e  (updated d))
          --let (act_res, deps) = splitE pm
          --d <- holdDyn Nothing act_res
          --d' <- improvingMaybe d
          --let ident = gshow name
--        return (traceDynE ("D:" ++ ident) res')
          return (name :=> GlobalVar d' upd)

    let renv = REnv genv mmap

    genv_rules <- mapM (rule pb) global_rules

    let genv = GlobalEnv (D.fromList genv_rules) hs_es init



    mmap <- mkModuleMap genv mod_rules input
--    (mmap, diags2) <- test opts env modules input
    let diags = M.empty
        hover = M.empty
        --parsedModules = holdDyn M.empty
        dependencyInformation = M.empty
        typecheckedModules = M.empty

    performEvent_ $ liftIO . print <$> input

    (init, init_trigger) <- newTriggerEvent
    (hs, hs_es) <- mkHandlers

    let ForallBasic start = startServer hs init_trigger input_trigger
    unwrapBG $ flip runReaderT renv start

    liftIO $ forkIO $ do
      input_trigger (toNormalizedFilePath "src/Development/IDE/Core/Rules.hs")
      threadDelay 1000000
--      input_trigger (toNormalizedFilePath "B.hs")
      threadDelay 1000000
      showProfilingData
      threadDelay 1000000000
      liftIO $ input_trigger (toNormalizedFilePath "def")

    --performEvent_ $ liftIO . print <$> (updated diags2)
    return never
{-
-- Typechecks a module.
typeCheckRule :: _ => ShakeDefinition m t
typeCheckRule = define GetTypecheckedModule $ \file -> do
  pm <- use_ GetParsedModule file
  deps <- use_ GetDependencies file
  packageState <- getSession
        -- Figure out whether we need TemplateHaskell or QuasiQuotes support
  --let graph_needs_th_qq = needsTemplateHaskellOrQQ $ hsc_mod_graph packageState
  --    file_uses_th_qq = uses_th_qq $ ms_hspp_opts (pm_mod_summary pm)
  --    any_uses_th_qq = graph_needs_th_qq || file_uses_th_qq
      {-
  tms <- if any_uses_th_qq || False
            -- If we use TH or QQ, we must obtain the bytecode
            then do
              --bytecodes <- uses_ GenerateByteCode (transitiveModuleDeps deps)
              --tmrs <- uses_ TypeCheck (transitiveModuleDeps deps)
              --pure (zipWith addByteCode bytecodes tmrs)
            else  -}
  tms <- uses_ GetTypecheckedModule (transitiveModuleDeps deps)
  --setPriority priorityTypeCheck
  IdeOptions{ optDefer = defer} <- getIdeOptions
  liftIO $ print ("typechecking", file)
  liftIO $ typecheckModule defer packageState tms pm
    where
        uses_th_qq dflags = xopt LangExt.TemplateHaskell dflags || xopt LangExt.QuasiQuotes dflags
        addByteCode :: Linkable -> TcModuleResult -> TcModuleResult
        addByteCode lm tmr = tmr { tmrModInfo = (tmrModInfo tmr) { hm_linkable = Just lm } }



getDependencyInformationRule :: _ => ShakeDefinition m t
getDependencyInformationRule = define GetDependencyInfo $ \file -> do
  (ds,rawDepInfo) <- rawDependencyInformation file
  return $ case rawDepInfo of
    Just rawDepInfo -> ([], Just $  processDependencyInformation rawDepInfo)
    Nothing -> (ds, Nothing)

rawDependencyInformation :: _ => Action t m RawDependencyInformation
rawDependencyInformation f = do
    let (initialId, initialMap) = getPathId f emptyPathIdMap
    res <- go (IntSet.singleton $ getFilePathId initialId)
       (RawDependencyInformation IntMap.empty initialMap)
    return ([], Just res)
  where
    go fs rawDepInfo =
        case IntSet.minView fs of
            -- Queue is empty
            Nothing -> pure rawDepInfo
            -- Pop f from the queue and process it
            Just (f, fs) -> do
                let fId = FilePathId f
                importsOrErr <- use GetLocatedImports (idToPath (rawPathIdMap rawDepInfo) fId)
                case importsOrErr of
                  Nothing ->

                    -- File doesn’t parse
                    let rawDepInfo' = insertImport fId (Left ModuleParseError) rawDepInfo
                    in do
                      go fs rawDepInfo'
                  Just (modImports, pkgImports) -> do
                    let f :: PathIdMap -> (a, Maybe NormalizedFilePath) -> (PathIdMap, (a, Maybe FilePathId))
                        f pathMap (imp, mbPath) = case mbPath of
                            Nothing -> (pathMap, (imp, Nothing))
                            Just path ->
                                let (pathId, pathMap') = getPathId path pathMap
                                in (pathMap', (imp, Just pathId))
                    -- Convert paths in imports to ids and update the path map
                    let (pathIdMap, modImports') = mapAccumL f (rawPathIdMap rawDepInfo) modImports
                    -- Files that we haven’t seen before are added to the queue.
                    let newFiles =
                            IntSet.fromList (coerce $ Data.Maybe.mapMaybe snd modImports')
                            IntSet.\\ IntMap.keysSet (rawImports rawDepInfo)
                    let rawDepInfo' = insertImport fId (Right $ ModuleImports modImports' pkgImports) rawDepInfo
                    go (newFiles `IntSet.union` fs) (rawDepInfo' { rawPathIdMap = pathIdMap })

-- returns all transitive dependencies in topological order.
-- NOTE: result does not include the argument file.
getDependenciesRule :: _ => ShakeDefinition m t
getDependenciesRule = define GetDependencies $ \fp -> do
        depInfo@DependencyInformation{..} <- use_ GetDependencyInfo fp
        return ([], transitiveDeps depInfo fp)

-- Source SpanInfo is used by AtPoint and Goto Definition.
getSpanInfoRule :: _ => ShakeDefinition m t
getSpanInfoRule = define GetSpanInfo $ \fp -> do
  tc <- use_ GetTypecheckedModule fp
  deps <- maybe (TransitiveDependencies [] []) id <$> use GetDependencies fp
  tms <- catMaybes <$> uses GetTypecheckedModule (transitiveModuleDeps deps)
  (fileImports, _) <- use_ GetLocatedImports fp
  packageState <- getSession
  x <- liftIO $ getSrcSpanInfos packageState fileImports tc tms
  return ([], Just x)
-}
sampleMaybe :: (Monad m
               , Reflex t
               , MonadIO m
               , MonadSample t m)
                 => RuleType a
                 -> NormalizedFilePath
                 -> ActionM t m a
sampleMaybe sel fp = do
  lr1 (use sel fp)

use_ = sampleMaybe

uses_ :: _ => RuleType a
              -> [NormalizedFilePath]
              -> ActionM t m [a]
uses_ sel fps = mapM (sampleMaybe sel) fps

uses :: _ => RuleType a
             -> [NormalizedFilePath]
             -> ActionM t m [Maybe a]
uses sel fps = mapM (use sel) fps

useWithStale = use

use :: (Monad m
       , Reflex t
       , MonadIO m
       , MonadSample t m)
         => RuleType a
         -> NormalizedFilePath
         -> ActionM t m (Maybe a)
use sel fp = do
  m <- askModuleMap
  mm <- liftActionM $ sample (currentMap m)
  case M.lookup fp mm of
    Just ms -> do
      d <- lift $ hoistMaybe (getMD <$> D.lookup sel (rules ms))
      lift $ lift $ tellEvent (() <$! updated d)
      liftActionM $ sampleThunk d
    Nothing -> do
      liftIO $ traceEventIO "FAILED TO FIND"
      lift $ lift $ tellEvent (() <$! updatedMap m)
      liftIO $ updateMap m [(D.Some sel, fp)]
      return Nothing

useNoTrack :: _ =>  RuleType a -> NormalizedFilePath -> BasicM t m (Maybe a)
useNoTrack sel fp = noTrack (use_ sel fp)


noTrack :: _ => ActionM t m a -> BasicM t m (Maybe a)
noTrack act = ReaderT $ (\renv -> do
                      let x = fst <$> runEventWriterT (runMaybeT (runReaderT act renv))
                      x)

useNoFile_ :: _ => GlobalType a
           -> ActionM t m a
useNoFile_ sel = useNoFile sel

useNoFile :: _ => GlobalType a
          -> ActionM t m a

useNoFile sel = do
  m <- globalEnv <$> askGlobal
  liftActionM $ sample (current $ dyn $ D.findWithDefault (error $ "MISSING RULE:" ++ gshow sel) sel m)

getHandlerEvent sel = do
  sel . handlers <$> askGlobal

getInitEvent = do
  init_e <$> askGlobal

withNotification :: _ => Event t (LSP.NotificationMessage m a) -> Event t a
withNotification = fmap (\(LSP.NotificationMessage _ _ a) -> a)

whenUriFile :: Uri -> r -> (NormalizedFilePath -> r) ->  r
whenUriFile uri def act = maybe def (act . toNormalizedFilePath) (LSP.uriToFilePath uri)



(<$!) v fa = fmap (\a -> a `seq` v) fa


getIdeOptions = useNoFile_ GetIdeOptions
getSession = useNoFile_ GetEnv

askModuleMap = asks (module_map)
askGlobal = asks global

{-
getLocatedImportsRule :: _ => ShakeDefinition m t
getLocatedImportsRule = define GetLocatedImports $ \file -> do
            pm <- use_  GetParsedModule file
            env <- getSession
            opt <- getIdeOptions
            let ms = pm_mod_summary pm
            let imports = [(False, imp) | imp <- ms_textual_imps ms] ++ [(True, imp) | imp <- ms_srcimps ms]
            let dflags = addRelativeImport file pm $ hsc_dflags env
            (diags, imports') <- lift $ fmap unzip $ forM imports $ \(isSource, (mbPkgName, modName)) -> do
                diagOrImp <- locateModule dflags (optExtensions opt) doesExist modName mbPkgName isSource
                case diagOrImp of
                    Left diags -> pure (diags, Left (modName, Nothing))
                    Right (FileImport path) -> pure ([], Left (modName, Just $ path))
                    Right (PackageImport pkgId) -> liftIO $ do
                        diagsOrPkgDeps <- computePackageDeps env pkgId
                        case diagsOrPkgDeps of
                          Left diags -> pure (diags, Right Nothing)
                          Right pkgIds -> pure ([], Right $ Just $ pkgId : pkgIds)
            let (moduleImports, pkgImports) = partitionEithers imports'
            case sequence pkgImports of
                Nothing -> pure (concat diags, Nothing)
                Just pkgImports -> pure (concat diags, Just (moduleImports, Set.fromList $ concat pkgImports))
                -}

doesExist nfp = let fp = fromNormalizedFilePath nfp in liftIO $ doesFileExist fp

-- When a new FilePath arrives we need to
-- 1. Parse the module
-- 2. Get the dependenices of the module
-- 3. Compile the dependendencies
-- 4. Compile the module
-- 5. Construct the hover map from the module
--
-- During these steps, the network should be constructed so that if
-- a new file modification event comes in, it only triggers recompilation
-- to the part of the network which may have changed.
{-
getParsedModuleRule :: _ => ShakeDefinition m t
getParsedModuleRule = define GetParsedModule $ \fp -> do
    contents <- liftIO $ stringToStringBuffer <$> (readFile (fromNormalizedFilePath fp))
    packageState <- getSession
    opt <- getIdeOptions
    (diag, res) <- liftIO $ parseModule opt packageState (fromNormalizedFilePath fp) (Just contents)
    case res of
        Nothing -> pure (diag, Nothing)
        Just (contents, modu) -> do
            pure (diag, Just modu)
            -}

