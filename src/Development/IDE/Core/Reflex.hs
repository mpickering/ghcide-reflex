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
module Development.IDE.Core.Reflex(module Development.IDE.Core.Reflex, HostFrame) where

import Language.Haskell.LSP.Core
import Data.Kind
import Reflex.Host.Class
import qualified Data.ByteString.Char8 as BS
import Development.IDE.Core.RuleTypes
import Control.Monad.Reader
import Data.GADT.Show
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Control.Error.Util
import qualified Data.Dependent.Map as D
import Data.Dependent.Map (DMap, DSum(..))
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
setPriority = undefined

type IdeResult v = ([FileDiagnostic], Maybe v)
data HoverMap = HoverMap
type Diagnostics = String
type RMap t a = M.Map NormalizedFilePath (Dynamic t a)

{-
data State t = State { input :: Event t FilePath -- ^ Files which are being modified
                     , fileInputs :: Dynamic t [FilePath] -- List of current files of interest
                     , diags :: RMap t [Diagnostics] -- ^ The current diagnostics for each path
                     , hover :: RMap t HoverMap -- ^ The current hover map for each path.
                     , parsedModules :: Dynami(HostFrame
                     , dependencyInformation :: RMap t ()
                     , typecheckedModules :: RMap t TypecheckedModule
                     }
                     -}






newtype MDynamic t a = MDynamic { getMD :: Dynamic t (Maybe a) }

data ModuleState t = ModuleState { rules :: DMap RuleType (MDynamic t) }

{-
data ModuleState t = ModuleState { fileChanged :: Event t NormalizedFilePath
                               , getParsedModule :: Dynamic t (Maybe ParsedModule)
                               , getLocatedImports :: Dynamic t (Maybe LocatedImports)
                               , getSpanInfo :: Dynamic t (Maybe SpansInfo)
                               , getDependencyInformation :: Dynamic t (Maybe DependencyInformation)
                               , getDependencies :: Dynamic t (Maybe TransitiveDependencies)
                               , getTypecheckedModule :: Dynamic t (Maybe TcModuleResult)
                               }
                               -}

data GlobalEnv t = GlobalEnv { globalEnv :: D.DMap GlobalType (Dynamic t) }

type ModuleMap t = (Dynamic t (M.Map NormalizedFilePath (ModuleState t)))

data ModuleMapWithUpdater t =
  MMU {
    getMap :: ModuleMap t
    , updateMap :: [NormalizedFilePath] -> IO ()
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
      mkM fp = (fp, Just (mkModule genv rules_raw mmu fp))
      mk_module fp act _ = mk_patch <$> act
      inp = M.fromList . map mkM <$> mergeWith (++) [(singleton <$> input), map_update']
--  let input_event = (fmap mk_patch . mkModule o e mod_map <$> input)

  mod_map <- listWithKeyShallowDiff M.empty inp mk_module  --(mergeWith (<>) [input_event, map_update])
  let mmu = (MMU mod_map update_trigger)
  return mmu

type Action t m a = NormalizedFilePath -> ActionM t m (IdeResult a)

type CAction t m a = NormalizedFilePath -> ActionM t m (Maybe BS.ByteString, IdeResult a)

type ActionM t m a = (ReaderT (REnv t) (MaybeT (EventWriterT t () m)) a)

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

newtype WrappedAction m t a =
          WrappedAction { getAction :: Action t m a }

newtype WrappedActionM m t a =
          WrappedActionM { getActionM :: ActionM t m a }

type Definition rt f t  = D.DSum rt (f t)

type ShakeDefinition m t = Definition RuleType (WrappedAction m) t

type GlobalDefinition m t = Definition GlobalType (WrappedActionM m) t

data Rule m t = ModRule (ShakeDefinition m t) | GlobalRule (GlobalDefinition m t)

type C t = (Monad (HostFrame t), MonadIO (HostFrame t), Ref (HostFrame t) ~ Ref IO
           , MonadRef (HostFrame t), Reflex t, MonadSample t (HostFrame t))

data WRule where
  WRule :: (forall t . C t => Rule (HostFrame t) t) -> WRule

type Rules = [WRule]

partitionRules :: C t => Rules -> ([ShakeDefinition (HostFrame t) t], [GlobalDefinition (HostFrame t) t])
partitionRules [] = ([], [])
partitionRules (WRule (ModRule r) : xs) = let (rs, gs) = partitionRules xs in  (r:rs, gs)
partitionRules (WRule (GlobalRule g) : xs) = let (rs, gs) = partitionRules xs in  (rs, g:gs)

define :: RuleType a -> (forall t . C t => Action t (HostFrame t) a) -> WRule
define rn a = WRule (ModRule (rn :=> WrappedAction a))

defineEarlyCutoff :: RuleType a -> (forall t . C t => CAction t (HostFrame t) a) -> WRule
defineEarlyCutoff rn a = undefined

-- Like define but doesn't depend on the filepath, usually depends on the
-- global input events somehow
addIdeGlobal :: GlobalType a -> (forall t . ActionM t (HostFrame t) a) -> WRule
addIdeGlobal gn a = WRule (GlobalRule (gn :=> WrappedActionM a))

mkModule :: forall t m . (MonadIO m, PerformEvent t m, TriggerEvent t m, Monad m, Reflex t, MonadFix m, _) =>
                 GlobalEnv t
              -> [ShakeDefinition (Performable m) t]
              -> ModuleMapWithUpdater t
              -> NormalizedFilePath
              -> m ((NormalizedFilePath, ModuleState t), Dynamic t [FileDiagnostic])
mkModule genv rules_raw mm f = runDynamicWriterT $ do
  -- List of all rules in the application

  (start, trigger) <- newTriggerEvent
  rule_dyns <- mapM (rule start) rules_raw
  -- Run all the rules once to get them going
  liftIO $ trigger ()

--  let diags = distributeListOverDyn [pm_diags]
  let m = ModuleState { rules = D.fromList rule_dyns }
  return (f, m)

  where
    rule trigger (name :=> (WrappedAction act)) = mdo
        let wrap = fromMaybe ([], Nothing)
        act_trig <- switchHoldPromptly trigger (fmap (\e -> leftmost [trigger, e]) deps)
        pm <- performEvent (traceAction ident (runEventWriterT (runMaybeT (flip runReaderT renv (act f)))) <$! act_trig)
        let (act_res, deps) = splitE pm
        d <- holdDyn ([], Nothing) (wrap <$> act_res)
        let (pm_diags, res) = splitDynPure d
        tellDyn pm_diags
        let ident = show f ++ ": " ++ gshow name
        res' <- improvingMaybe res
--        return (traceDynE ("D:" ++ ident) res')
        return (name :=> MDynamic res)

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

reflexOpen :: Logger
           -> Debouncer LSP.NormalizedUri
           -> IdeOptions
           -> (Handlers ->
                ((VFSHandle, LSP.ClientCapabilities, IO LSP.LspId, (LSP.FromServerMessage -> IO ())) -> IO ()) -> IO ())
           -> Rules
           -> IO ()
reflexOpen logger debouncer opts startServer init_rules = do
--  session <- loadSession "/home/matt/reflex-ghc/ghcide/" "/home/matt/reflex-ghc/ghcide/hie.yaml"
--  setCurrentDirectory "/home/matt/reflex-ghc/ghcide"
--

  basicHostWithQuit $ mdo
    let (mod_rules, global_rules) = partitionRules init_rules
    pb <- getPostBuild
    (input, input_trigger) <- newTriggerEvent

    fileInputs <- holdDyn [] never
--    opts :: Dynamic t IdeOptions
    --opts <- holdDyn  never

--    env :: Dynamic t HscEnv
    --env <- holdDyn session never

    -- TODO: Deduplicate
    {-
    let rule :: forall t m . Event t () -> GlobalDefinition (Performable m) t -> m (DSum GlobalType (Dynamic t))
        rule trigger (name :=> (WrappedActionM act)) = mdo
          let wrap = fromMaybe ([], Nothing)
          act_trig <- switchHoldPromptly trigger (fmap (\e -> leftmost [trigger, e]) deps)
          pm <- performEvent (traceAction ident (runEventWriterT (runMaybeT (flip runReaderT renv act))) <$! act_trig)
          let (act_res, deps) = splitE pm
          d <- holdDyn act_res
          let (pm_diags, res) = splitDynPure d
          tellDyn pm_diags
          let ident = gshow name
          res' <- improvingMaybe res
--        return (traceDynE ("D:" ++ ident) res')
          return (name :=> res)

    let renv = REnv genv mmap

    genv_rules <- mapM (rule pb) global_rules
    -}
    let genv = GlobalEnv undefined



    mmap <- mkModuleMap genv mod_rules input
--    (mmap, diags2) <- test opts env modules input
    let diags = M.empty
        hover = M.empty
        --parsedModules = holdDyn M.empty
        dependencyInformation = M.empty
        typecheckedModules = M.empty

    performEvent_ $ liftIO . print <$> input

    let hs = undefined
    liftIO $ startServer hs undefined

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
      liftActionM $ sample (current d)
    Nothing -> do
      liftIO $ traceEventIO "FAILED TO FIND"
      lift $ lift $ tellEvent (() <$! updatedMap m)
      liftIO $ updateMap m [fp]
      return Nothing

useNoFile_ :: _ => GlobalType a
          -> ActionM t m a
useNoFile_ sel = do
  m <- globalEnv <$> askGlobal
  liftActionM $ sample (current $ D.findWithDefault (error "MISSING RULE") sel m)


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

