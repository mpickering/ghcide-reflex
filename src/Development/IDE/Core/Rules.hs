-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PartialTypeSignatures       #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.Rules(
    IdeState,
    Priority(..),
    priorityTypeCheck,
    priorityGenerateCore,
    priorityFilesOfInterest,
--    runAction,
    toIdeResult,
    mainRule,
    getAtPoint,
    getDefinition,
    getDependencies,
    getParsedModule,
    generateCore,
    ) where

import Fingerprint

import Development.IDE.Core.FileStore
import Data.Binary
import Data.Bifunctor (second)
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Development.IDE.Core.Compile
import Development.IDE.Core.OfInterest
import Development.IDE.Types.Options
import Development.IDE.Spans.Calculate
import Development.IDE.Import.DependencyInformation
import Development.IDE.Import.FindImports
import           Development.IDE.Core.FileExists
import           Development.IDE.Core.FileStore        (getFileContents)
import           Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Development.IDE.GHC.Compat hiding (parseModule, typecheckModule)
import Development.IDE.GHC.Util
import Data.Coerce
import Data.Either.Extra
import Data.Maybe
import           Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import Data.List
import Data.Ord
import qualified Data.Set                                 as Set
import qualified Data.Text                                as T
import qualified Data.Text.Encoding                       as TE
import           Development.IDE.GHC.Error
--import           Development.Shake                        hiding (Diagnostic)
import Development.IDE.Core.RuleTypes
import Development.IDE.Spans.Type
import Control.Monad.IO.Class

import qualified GHC.LanguageExtensions as LangExt
import HscTypes
import DynFlags (xopt)
import GHC.Generics(Generic)

import qualified Development.IDE.Spans.AtPoint as AtPoint
--import Development.IDE.Core.Service
import Development.IDE.Core.Reflex
import Development.Shake.Classes

-- | This is useful for rules to convert rules that can only produce errors or
-- a result into the more general IdeResult type that supports producing
-- warnings while also producing a result.
toIdeResult :: Either [FileDiagnostic] v -> IdeResult v
toIdeResult = either (, Nothing) (([],) . Just)

-- | useE is useful to implement functions that aren’t rules but need shortcircuiting
-- e.g. getDefinition.
--useE :: RuleType a -> NormalizedFilePath -> MaybeT (Action v
--useE k = MaybeT . use k
{-
useNoFileE :: IdeRule k v => k -> MaybeT Action v
useNoFileE k = useE k ""

usesE :: IdeRule k v => k -> [NormalizedFilePath] -> MaybeT Action [v]
usesE k = MaybeT . fmap sequence . uses k

defineNoFile :: _ => RuleType a -> (Action t m a) -> WRule
defineNoFile f = define k $ \file -> do
    if file == "" then do res <- f k; return ([], Just res) else
        fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"
        -}


------------------------------------------------------------
-- Exposed API

-- | Get all transitive file dependencies of a given module.
-- Does not include the file itself.
getDependencies :: _ => NormalizedFilePath -> ActionM t m (Maybe [NormalizedFilePath])
getDependencies file = fmap transitiveModuleDeps <$> use GetDependencies file

-- | Try to get hover text for the name under point.
getAtPoint :: _ => NormalizedFilePath -> Position -> ActionM t m (Maybe (Maybe Range, [T.Text]))
getAtPoint file pos = fmap join $ runMaybeT $ do
  opts <- lift getIdeOptions
  spans <- MaybeT $ use GetSpanInfo file
  return $ AtPoint.atPoint opts spans pos

-- | Goto Definition.
getDefinition :: _ => NormalizedFilePath -> Position -> ActionM  t m (Maybe Location)
getDefinition file pos = fmap join $ runMaybeT $ do
    opts <- lift getIdeOptions
    spans <- MaybeT $ use GetSpanInfo file
    lift $ AtPoint.gotoDefinition (getHieFile file) opts (spansExprs spans) pos

getHieFile
  :: _ => NormalizedFilePath -- ^ file we're editing
  -> Module -- ^ module dep we want info for
  -> ActionM t m (Maybe (HieFile, FilePath)) -- ^ hie stuff for the module
getHieFile file mod = do
  TransitiveDependencies {transitiveNamedModuleDeps} <- use_ GetDependencies file
  case find (\x -> nmdModuleName x == moduleName mod) transitiveNamedModuleDeps of
    Just NamedModuleDep{nmdFilePath=nfp} -> do
        let modPath = fromNormalizedFilePath nfp
        (_diags, hieFile) <- getHomeHieFile nfp
        return $ (, modPath) <$> hieFile
    _ -> getPackageHieFile mod file


getHomeHieFile :: _ => NormalizedFilePath -> ActionM t m ([a], Maybe HieFile)
getHomeHieFile f = do
  pm <- use_ GetParsedModule f
  let normal_hie_f = toNormalizedFilePath hie_f
      hie_f = ml_hie_file $ ms_location $ pm_mod_summary pm
  mbHieTimestamp <- getModificationTimeM normal_hie_f
  srcTimestamp   <- getModificationTimeM f

  let isUpToDate
        | let d = mbHieTimestamp = comparing modificationTime d srcTimestamp == GT
        | otherwise = False

  unless isUpToDate $
       void $ use_ GetTypecheckedModule f

  hf <- liftIO $ if isUpToDate then Just <$> loadHieFile hie_f else pure Nothing
  return ([], hf)

getModificationTimeM f = do
  vfs <- useNoFile_ GetVFSHandle
  (mb, (ds, r)) <- getModificationTime vfs f
  case r of
    Just v -> return v
    Nothing -> fail  "NOTING"

getPackageHieFile :: _ => Module             -- ^ Package Module to load .hie file for
                  -> NormalizedFilePath -- ^ Path of home module importing the package module
                  -> ActionM t m (Maybe (HieFile, FilePath))
getPackageHieFile mod file = do
    pkgState  <- hscEnv <$> use_ GhcSession file
    IdeOptions {..} <- getIdeOptions
    let unitId = moduleUnitId mod
    case lookupPackageConfig unitId pkgState of
        Just pkgConfig -> do
            -- 'optLocateHieFile' returns Nothing if the file does not exist
            hieFile <- liftIO $ optLocateHieFile optPkgLocationOpts pkgConfig mod
            path    <- liftIO $ optLocateSrcFile optPkgLocationOpts pkgConfig mod
            case (hieFile, path) of
                (Just hiePath, Just modPath) ->
                    -- deliberately loaded outside the Shake graph
                    -- to avoid dependencies on non-workspace files
                        liftIO $ Just . (, modPath) <$> loadHieFile hiePath
                _ -> return Nothing
        _ -> return Nothing

-- | Parse the contents of a daml file.
getParsedModule :: _ => NormalizedFilePath -> ActionM t m (Maybe ParsedModule)
getParsedModule file = use GetParsedModule file

------------------------------------------------------------
-- Rules
-- These typically go from key to value and are oracles.

priorityTypeCheck :: Priority
priorityTypeCheck = Priority 0

priorityGenerateCore :: Priority
priorityGenerateCore = Priority (-1)

priorityFilesOfInterest :: Priority
priorityFilesOfInterest = Priority (-2)

getParsedModuleRule :: _ => WRule
getParsedModuleRule =
    defineEarlyCutoff GetParsedModule $ \file -> do
        (_, contents) <- getFileContents file
        packageState <- hscEnv <$> use_ GhcSession file
        opt <- getIdeOptions
        (diag, res) <- liftIO $ parseModule opt packageState (fromNormalizedFilePath file) (fmap textToStringBuffer contents)
        case res of
            Nothing -> pure (Nothing, (diag, Nothing))
            Just (contents, modu) -> do
                mbFingerprint <- if isNothing $ optShakeFiles opt
                    then pure Nothing
                    else liftIO $ Just . fingerprintToBS <$> fingerprintFromStringBuffer contents
                pure (mbFingerprint, (diag, Just modu))

getLocatedImportsRule :: _ => WRule
getLocatedImportsRule =
    define GetLocatedImports $ \file -> do
        pm <- use_ GetParsedModule file
        let ms = pm_mod_summary pm
        let imports = [(False, imp) | imp <- ms_textual_imps ms] ++ [(True, imp) | imp <- ms_srcimps ms]
        env <- hscEnv <$> use_ GhcSession file
        let dflags = addRelativeImport file pm $ hsc_dflags env
        opt <- getIdeOptions
        (diags, imports') <- fmap unzip $ forM imports $ \(isSource, (mbPkgName, modName)) -> do
            diagOrImp <- locateModule dflags (optExtensions opt) getFileExists modName mbPkgName isSource
            case diagOrImp of
                Left diags -> pure (diags, Left (modName, Nothing))
                Right (FileImport path) -> pure ([], Left (modName, Just path))
                Right (PackageImport pkgId) -> liftIO $ do
                    diagsOrPkgDeps <- computePackageDeps env pkgId
                    case diagsOrPkgDeps of
                        Left diags -> pure (diags, Right Nothing)
                        Right pkgIds -> pure ([], Right $ Just $ pkgId : pkgIds)
        let (moduleImports, pkgImports) = partitionEithers imports'
        case sequence pkgImports of
            Nothing -> pure (concat diags, Nothing)
            Just pkgImports -> pure (concat diags, Just (moduleImports, Set.fromList $ concat pkgImports))


-- | Given a target file path, construct the raw dependency results by following
-- imports recursively.
rawDependencyInformation :: _ => NormalizedFilePath -> ActionM t m RawDependencyInformation
rawDependencyInformation f = do
    let initialArtifact = ArtifactsLocation f (ModLocation (Just $ fromNormalizedFilePath f) "" "") False
        (initialId, initialMap) = getPathId initialArtifact emptyPathIdMap
    (rdi, ss) <- go (IntSet.singleton $ getFilePathId initialId)
                    ((RawDependencyInformation IntMap.empty initialMap IntMap.empty), IntMap.empty)
    let bm = IntMap.foldrWithKey (updateBootMap rdi) IntMap.empty ss
    return (rdi { rawBootMap = bm })
  where
    go fs (rawDepInfo, ss) =
        case IntSet.minView fs of
            -- Queue is empty
            Nothing -> pure (rawDepInfo, ss)
            -- Pop f from the queue and process it
            Just (f, fs) -> do
                let fId = FilePathId f
                importsOrErr <- use GetLocatedImports $ idToPath (rawPathIdMap rawDepInfo) fId
                case importsOrErr of
                  Nothing ->
                    -- File doesn’t parse
                    let rawDepInfo' = insertImport fId (Left ModuleParseError) rawDepInfo
                    in go fs (rawDepInfo', ss)
                  Just (modImports, pkgImports) -> do
                    let f :: (PathIdMap, IntMap ArtifactsLocation)
                          -> (a, Maybe ArtifactsLocation)
                          -> ((PathIdMap, IntMap ArtifactsLocation), (a, Maybe FilePathId))
                        f (pathMap, ss) (imp, mbPath) = case mbPath of
                            Nothing -> ((pathMap, ss), (imp, Nothing))
                            Just path ->
                                let (pathId, pathMap') = getPathId path pathMap
                                    ss' = if isBootLocation path
                                            then IntMap.insert (getFilePathId pathId) path ss
                                            else ss
                                in ((pathMap', ss'), (imp, Just pathId))
                    -- Convert paths in imports to ids and update the path map
                    let ((pathIdMap, ss'), modImports') = mapAccumL f (rawPathIdMap rawDepInfo, ss) modImports
                    -- Files that we haven’t seen before are added to the queue.
                    let newFiles =
                            IntSet.fromList (coerce $ mapMaybe snd modImports')
                            IntSet.\\ IntMap.keysSet (rawImports rawDepInfo)
                    let rawDepInfo' = insertImport fId (Right $ ModuleImports modImports' pkgImports) rawDepInfo
                    go (newFiles `IntSet.union` fs)
                       (rawDepInfo' { rawPathIdMap = pathIdMap }, ss')



    updateBootMap pm boot_mod_id ArtifactsLocation{..} bm =
      if not artifactIsSource
        then
          let msource_mod_id = lookupPathToId (rawPathIdMap pm) (toNormalizedFilePath $ dropBootSuffix artifactModLocation)
          in case msource_mod_id of
               Just source_mod_id -> insertBootId source_mod_id (FilePathId boot_mod_id) bm
               Nothing -> bm
        else bm

    dropBootSuffix :: ModLocation -> FilePath
    dropBootSuffix (ModLocation (Just hs_src) _ _) = reverse . drop (length @[] "-boot") . reverse $ hs_src
    dropBootSuffix _ = error "dropBootSuffix"

getDependencyInformationRule :: _ => WRule
getDependencyInformationRule =
    define GetDependencyInformation $ \file -> do
       rawDepInfo <- rawDependencyInformation file
       pure ([], Just $ processDependencyInformation rawDepInfo)

reportImportCyclesRule :: _ => WRule
reportImportCyclesRule =
    define ReportImportCycles $ \file -> fmap (\errs -> if null errs then ([], Just ()) else (errs, Nothing)) $ do
        DependencyInformation{..} <- use_ GetDependencyInformation file
        let fileId = pathToId depPathIdMap file
        case IntMap.lookup (getFilePathId fileId) depErrorNodes of
            Nothing -> pure []
            Just errs -> do
                let cycles = mapMaybe (cycleErrorInFile fileId) (toList errs)
                -- Convert cycles of files into cycles of module names
                forM cycles $ \(imp, files) -> do
                    modNames <- forM files $ \fileId -> do
                        let file = idToPath depPathIdMap fileId
                        getModuleName file
                    pure $ toDiag imp $ sort modNames
    where cycleErrorInFile f (PartOfCycle imp fs)
            | f `elem` fs = Just (imp, fs)
          cycleErrorInFile _ _ = Nothing
          toDiag imp mods = (fp , ShowDiag , ) $ Diagnostic
            { _range = (_range :: Location -> Range) loc
            , _severity = Just DsError
            , _source = Just "Import cycle detection"
            , _message = "Cyclic module dependency between " <> showCycle mods
            , _code = Nothing
            , _relatedInformation = Nothing
            }
            where loc = srcSpanToLocation (getLoc imp)
                  fp = toNormalizedFilePath $ srcSpanToFilename (getLoc imp)
          getModuleName file = do
           pm <- use_ GetParsedModule file
           pure (moduleNameString . moduleName . ms_mod $ pm_mod_summary pm)
          showCycle mods  = T.intercalate ", " (map T.pack mods)

-- returns all transitive dependencies in topological order.
-- NOTE: result does not include the argument file.
getDependenciesRule :: _ => WRule
getDependenciesRule =
    defineEarlyCutoff GetDependencies $ \file -> do
        depInfo <- use_ GetDependencyInformation file
        let allFiles = reachableModules depInfo
        _ <- uses_ ReportImportCycles allFiles
        opts <- getIdeOptions
        let mbFingerprints = map (fingerprintString . fromNormalizedFilePath) allFiles <$ optShakeFiles opts
        return (fingerprintToBS . fingerprintFingerprints <$> mbFingerprints, ([], transitiveDeps depInfo file))

-- Source SpanInfo is used by AtPoint and Goto Definition.
getSpanInfoRule :: _ => WRule
getSpanInfoRule =
    define GetSpanInfo $ \file -> do
        tc <- use_ GetTypecheckedModule file
        deps <- maybe (TransitiveDependencies [] [] []) id <$> useWithStale GetDependencies file
        let tdeps = transitiveModuleDeps deps
        parsedDeps <- uses_ GetParsedModule tdeps
        ifaces <- uses_ GetModIface tdeps
        (fileImports, _) <- use_ GetLocatedImports file
        packageState <- hscEnv <$> use_ GhcSession file
        let imports = second (fmap artifactFilePath) <$> fileImports
        x <- liftIO $ getSrcSpanInfos packageState imports tc (zip parsedDeps $ map hirModIface ifaces)
        return ([], Just x)

-- Typechecks a module.
typeCheckRule :: _ => WRule
typeCheckRule = define GetTypecheckedModule typeCheckRuleDefinition

-- This is factored out so it can be directly called from the GetModIface
-- rule. Directly calling this rule means that on the initial load we can
-- garbage collect all the intermediate typechecked modules rather than
-- retain the information forever in the shake graph.
typeCheckRuleDefinition :: _ => NormalizedFilePath -> ActionM t m (IdeResult TcModuleResult)
typeCheckRuleDefinition file = do
  pm   <- use_ GetParsedModule file
  deps <- use_ GetDependencies file
  hsc  <- hscEnv <$> use_ GhcSession file
  -- Figure out whether we need TemplateHaskell or QuasiQuotes support
  let graph_needs_th_qq = needsTemplateHaskellOrQQ $ hsc_mod_graph hsc
      file_uses_th_qq   = uses_th_qq $ ms_hspp_opts (pm_mod_summary pm)
      any_uses_th_qq    = graph_needs_th_qq || file_uses_th_qq
  mirs      <- uses_ GetModIface (transitiveModuleDeps deps)
  bytecodes <- if any_uses_th_qq
    then -- If we use TH or QQ, we must obtain the bytecode
      fmap Just <$> uses_ GenerateByteCode (transitiveModuleDeps deps)
    else
      pure $ repeat Nothing

  setPriority priorityTypeCheck
  IdeOptions { optDefer = defer } <- getIdeOptions

  res <- liftIO $ typecheckModule defer hsc (zipWith unpack mirs bytecodes) pm

  case res of
    (diags, Just (hsc,tcm)) -> do
      (_, contents) <- getFileContents file
      diagsHie <- liftIO $
        generateAndWriteHieFile hsc (TE.encodeUtf8 <$> contents) (tmrModule tcm)
      diagsHi  <- liftIO $
        generateAndWriteHiFile hsc tcm
      return (diags <> diagsHi <> diagsHie, Just tcm)
    (diags, res) ->
      return (diags, snd <$> res)
 where
  unpack HiFileResult{..} bc = (hirModSummary, (hirModIface, bc))
  uses_th_qq dflags =
    xopt LangExt.TemplateHaskell dflags || xopt LangExt.QuasiQuotes dflags


generateCore :: _ => NormalizedFilePath -> ActionM t m (IdeResult (SafeHaskellMode, CgGuts, ModDetails))
generateCore file = do
    deps <- use_ GetDependencies file
    (tm:tms) <- uses_ GetTypecheckedModule (file:transitiveModuleDeps deps)
    setPriority priorityGenerateCore
    packageState <- hscEnv <$> use_ GhcSession file
    liftIO $ compileModule packageState [(tmrModSummary x, tmrModInfo x) | x <- tms] tm

generateCoreRule :: _ => WRule
generateCoreRule =
    define GenerateCore generateCore

generateByteCodeRule :: _ => WRule
generateByteCodeRule =
    define GenerateByteCode $ \file -> do
      deps <- use_ GetDependencies file
      (tm : tms) <- uses_ GetTypecheckedModule (file: transitiveModuleDeps deps)
      session <- hscEnv <$> use_ GhcSession file
      (_, guts, _) <- use_ GenerateCore file
      liftIO $ generateByteCode session [(tmrModSummary x, tmrModInfo x) | x <- tms] tm guts

-- A local rule type to get caching. We want to use newCache, but it has
-- thread killed exception issues, so we lift it to a full rule.
-- https://github.com/digital-asset/daml/pull/2808#issuecomment-529639547
--type instance RuleResult GhcSessionIO = GhcSessionFun



loadGhcSession :: _ => WRule
loadGhcSession = do
    --defineNoFile $ \GhcSessionIO -> do
    --    opts <- getIdeOptions
    --    GhcSessionFun <$> optGhcSession opts
    defineEarlyCutoff GhcSession $ \file -> do
        GhcSessionFun fun <- useNoFile_ GhcSessionIO
        let ForallAction val = fun $ fromNormalizedFilePath file
        res <- val
        opts <- getIdeOptions
        return (Nothing, ([], Just res))

getHiFileRule :: _ => WRule
getHiFileRule = defineEarlyCutoff GetHiFile $ \f -> do
  session <- hscEnv <$> use_ GhcSession f
  -- get all dependencies interface files, to check for freshness
  (deps,_)<- use_ GetLocatedImports f
  depHis  <- traverse (use GetHiFile) (mapMaybe (fmap artifactFilePath . snd) deps)

  -- TODO find the hi file without relying on the parsed module
  --      it should be possible to construct a ModSummary parsing just the imports
  --      (see HeaderInfo in the GHC package)
  pm      <- use_ GetParsedModule f
  let hiFile = toNormalizedFilePath $
            case ms_hsc_src ms of
                HsBootFile -> addBootSuffix (ml_hi_file $ ms_location ms)
                _ -> ml_hi_file $ ms_location ms
      ms     = pm_mod_summary pm

  case sequence depHis of
    Nothing -> do
          let d = mkInterfaceFilesGenerationDiag f "Missing interface file dependencies"
          pure (Nothing, (d, Nothing))
    Just deps -> do
      gotHiFile <- getFileExists hiFile
      if not gotHiFile
        then do
          let d = mkInterfaceFilesGenerationDiag f "Missing interface file"
          pure (Nothing, (d, Nothing))
        else do
          hiVersion  <- getModificationTimeM hiFile
          modVersion <- getModificationTimeM f
          let sourceModified = LT == comparing modificationTime hiVersion modVersion
          if sourceModified
            then do
              let d = mkInterfaceFilesGenerationDiag f "Stale interface file"
              pure (Nothing, (d, Nothing))
            else do
              r <- liftIO $ loadInterface session ms deps
              case r of
                Right iface -> do
                  let result = HiFileResult ms iface
                  return (Just (fingerprintToBS (mi_mod_hash iface)), ([], Just result))
                Left err -> do
                  let diag = ideErrorWithSource (Just "interface file loading") (Just DsError) f . T.pack $ err
                  return (Nothing, (pure diag, Nothing))

mkInterfaceFilesGenerationDiag :: a -> String -> [(a, ShowDiagnostic, Diagnostic)]
mkInterfaceFilesGenerationDiag f intro = mkDiag $ intro <> msg
  where
      msg = ": additional resource use while generating interface files in the background."
      mkDiag = pure . ideErrorWithSource (Just "interface file loading") (Just DsInfo) f . T.pack

getModIfaceRule :: _ => WRule
getModIfaceRule = define GetModIface $ \f -> do
    fileOfInterest <- use_ IsFileOfInterest f
    let useHiFile =
          -- Never load interface files for files of interest
          not fileOfInterest
    mbHiFile <- if useHiFile then use GetHiFile f else return Nothing
    case mbHiFile of
        Just x ->
            return ([], Just x)
        Nothing
          | fileOfInterest -> do
            tmr <- use GetTypecheckedModule f
            return ([], extract tmr)
          | otherwise -> do
            (diags, tmr) <- typeCheckRuleDefinition f
            -- Bang pattern is important to avoid leaking 'tmr'
            let !res = extract tmr
            return (diags, res)
    where
      extract Nothing = Nothing
      extract (Just tmr) =
        -- Bang patterns are important to force the inner fields
        Just $! HiFileResult (tmrModSummary tmr) (hm_iface $ tmrModInfo tmr)

isFileOfInterestRule :: _ => WRule
isFileOfInterestRule = undefined --defineEarlyCutoff IsFileOfInterest $ \f -> do
--    filesOfInterest <- undefined --getFilesOfInterest TODO with dynamic
    --let res = f `elem` filesOfInterest
    --return (Just (if res then "1" else ""), ([], Just res))

-- | A rule that wires per-file rules together
mainRule :: Rules
mainRule =
    [ getParsedModuleRule
    , getLocatedImportsRule
    , getDependencyInformationRule
    , reportImportCyclesRule
    , getDependenciesRule
    , typeCheckRule
    , getSpanInfoRule
    , generateCoreRule
    , generateByteCodeRule
    , loadGhcSession
    , getHiFileRule
    , getModIfaceRule
    , isFileOfInterestRule ]
