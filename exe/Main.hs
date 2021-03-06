-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE CPP #-} -- To get precise GHC version
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Main(main) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Dependent.Map as D
import Reflex
import Arguments
import Data.Maybe
import Data.List.Extra
import System.FilePath
import Control.Concurrent.Extra
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA1 as H
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base16
import Data.Default
import Development.IDE.Core.Debouncer
import Development.IDE.Core.FileStore
import Development.IDE.Core.Service
import Development.IDE.Core.Rules
import Development.IDE.Core.Reflex hiding (getSession)
import Development.IDE.Core.RuleTypes
import Development.IDE.LSP.Protocol
import Development.IDE.Types.Location
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Options
import Development.IDE.Types.Logger
import Development.IDE.GHC.Util
--import Development.IDE.Plugin
--import Development.IDE.Plugin.Completions as Completions
--import Development.IDE.Plugin.CodeAction as CodeAction
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types (LspId(IdInt))
import Linker
import Data.Version
import Development.IDE.LSP.LanguageServer
import qualified System.Directory.Extra as IO
import System.Environment
import System.IO
import System.Exit
import Paths_ghcide
import Development.GitRev
--import Development.Shake (Action, RuleResult, Rules, action, doesFileExist, need)
import qualified Data.Map.Strict as Map

import GHC hiding (def)
import qualified GHC.Paths
import           DynFlags

import HIE.Bios.Environment
import HIE.Bios
--import HIE.Bios.Cradle
import HIE.Bios.Types
--import Development.IDE.Core.Service

-- Prefix for the cache path
cacheDir :: String
cacheDir = "ghcide"

-- Set the GHC libdir to the nix libdir if it's present.
getLibdir :: IO FilePath
getLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"

getCacheDir :: [String] -> IO FilePath
getCacheDir opts = IO.getXdgDirectory IO.XdgCache (cacheDir </> opts_hash)
    where
        -- Create a unique folder per set of different GHC options, assuming that each different set of
        -- GHC options will create incompatible interface files.
        opts_hash = B.unpack $ encode $ H.finalize $ H.updates H.init (map B.pack opts)

ghcideVersion :: IO String
ghcideVersion = do
  path <- getExecutablePath
  let gitHashSection = case $(gitHash) of
        x | x == "UNKNOWN" -> ""
        x -> " (GIT hash: " <> x <> ")"
  return $ "ghcide version: " <> showVersion version
             <> " (GHC: " <> VERSION_ghc
             <> ") (PATH: " <> path <> ")"
             <> gitHashSection

main :: IO ()
main = do
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work
    Arguments{..} <- getArguments

    if argsVersion then ghcideVersion >>= putStrLn >> exitSuccess
    else hPutStrLn stderr {- see WARNING above -} =<< ghcideVersion

    -- lock to avoid overlapping output on stdout
    lock <- newLock
    let logger p = Logger $ \pri msg -> when (pri >= p) $ withLock lock $
            T.putStrLn $ T.pack ("[" ++ upper (show pri) ++ "] ") <> msg

    whenJust argsCwd IO.setCurrentDirectory

    dir <- IO.getCurrentDirectory

    let --plugins = Completions.plugin <> CodeAction.plugin
        onInitialConfiguration = const $ Right ()
        onConfigurationChange  = const $ Right ()

    if argLSP then do
--        t <- offsetTime
        hPutStrLn stderr "Starting LSP server..."
        hPutStrLn stderr "If you are seeing this in a terminal, you probably should have run ghcide WITHOUT the --lsp option!"

        let options = defaultIdeOptions $ ForallDynamic
                        (return $ constDyn (\fp -> ForallAction (getGhcSession dir fp)))
        debouncer <- newAsyncDebouncer
        initialise (loadGhcSessionIO ++ mainRule) -- >> pluginRules plugins >> action kick)
            (logger minBound) debouncer options
            (\h i _ -> ForallBasic $ liftIO $ void $ forkIO $ runLanguageServer def def onInitialConfiguration onConfigurationChange h i)
    else do
        putStrLn $ "Ghcide setup tester in " ++ dir ++ "."
        putStrLn "Report bugs at https://github.com/digital-asset/ghcide/issues"

        putStrLn $ "\nStep 1/6: Finding files to test in " ++ dir
        files <- expandFiles (argFiles ++ ["." | null argFiles])
        -- LSP works with absolute file paths, so try and behave similarly
        files <- nubOrd <$> mapM IO.canonicalizePath files
        putStrLn $ "Found " ++ show (length files) ++ " files"

        putStrLn "\nStep 2/6: Looking for hie.yaml files that control setup"
        cradles <- mapM findCradle files
        let ucradles = nubOrd cradles
        let n = length ucradles
        putStrLn $ "Found " ++ show n ++ " cradle" ++ ['s' | n /= 1]
        sessions <- forM (zipFrom (1 :: Int) ucradles) $ \(i, x) -> do
            let msg = maybe ("Implicit cradle for " ++ dir) ("Loading " ++) x
            putStrLn $ "\nStep 3/6, Cradle " ++ show i ++ "/" ++ show n ++ ": " ++ msg
            cradle <- maybe (loadImplicitCradle $ addTrailingPathSeparator dir) loadCradle x
            when (isNothing x) $ print cradle
            putStrLn $ "\nStep 4/6, Cradle " ++ show i ++ "/" ++ show n ++ ": Loading GHC Session"
            opts <- getComponentOptions cradle
            createSession opts

        putStrLn "\nStep 5/6: Initializing the IDE"
        vfs <- makeVFSHandle
        let cradlesToSessions = Map.fromList $ zip ucradles sessions
        let filesToCradles = Map.fromList $ zip files cradles

        let grab :: Monad m => FilePath -> ActionM t m HscEnvEq
            grab file = return $ fromMaybe (head sessions) $ do
                cradle <- Map.lookup file filesToCradles
                Map.lookup cradle cradlesToSessions

        let options =
              (defaultIdeOptions $ ForallDynamic (return $ constDyn (\fp -> ForallAction (grab fp))))
                    { optShakeProfiling = argsShakeProfiling }

        initialise (loadGhcSessionIO ++ mainRule) (logger Info) noopDebouncer options
                           (\_ f open -> ForallBasic $ do
                liftIO $ f (vfs, def, (pure $ IdInt 0), (showEvent lock))
                liftIO $ putStrLn "\nStep 6/6: Type checking the files"
                --setFilesOfInterest ide $ HashSet.fromList $ map toNormalizedFilePath files
                let typecheck fp = open (D.Some GetTypecheckedModule
                                        , toNormalizedFilePath fp)
                liftIO $ mapM_ typecheck files
                {-
                let (worked, failed) = partition fst $ zip (map isJust results) files
                when (failed /= []) $
                  putStr $ unlines $ "Files that failed:" : map ((++) " * " . snd) failed

                let files xs = let n = length xs in if n == 1 then "1 file" else show n ++ " files"
                putStrLn $ "\nCompleted (" ++ files worked ++ " worked, " ++ files failed ++ " failed)"
                -}
                return ())

                --unless (null failed) exitFailure)


expandFiles :: [FilePath] -> IO [FilePath]
expandFiles = concatMapM $ \x -> do
    b <- IO.doesFileExist x
    if b then return [x] else do
        let recurse "." = True
            recurse x | "." `isPrefixOf` takeFileName x = False -- skip .git etc
            recurse x = takeFileName x `notElem` ["dist","dist-newstyle"] -- cabal directories
        files <- filter (\x -> takeExtension x `elem` [".hs",".lhs"]) <$> IO.listFilesInside (return . recurse) x
        when (null files) $
            fail $ "Couldn't find any .hs/.lhs files inside directory: " ++ x
        return files


-- | Print an LSP event.
showEvent :: Lock -> FromServerMessage -> IO ()
showEvent _ (EventFileDiagnostics _ []) = return ()
showEvent lock (EventFileDiagnostics (toNormalizedFilePath -> file) diags) =
    withLock lock $ T.putStrLn $ showDiagnosticsColored $ map (file,ShowDiag,) diags
showEvent lock e = withLock lock $ print e

{-
-- Rule type for caching GHC sessions.
type instance RuleResult GetHscEnv = HscEnvEq

-}


loadGhcSessionIO :: Rules
loadGhcSessionIO =
  [addIdeGlobal GetHscEnv $ do

        cradleLoc <- liftIO $ memoIO $ \v -> do
          res <- findCradle v
          -- Sometimes we get C:, sometimes we get c:, and sometimes we get a relative path
          -- try and normalise that
          -- e.g. see https://github.com/digital-asset/ghcide/issues/126
          res' <- traverse IO.makeAbsolute res
          return $ normalise <$> res'
        (update, trig) <- newTriggerEvent
        update' <- batchOccurrences 0.01 update
        md <- foldDyn (\kvs m -> M.fromList (F.toList kvs) `M.union` m) M.empty update'
        return $ (\m -> SessionMap m cradleLoc trig) <$> md]


getGhcSession :: (Reflex t, MonadIO m, MonadSample t m)
                => FilePath -> FilePath -> ActionM t m HscEnvEq
getGhcSession dir file = do
  (SessionMap m cl t) <- useNoFile_ $ GetHscEnv
  nfp <- liftIO $ cl file
  case nfp >>= flip M.lookup m of
    Just res -> return res
    Nothing -> liftIO $ do
      c <- maybe (loadImplicitCradle $ addTrailingPathSeparator dir) loadCradle nfp
--      cradleToSession file c
      cmpOpts <- getComponentOptions c
      r <- createSession cmpOpts
      t (fromMaybe dir nfp, r)
      return r




getComponentOptions :: Cradle a -> IO ComponentOptions
getComponentOptions cradle = do
    let showLine s = putStrLn ("> " ++ s)
    cradleRes <- runCradle (cradleOptsProg cradle) showLine ""
    case cradleRes of
        CradleSuccess r -> pure r
        CradleFail err -> throwIO err
        -- TODO Rather than failing here, we should ignore any files that use this cradle.
        -- That will require some more changes.
        CradleNone -> fail "'none' cradle is not yet supported"


createSession :: ComponentOptions -> IO HscEnvEq
createSession (ComponentOptions theOpts _) = do
    libdir <- getLibdir

    cacheDir <- Main.getCacheDir theOpts

    env <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        (dflags', _targets) <- addCmdOpts theOpts dflags
        _ <- setSessionDynFlags $
             -- disabled, generated directly by ghcide instead
             flip gopt_unset Opt_WriteInterface $
             -- disabled, generated directly by ghcide instead
             -- also, it can confuse the interface stale check
             dontWriteHieFiles $
             setHiDir cacheDir $
             setDefaultHieDir cacheDir $
             setIgnoreInterfacePragmas $
             setLinkerOptions $
             disableOptimisation dflags'
        getSession
    initDynLinker env
    newHscEnvEq env

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink   = LinkInMemory
  , hscTarget = HscNothing
  , ghcMode = CompManager
  }

setIgnoreInterfacePragmas :: DynFlags -> DynFlags
setIgnoreInterfacePragmas df =
    gopt_set (gopt_set df Opt_IgnoreInterfacePragmas) Opt_IgnoreOptimChanges

disableOptimisation :: DynFlags -> DynFlags
disableOptimisation df = updOptLevel 0 df

setHiDir :: FilePath -> DynFlags -> DynFlags
setHiDir f d =
    -- override user settings to avoid conflicts leading to recompilation
    d { hiDir      = Just f}

{-
cradleToSession :: _ => Maybe FilePath -> Cradle a -> ActionM t m HscEnvEq
cradleToSession mbYaml cradle = do
    cmpOpts <- liftIO $ getComponentOptions cradle
    let opts = componentOptions cmpOpts
        deps = componentDependencies cmpOpts
        deps' = case mbYaml of
                  -- For direct cradles, the hie.yaml file itself must be watched.
                  Just yaml | isDirectCradle cradle -> yaml : deps
                  _ -> deps
    existingDeps <- liftIO $ filterM (IO.doesFileExist) deps'
    --need existingDeps
    --
    getGhcSession (GetHscEnvArgs opts deps)


loadSession :: _ => FilePath -> DynamicM t m (FilePath -> ForallAction HscEnvEq)
loadSession dir = liftIO $ do
    cradleLoc <- memoIO $ \v -> do
        res <- findCradle v
        -- Sometimes we get C:, sometimes we get c:, and sometimes we get a relative path
        -- try and normalise that
        -- e.g. see https://github.com/digital-asset/ghcide/issues/126
        res' <- traverse IO.makeAbsolute res
        return $ normalise <$> res'
    let session :: C t => Maybe FilePath -> ActionM t (HostFrame t) HscEnvEq
        session file = do
          c <- liftIO $ maybe (loadImplicitCradle $ addTrailingPathSeparator dir) loadCradle file
          cradleToSession file c
    return $ constDyn (\file -> ForallAction (session =<< liftIO (cradleLoc file)))
    -}


-- | Memoize an IO function, with the characteristics:
--
--   * If multiple people ask for a result simultaneously, make sure you only compute it once.
--
--   * If there are exceptions, repeatedly reraise them.
--
--   * If the caller is aborted (async exception) finish computing it anyway.
memoIO :: Ord a => (a -> IO b) -> IO (a -> IO b)
memoIO op = do
    ref <- newVar Map.empty
    return $ \k -> join $ mask_ $ modifyVar ref $ \mp ->
        case Map.lookup k mp of
            Nothing -> do
                res <- onceFork $ op k
                return (Map.insert k res mp, res)
            Just res -> return (mp, res)
