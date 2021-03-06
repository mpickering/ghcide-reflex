{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Development.IDE.Core.FileExists
  ( fileExistsRules
  , getFileExists
  )

where

import           Data.Foldable                    as F
import           Control.Exception
import           Control.Monad.Extra
import qualified Data.Aeson                    as A
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe
import qualified Data.Text                     as T
import           Development.IDE.Core.FileStore
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.Reflex
import           Development.IDE.Types.Location
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Capabilities
import qualified System.Directory as Dir
import Control.Monad.IO.Class
import Development.IDE.Core.RuleTypes
import Language.Haskell.LSP.Core
import Development.IDE.Types.Logger
import Reflex (Reflex, MonadSample, foldDyn)


-- | A wrapper around a mutable 'FileExistsMap'
-- | Grab the current global value of 'FileExistsMap' without acquiring a dependency
getFileExistsMapUntracked :: (Reflex t, MonadSample t m) => ActionM t m FileExistsMap
getFileExistsMapUntracked = do
  useNoFile_ FileExistsMapVar

{-
-- | Modify the global store of file exists
modifyFileExistsAction :: (FileExistsMap -> IO FileExistsMap) -> Action ()
modifyFileExistsAction f = do
  FileExistsMapVar var <- getIdeGlobalAction
  liftIO $ modifyVar_ var f

-- | Modify the global store of file exists
modifyFileExists :: IdeState -> [(NormalizedFilePath, Bool)] -> IO ()
modifyFileExists state changes = do
  FileExistsMapVar var <- getIdeGlobalState state
  changesMap           <- evaluate $ HashMap.fromList changes

  -- Masked to ensure that the previous values are flushed together with the map update
  mask $ \_ -> do
    -- update the map
    modifyVar_ var $ evaluate . HashMap.union changesMap
    -- flush previous values
    mapM_ (deleteValue state GetFileExists . fst) changes
    -}

-------------------------------------------------------------------------------------

-- | Returns True if the file exists
--   Note that a file is not considered to exist unless it is saved to disk.
--   In particular, VFS existence is not enough.
--   Consider the following example:
--     1. The file @A.hs@ containing the line @import B@ is added to the files of interest
--        Since @B.hs@ is neither open nor exists, GetLocatedImports finds Nothing
--     2. The editor creates a new buffer @B.hs@
--        Unless the editor also sends a @DidChangeWatchedFile@ event, ghcide will not pick it up
--        Most editors, e.g. VSCode, only send the event when the file is saved to disk.
getFileExists :: (Reflex t, MonadIO m, MonadSample t m)
                => NormalizedFilePath -> ActionM t m Bool
getFileExists fp = do
  (v, c, f, _) <- useNoFile GetInitFuncs
  getFileExistsImpl f c v fp



-- | Installs the 'getFileExists' rules.
--   Provides a fast implementation if client supports dynamic watched files.
--   Creates a global state as a side effect in that case.
getFileExistsImpl :: (Reflex t, MonadIO m, MonadSample t m)
                    => IO LspId
                    -> ClientCapabilities
                    -> VFSHandle
                    -> NormalizedFilePath
                    -> ActionM t m Bool
getFileExistsImpl getLspId ClientCapabilities{_workspace} vfs nfp
  | Just WorkspaceClientCapabilities{_didChangeWatchedFiles} <- _workspace
  , Just DidChangeWatchedFilesClientCapabilities{_dynamicRegistration} <- _didChangeWatchedFiles
  , Just True <- _dynamicRegistration
  = fileExistsRulesFast getLspId vfs nfp
  | otherwise = fileExistsSlow nfp

fileExistsRules :: Rules
fileExistsRules = [fileExistsVar]

fileExistsVar :: WRule
fileExistsVar = addIdeGlobal FileExistsMapVar $ do
  ce <- withNotification <$> getHandlerEvent didChangeWatchedFilesNotificationHandler
  updates <- logAction Info (go <$> ce)
  foldDyn HashMap.union HashMap.empty updates
  where
    go (DidChangeWatchedFilesParams fileEvents) =
        let es = mapMaybe
                        (\(FileEvent uri ev) ->
                            (, ev /= FcDeleted) . toNormalizedFilePath
                            <$> uriToFilePath uri
                        )
                        ( (F.toList fileEvents) )
            msg = T.pack $ show es
        in (HashMap.fromList es, "Files created or deleted:" <> msg)

--   Requires an lsp client that provides WatchedFiles notifications.
fileExistsRulesFast :: (Reflex t, MonadIO m, MonadSample t m)
                      => IO LspId
                      -> VFSHandle
                      -> NormalizedFilePath
                      -> ActionM t m Bool
fileExistsRulesFast getLspId vfs file = do
  isWf <- isWorkspaceFile file
  if isWf then fileExistsFast getLspId vfs file else fileExistsSlow file

fileExistsFast :: (Reflex t, MonadIO m, MonadSample t m)
                => IO LspId
                -> VFSHandle
                -> NormalizedFilePath
                -> ActionM t m Bool
fileExistsFast _getLspId vfs file = do
    fileExistsMap <- getFileExistsMapUntracked
    let mbFilesWatched = HashMap.lookup file fileExistsMap
    case mbFilesWatched of
      Just fv -> return fv
      Nothing -> do
        exist                   <- liftIO $ getFileExistsVFS vfs file
        --ShakeExtras { eventer } <- getShakeExtras

        -- add a listener for VFS Create/Delete file events,
        -- taking the FileExistsMap lock to prevent race conditions
        -- that would lead to multiple listeners for the same path
        {-
        modifyFileExistsAction $ \x -> do
          case HashMap.alterF (,Just exist) file x of
            (Nothing, x') -> do
            -- if the listener addition fails, we never recover. This is a bug.
              addListener undefined file
              return x'
            (Just _, _) ->
              -- if the key was already there, do nothing
              return x
              -}

        pure exist
 where
   {-
  addListener eventer fp = do
    reqId <- getLspId
    let
      req = RequestMessage "2.0" reqId ClientRegisterCapability regParams
      fpAsId       = T.pack $ fromNormalizedFilePath fp
      regParams    = RegistrationParams (List [registration])
      registration = Registration fpAsId
                                  WorkspaceDidChangeWatchedFiles
                                  (Just (A.toJSON regOptions))
      regOptions =
        DidChangeWatchedFilesRegistrationOptions { watchers = List [watcher] }
      watcher = FileSystemWatcher { globPattern = fromNormalizedFilePath fp
                                  , kind        = Just 5 -- Create and Delete events only
                                  }

    eventer $ ReqRegisterCapability req
    -}

{-
fileExistsRulesSlow:: VFSHandle -> WRule
fileExistsRulesSlow vfs =
  defineEarlyCutoff GetFileExists $ \file -> fileExistsSlow vfs file

fileExistsSlow :: VFSHandle -> NormalizedFilePath -> Action (Maybe BS.ByteString, ([a], Maybe Bool))
fileExistsSlow vfs file = do
    alwaysRerun
    exist <- liftIO $ getFileExistsVFS vfs file
    pure (summarizeExists exist, ([], Just exist))

-}

fileExistsSlow :: (Reflex t, MonadIO m, MonadSample t m)
                => NormalizedFilePath
                -> ActionM t m Bool
fileExistsSlow f = do
  h <- useNoFile_ GetVFSHandle
  liftIO $ getFileExistsVFS h f

getFileExistsVFS :: VFSHandle -> NormalizedFilePath -> IO Bool
getFileExistsVFS vfs file = do
    -- we deliberately and intentionally wrap the file as an FilePath WITHOUT mkAbsolute
    -- so that if the file doesn't exist, is on a shared drive that is unmounted etc we get a properly
    -- cached 'No' rather than an exception in the wrong place
    handle (\(_ :: IOException) -> return False) $
        (isJust <$> getVirtualFile vfs (filePathToUri' file)) ||^
        Dir.doesFileExist (fromNormalizedFilePath file)

--------------------------------------------------------------------------------------------------
-- The message definitions below probably belong in haskell-lsp-types

data DidChangeWatchedFilesRegistrationOptions = DidChangeWatchedFilesRegistrationOptions
    { watchers :: List FileSystemWatcher
    }

instance A.ToJSON DidChangeWatchedFilesRegistrationOptions where
  toJSON DidChangeWatchedFilesRegistrationOptions {..} =
    A.object ["watchers" A..= watchers]

data FileSystemWatcher = FileSystemWatcher
    { -- | The glob pattern to watch.
      --   For details on glob pattern syntax, check the spec: https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/#workspace_didChangeWatchedFiles
      globPattern :: String
        -- | The kind of event to subscribe to. Defaults to all.
        --   Defined as a bitmap of Create(1), Change(2), and Delete(4)
    , kind        :: Maybe Int
    }

instance A.ToJSON FileSystemWatcher where
  toJSON FileSystemWatcher {..} =
    A.object
      $  ["globPattern" A..= globPattern]
      ++ [ "kind" A..= x | Just x <- [kind] ]
