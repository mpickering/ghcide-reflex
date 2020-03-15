{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Development.IDE.Core.IdeConfiguration
  ( IdeConfiguration(..)
  , parseConfiguration
  , parseWorkspaceFolder
  , isWorkspaceFile
  , ideConfigurationRule
  )
where

import           Data.HashSet                   (singleton)
import qualified Data.HashSet as S
import           Data.Text                      (Text, isPrefixOf)
import           Development.IDE.Core.Reflex
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Types.Location
import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Core
import Reflex
import Control.Monad.Trans

ideConfigurationRule :: WRule
ideConfigurationRule = addIdeGlobal IdeConfigurationVar $ do
  e <- getHandlerEvent didChangeWorkspaceFoldersNotificationHandler
  lift $ foldDyn (\i e -> modifyIdeConfig (go i) e) (IdeConfiguration S.empty) e
  where
    modifyIdeConfig f (IdeConfiguration w) = (IdeConfiguration (f w))
    add       = S.union
    substract = flip S.difference
    go (NotificationMessage _ _ (DidChangeWorkspaceFoldersParams events)) =
          add       (foldMap (S.singleton . parseWorkspaceFolder) (_added   events))
            . substract (foldMap (S.singleton . parseWorkspaceFolder) (_removed events))
  {-
  withNotification (LSP.didChangeWorkspaceFoldersNotificationHandler x) $
        \_ ide (DidChangeWorkspaceFoldersParams events) -> do
            modifyWorkspaceFolders ide
              -}

getIdeConfiguration :: (Reflex t, MonadSample t m) => ActionM t m IdeConfiguration
getIdeConfiguration =
  useNoFile_ IdeConfigurationVar
--  getIdeGlobalAction >>= liftIO . readVar . unIdeConfigurationRef

parseConfiguration :: InitializeParams -> IdeConfiguration
parseConfiguration InitializeParams {..} =
  IdeConfiguration { .. }
 where
  workspaceFolders =
    foldMap (singleton . toNormalizedUri) _rootUri
      <> (foldMap . foldMap)
           (singleton . parseWorkspaceFolder)
           _workspaceFolders

parseWorkspaceFolder :: WorkspaceFolder -> NormalizedUri
parseWorkspaceFolder =
  toNormalizedUri . Uri . (_uri :: WorkspaceFolder -> Text)

{-
modifyWorkspaceFolders
  :: IdeState -> (HashSet NormalizedUri -> HashSet NormalizedUri) -> IO ()
modifyWorkspaceFolders ide f = do
  IdeConfigurationVar var <- getIdeGlobalState ide
  IdeConfiguration    ws  <- readVar var
  writeVar var (IdeConfiguration (f ws))
  -}

isWorkspaceFile :: (Reflex t, MonadSample t m) => NormalizedFilePath -> ActionM t m Bool
isWorkspaceFile file = do
  IdeConfiguration {..} <- getIdeConfiguration
  let toText = getUri . fromNormalizedUri
  return $ any
    (\root -> toText root `isPrefixOf` toText (filePathToUri' file))
    workspaceFolders
