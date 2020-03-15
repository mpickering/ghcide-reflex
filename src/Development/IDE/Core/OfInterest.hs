-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Utilities and state for the files of interest - those which are currently
--   open in the editor. The useful function is 'getFilesOfInterest'.
module Development.IDE.Core.OfInterest(
    ofInterestRules, getFilesOfInterest, isFileOfInterest
    ) where

import Data.HashSet (HashSet)
import qualified Data.HashSet as S

import Development.IDE.Types.Location
import Development.IDE.Types.Logger
import Development.IDE.Core.Reflex
import Development.IDE.Core.RuleTypes
import Reflex
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Types



-- | The rule that initialises the files of interest state.
ofInterestRules :: Rules
ofInterestRules = do
    [ofInterestVar]
      {-
    defineEarlyCutoff $ \GetFilesOfInterest _file -> assert (null $ fromNormalizedFilePath _file) $ do
        alwaysRerun
        filesOfInterest <- getFilesOfInterestUntracked
        pure (Just $ BS.fromString $ show filesOfInterest, ([], Just filesOfInterest))
        -}

ofInterestVar :: WRule
ofInterestVar =
  addIdeGlobal OfInterestVar $ do
    e1 <- withNotification <$> getHandlerEvent didOpenTextDocumentNotificationHandler
    e2 <- withNotification <$> getHandlerEvent didCloseTextDocumentNotificationHandler
--    e3 <- setInterestEvent
    upd <- logAction Info (fmapMaybe check e1)
    upd2 <- logAction Info (fmapMaybe check2 e2)
    d <- foldDyn ($) S.empty (mergeWith (.) [upd, upd2])
    return $ traceDyn "ofInterest" d
  where
      check (DidOpenTextDocumentParams TextDocumentItem{_uri, _version}) =
        whenUriFile _uri Nothing $ \file -> Just (add file, "Opened text document: " <> getUri _uri)


      check2 (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) =
        whenUriFile _uri Nothing $ \file -> Just (remove file, "Closed text document:" <> getUri _uri)
      add file = S.insert file
      remove file = S.delete file



getFilesOfInterest :: (Reflex t, MonadSample t m) => ActionM t m (HashSet NormalizedFilePath)
getFilesOfInterest = useNoFile_ OfInterestVar

isFileOfInterest :: (Reflex t, MonadSample t m) => NormalizedFilePath -> ActionM t m Bool
isFileOfInterest fp = S.member fp <$> getFilesOfInterest



------------------------------------------------------------
-- Exposed API

-- | Set the files-of-interest - not usually necessary or advisable.
--   The LSP client will keep this information up to date.
{-
setFilesOfInterest :: IdeState -> HashSet NormalizedFilePath -> IO ()
setFilesOfInterest state files = modifyFilesOfInterest state (const files)

getFilesOfInterestUntracked :: Action (HashSet NormalizedFilePath)
getFilesOfInterestUntracked = do
    OfInterestVar var <- getIdeGlobalAction
    liftIO $ readVar var

-- | Modify the files-of-interest - not usually necessary or advisable.
--   The LSP client will keep this information up to date.
modifyFilesOfInterest :: IdeState -> (HashSet NormalizedFilePath -> HashSet NormalizedFilePath) -> IO ()
modifyFilesOfInterest state f = do
    OfInterestVar var <- getIdeGlobalState state
    files <- modifyVar var $ pure . dupe . f
    logDebug (ideLogger state) $ "Set files of interest to: " <> T.pack (show $ HashSet.toList files)
    void $ shakeRun state []
    -}
