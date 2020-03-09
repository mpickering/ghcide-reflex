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

import Control.Concurrent.Extra
import Data.Binary
import Data.Hashable
import Control.DeepSeq
import GHC.Generics
import Data.Typeable
import qualified Data.ByteString.UTF8 as BS
import Control.Exception
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import qualified Data.Text as T
import Data.Tuple.Extra
import Data.Functor

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
    foldDyn ($) S.empty (mergeWith (.) [check add <$> e1, check2 remove <$> e2])
  where
      check act (DidOpenTextDocumentParams TextDocumentItem{_uri, _version}) hs =
        whenUriFile _uri hs $ \file -> (act file hs)

      check2 act (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) hs =
        whenUriFile _uri hs $ \file -> (act file hs)
      add file = S.insert file
      remove file = S.delete file



getFilesOfInterest :: _ => ActionM t m (HashSet NormalizedFilePath)
getFilesOfInterest = useNoFile_ OfInterestVar

isFileOfInterest :: _ => NormalizedFilePath -> ActionM t m Bool
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
