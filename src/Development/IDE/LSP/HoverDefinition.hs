-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE PartialTypeSignatures #-}


-- | Display information on hover.
module Development.IDE.LSP.HoverDefinition
    ( hoverRule
    , goToDefinitionRule
    ) where

import           Development.IDE.Core.Rules
import           Development.IDE.Core.Reflex
--import           Development.IDE.Core.Service
--import           Development.IDE.LSP.Server
import           Development.IDE.Types.Location
--import           Development.IDE.Types.Logger
import qualified Language.Haskell.LSP.Core       as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import Reflex
import Control.Monad.IO.Class


import qualified Data.Text as T

gotoDefinition :: (Reflex t, MonadIO m, MonadSample t m)
                => TextDocumentPositionParams
                -> ActionM t m (Either ResponseError LocationResponseParams)
hover          :: (Reflex t, MonadIO m, MonadSample t m)
                => TextDocumentPositionParams
                -> ActionM t m (Either ResponseError (Maybe Hover))
gotoDefinition = request "Definition" getDefinition (MultiLoc []) SingleLoc
hover          = request "Hover"      getAtPoint     Nothing      foundHover

foundHover :: (Maybe Range, [T.Text]) -> Maybe Hover
foundHover (mbRange, contents) =
  Just $ Hover (HoverContents $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator contents) mbRange

hoverRule :: WRule
hoverRule = unitAction $ do
  hover_e <- getHandlerEvent LSP.hoverHandler
  e <- waitInit hover_e
  withResponse (Just (0.1, Nothing)) RspHover e hover

goToDefinitionRule :: WRule
goToDefinitionRule = unitAction $ do
  goto_e <- getHandlerEvent LSP.definitionHandler
  e <- waitInit goto_e
  withResponse (Just (1, MultiLoc [])) RspDefinition e gotoDefinition

{-
setHandlersDefinition, setHandlersHover :: PartialHandlers c
setHandlersDefinition = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.definitionHandler = withResponse RspDefinition $ const gotoDefinition}
setHandlersHover      = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.hoverHandler      = withResponse RspHover      $ const hover}
  -}

-- | Respond to and log a hover or go-to-definition request
request
  :: (Monad m) => T.Text
  -> (NormalizedFilePath -> Position -> ActionM t m (Maybe a))
  -> b
  -> (a -> b)
  -> TextDocumentPositionParams
  -> ActionM t m (Either ResponseError b)
request label getResults notFound found (TextDocumentPositionParams (TextDocumentIdentifier uri) pos _) = do
    mbResult <- case uriToFilePath' uri of
        Just path -> logAndRunRequest label getResults pos path
        Nothing   -> pure Nothing
    pure $ Right $ maybe notFound found mbResult

logAndRunRequest :: T.Text -> (NormalizedFilePath -> Position -> ActionM t m b)  -> Position -> String -> ActionM t m b
logAndRunRequest _label getResults pos path = do
  let filePath = toNormalizedFilePath path
--  logInfo (ideLogger ide) $
--    label <> " request at position " <> T.pack (showPosition pos) <>
--    " in file: " <> T.pack path
  getResults filePath pos
