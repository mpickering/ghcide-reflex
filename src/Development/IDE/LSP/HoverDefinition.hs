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
import           Development.IDE.Types.Logger
import qualified Language.Haskell.LSP.Core       as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types

import qualified Data.Text as T

gotoDefinition :: _ => TextDocumentPositionParams -> BasicM t m (Either ResponseError LocationResponseParams)
hover          :: _ => TextDocumentPositionParams -> BasicM t m (Either ResponseError (Maybe Hover))
gotoDefinition = request "Definition" getDefinition (MultiLoc []) SingleLoc
hover          = request "Hover"      getAtPoint     Nothing      foundHover

foundHover :: (Maybe Range, [T.Text]) -> Maybe Hover
foundHover (mbRange, contents) =
  Just $ Hover (HoverContents $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator contents) mbRange

hoverRule :: WRule
hoverRule = unitAction $ do
  hover_e <- getHandlerEvent LSP.hoverHandler
  e <- waitInit hover_e
  withResponse (Just 0.1) RspHover e (liftBasic . hover)

goToDefinitionRule :: WRule
goToDefinitionRule = unitAction $ do
  goto_e <- getHandlerEvent LSP.definitionHandler
  e <- waitInit goto_e
  withResponse (Just 1) RspDefinition goto_e (liftBasic . gotoDefinition)

{-
setHandlersDefinition, setHandlersHover :: PartialHandlers c
setHandlersDefinition = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.definitionHandler = withResponse RspDefinition $ const gotoDefinition}
setHandlersHover      = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.hoverHandler      = withResponse RspHover      $ const hover}
  -}

-- | Respond to and log a hover or go-to-definition request
request
  :: _ => T.Text
  -> (NormalizedFilePath -> Position -> BasicM t m (Maybe a))
  -> b
  -> (a -> b)
  -> TextDocumentPositionParams
  -> BasicM t m (Either ResponseError b)
request label getResults notFound found (TextDocumentPositionParams (TextDocumentIdentifier uri) pos _) = do
    mbResult <- case uriToFilePath' uri of
        Just path -> logAndRunRequest label getResults pos path
        Nothing   -> pure Nothing
    pure $ Right $ maybe notFound found mbResult

logAndRunRequest :: _ => T.Text -> (NormalizedFilePath -> Position -> BasicM t m b)  -> Position -> String -> BasicM t m b
logAndRunRequest label getResults pos path = do
  let filePath = toNormalizedFilePath path
--  logInfo (ideLogger ide) $
--    label <> " request at position " <> T.pack (showPosition pos) <>
--    " in file: " <> T.pack path
  getResults filePath pos
