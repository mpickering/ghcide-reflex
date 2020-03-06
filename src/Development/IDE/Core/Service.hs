-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.Service( initialise
    {-
    IdeState, initialise, shutdown,
    runAction,
    runActionSync,
    writeProfile,
    getDiagnostics, unsafeClearDiagnostics,
    ideLogger,
    updatePositionMapping,
    -}
    ) where

import           Control.Concurrent.Extra
import           Control.Concurrent.Async
import Data.Maybe
import Development.IDE.Types.Options (IdeOptions(..))
import Control.Monad
import Development.IDE.Core.Debouncer
import           Development.IDE.Core.FileStore  (VFSHandle, fileStoreRules)
import           Development.IDE.Core.FileExists (fileExistsRules)
import           Development.IDE.Core.OfInterest
import Development.IDE.Types.Logger
import Data.Either.Extra
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as LSP
import qualified Language.Haskell.LSP.Types.Capabilities as LSP

import           Development.IDE.Core.Reflex
import           Development.IDE.Core.RuleTypes
import Language.Haskell.LSP.Core



newtype GlobalIdeOptions = GlobalIdeOptions IdeOptions

------------------------------------------------------------
-- Exposed API

-- | Initialise the Compiler Service.
initialise :: Rules
           -> Logger
           -> Debouncer LSP.NormalizedUri
           -> IdeOptions
           -> (Handlers ->
               ((VFSHandle, LSP.ClientCapabilities, IO LSP.LspId, (LSP.FromServerMessage -> IO ())) -> IO ()) ->
                IO () )
           -> IO () -- IdeState
initialise mainRule logger debouncer options start =
    reflexOpen
        logger
        debouncer
        options
        start
        (addIdeGlobal GetIdeOptions undefined --GlobalIdeOptions options
            : (fileStoreRules undefined
            ++ ofInterestRules -- In a global dynamic
--            ++ fileExistsRules getLspId caps vfs
            ++ mainRule))
{-
writeProfile :: IdeState -> FilePath -> IO ()
writeProfile = shakeProfile

-- | Shutdown the Compiler Service.
shutdown :: IdeState -> IO ()
shutdown = shakeShut

-- This will return as soon as the result of the action is
-- available.  There might still be other rules running at this point,
-- e.g., the ofInterestRule.
runAction :: IdeState -> Action a -> IO a
runAction ide action = do
    bar <- newBarrier
    res <- shakeRun ide [do v <- action; liftIO $ signalBarrier bar v; return v]
    -- shakeRun might throw an exception (either through action or a default rule),
    -- in which case action may not complete successfully, and signalBarrier might not be called.
    -- Therefore we wait for either res (which propagates the exception) or the barrier.
    -- Importantly, if the barrier does finish, cancelling res only kills waiting for the result,
    -- it doesn't kill the actual work
    fmap fromEither $ race (head <$> res) $ waitBarrier bar


-- | `runActionSync` is similar to `runAction` but it will
-- wait for all rules (so in particular the `ofInterestRule`) to
-- finish running. This is mainly useful in tests, where you want
-- to wait for all rules to fire so you can check diagnostics.
runActionSync :: IdeState -> Action a -> IO a
runActionSync s act = fmap head $ join $ shakeRun s [act]

getIdeOptions :: Action IdeOptions
getIdeOptions = do
    GlobalIdeOptions x <- getIdeGlobalAction
    return x
    -}
