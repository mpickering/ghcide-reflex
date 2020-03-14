{-# LANGUAGE FlexibleContexts #-}
module Development.IDE.Core.Reflex.Logger where

import Development.IDE.Core.Reflex.Rules
import Development.IDE.Types.Logger
import Reflex
import qualified Data.Text as T
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad
import Control.Monad.IO.Class

{- Logging -}

logM :: (MonadReader (REnv t) m, MonadIO m) => Priority -> T.Text -> m ()
logM p s = do
  logger <- asks (ideLogger . global)
  liftIO $ void $ forkIO $ logPriority logger p s

logEvent :: (PerformEvent t m
            , MonadReader (REnv t) (Performable m)
            , MonadIO (Performable m)) => Event t (Priority, T.Text) -> m ()
logEvent e = do
  performEvent_ (uncurry logM <$> e)


logEventInfo :: (PerformEvent t m
                , MonadReader (REnv t) (Performable m)
                , MonadIO (Performable m)) => Event t T.Text -> m ()
logEventInfo e = logEvent (fmap (Info,) e)

logAction :: (PerformEvent t m
              , MonadReader (REnv t) (Performable m), MonadIO (Performable m))
            => Priority -> Event t (a, T.Text) -> m (Event t a)
logAction p e = do
  let (a, l) = splitE e
  logEvent (fmap (p,) l)
  return a
