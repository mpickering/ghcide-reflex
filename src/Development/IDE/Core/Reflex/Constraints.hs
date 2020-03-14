{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
-- A module for different constraint aliases
module Development.IDE.Core.Reflex.Constraints where

import Reflex
import Control.Monad.Ref
import Control.Monad.IO.Class
import Reflex.Host.Class

type C t = (Monad (HostFrame t), MonadIO (HostFrame t), Ref (HostFrame t) ~ Ref IO
           , MonadRef (HostFrame t), Reflex t, MonadSample t (HostFrame t))
