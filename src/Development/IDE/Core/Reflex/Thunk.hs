{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Development.IDE.Core.Reflex.Thunk where

import Reflex
import Control.Monad.IO.Class
import Control.Monad.Fix

-- Like a Maybe, but has to be activated the first time we try to access it
data Thunk a = Value a | Awaiting | Seed (IO ()) deriving Functor

joinThunk :: Thunk (Maybe a) -> Thunk a
joinThunk (Value m) = maybe Awaiting Value m
joinThunk Awaiting = Awaiting
joinThunk (Seed trig) = (Seed trig)

splitThunk :: a -> Thunk (a, b)  -> (a, Thunk b)
splitThunk _ (Value (l, a)) = (l, Value a)
splitThunk a (Awaiting)  = (a, Awaiting)
splitThunk a (Seed trig) = (a, Seed trig)

thunk :: (TriggerEvent t m
         , Reflex t
         , MonadHold t m
         , MonadIO m
         , MonadFix m) => Event t (Maybe a) -> m (Dynamic t (Thunk a), Event t ())
thunk e  = do
  (start, grow) <- newTriggerEvent
  -- This headE is very important.
  -- If you remove it then at the star of the program the event gets
  -- triggered many times which leads to a lot of computation happening.
  -- It used to be "batchOccurences" which worked somewhat but not on
  -- a bigger code base like GHC. It is best to enforce that the start
  -- event only fires once using headE.
  start' <- headE start
  let trig = grow Awaiting
  d <- holdDyn (Seed trig) (leftmost [maybe Awaiting Value <$> e
                                                , Awaiting <$ start' ])
  -- Only allow the thunk to improve
  d' <- improvingResetableThunk d
  return (d', () <$ start')

forceThunk :: (Reflex t, MonadIO m, MonadSample t m) => Dynamic t (Thunk a) -> m ()
forceThunk d = do
  t <- sample (current d)
  case t of
    Seed start -> liftIO start
    _ -> return ()

sampleThunk :: (Reflex t, MonadIO m, MonadSample t m) => Dynamic t (Thunk a) -> m (Maybe a)
sampleThunk d = do
  t <- sample (current d)
  case t of
    Seed start -> liftIO start >> return Nothing
    Awaiting   -> return Nothing
    Value a    -> return (Just a)


-- Like improvingMaybe, but for the Thunk type
improvingResetableThunk  ::  (MonadFix m, MonadHold t m, Reflex t, MonadIO m, MonadSample t m) => Dynamic t (Thunk a) -> m (Dynamic t (Thunk a))
improvingResetableThunk = scanDynMaybe id upd
  where
    -- ok, if you insist, write the new value
    upd (Value a) _ = --trace "UPDATING: New Value" $
                      Just (Value a)
    -- Wait, once the trigger is pressed
    upd Awaiting  (Seed {}) = --trace "UPDATING: Awaiting" $
                              Just Awaiting
    -- Allows the thunk to be reset to GC
--    upd s@(Seed {}) _ = Just s
    -- NOPE
    upd _ _ = Nothing

updatedThunk :: Reflex t => Dynamic t (Thunk a) -> Event t (Thunk a)
updatedThunk  = ffilter (\a -> case a of
                          Value {} -> True
                          _ -> False ) . updated
