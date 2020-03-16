{-# LANGUAGE PartialTypeSignatures #-}
module Development.IDE.Core.Reflex.Early where

import qualified Data.ByteString.Char8 as BS
import Reflex
import Control.Monad.Fix

-- Early cut off
data Early a = Early (Maybe BS.ByteString) Int a

unearly :: Early a -> a
unearly (Early _ _ a) = a

early :: (Reflex t, MonadHold t m, MonadFix m) => Dynamic t (Maybe BS.ByteString, a) -> m (Dynamic t (Early a))
early d = scanDynMaybe (\(h, v) -> Early h 0 v) upd d
  where
    -- Nothing means there's no hash, so always update
    upd (Nothing, a) (Early _ n _) = Just (Early Nothing (n + 1) a)
    -- If there's already a hash, and we get a new hash then update
    upd (Just h, new_a) (Early (Just h') n _) = if h == h'
                                                  then Nothing
                                                  else (Just (Early (Just h') (n + 1) new_a))
    -- No stored, hash, just update
    upd (h, new_a) (Early Nothing n _)   = Just (Early h (n + 1) new_a)

showEarly :: (a -> String) -> Early a -> String
showEarly f (Early h n a) = "(" ++
                              "(" ++
                               show n ++
                               ")" ++ show h ++ "," ++ f a ++ ")"
