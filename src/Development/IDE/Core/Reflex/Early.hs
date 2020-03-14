{-# LANGUAGE PartialTypeSignatures #-}
module Development.IDE.Core.Reflex.Early where

import qualified Data.ByteString.Char8 as BS
import Reflex

-- Early cut off
data Early a = Early (Maybe BS.ByteString) a

unearly :: Early a -> a
unearly (Early _ a) = a

early :: _ => Dynamic t (Maybe BS.ByteString, a) -> m (Dynamic t (Early a))
early d = scanDynMaybe (\(h, v) -> Early h v) upd d
  where
    -- Nothing means there's no hash, so always update
    upd (Nothing, a) v = Just (Early Nothing a)
    -- If there's already a hash, and we get a new hash then update
    upd (Just h, new_a) (Early (Just h') _) = if h == h'
                                                  then Nothing
                                                  else (Just (Early (Just h) new_a))
    -- No stored, hash, just update
    upd (h, new_a) (Early Nothing _)   = Just (Early h new_a)
