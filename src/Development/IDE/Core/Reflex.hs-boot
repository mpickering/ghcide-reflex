{-# LANGUAGE RoleAnnotations #-}
module Development.IDE.Core.Reflex where

type role ForallAction nominal
type role ForallDynamic nominal
data ForallAction a
data ForallDynamic a
