{-# LANGUAGE RoleAnnotations #-}
module Development.IDE.Core.Reflex.Rules where

type role ForallAction nominal
type role ForallDynamic nominal
data ForallAction a
data ForallDynamic a
