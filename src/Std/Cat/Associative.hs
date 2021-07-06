module Std.Cat.Associative where

import "base" Data.Either
import "base" Data.Kind

import "this" Std.Cat.Class
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Cocartesian


class Category cat => CatAssociative (cat :: k1 -> k1 -> Type) (f :: k0 -> k0 -> k1) where
    assoc :: (a `f` b) `cat` (b `f` a)

instance CatAssociative HASK (,) where
    assoc = assocProd
instance CatAssociative HASK Either where
    assoc = assocCoprod
