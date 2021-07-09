{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Std.Cat.Arrow where

import "base" Data.Coerce qualified as Base
import "base" Control.Arrow qualified as Base

import "this" Std.Type
import "this" Std.Cat.Class
import "this" Std.Cat.Iso
import "this" Std.Cat.Op


class CatArrow (cat :: k -> k -> Type) (f :: k0 -> k0 -> k)  (g :: k0 -> k0 -> k) where
    catArr :: (a `f` b) `cat` (a `g` b)

type Arrow = CatArrow HASK

arr :: Arrow f g => a `f` b -> a `g` b
arr = catArr

arrow :: (CatArrow cat f g, CatArrow cat g f) => Iso cat (a `f` b) (a `g` b)
arrow = catArr :<-> catArr

instance CatArrow HASK (Iso cat) cat where catArr = to
instance CatArrow HASK (Iso cat) (Op cat) where catArr = Op . from

instance Base.Arrow cat => CatArrow HASK HASK (Basic2 cat) where
    catArr :: forall a b. (a -> b) -> Basic2 cat a b
    catArr = Base.coerce (Base.arr :: (a -> b) -> cat a b)

deriving via (Basic2 HASK) instance CatArrow HASK HASK HASK
