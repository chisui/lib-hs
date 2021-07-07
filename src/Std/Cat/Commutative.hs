module Std.Cat.Commutative where

import "base" Data.Either
import "base" Data.Kind

import "this" Std.Cat.Class
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.Iso
import "this" Std.Cat.Dual
import "this" Std.Cat.Product
import "this" Std.Cat.NaturalTransformation


class Category cat => CatCommutative (cat :: k1 -> k1 -> Type) (f :: k0 -> k0 -> k1) where
    -- associate :: ((a `f` b) `f` c) `cat` (a `f` (b `f` c))
    commute :: (a `f` b) `cat` (b `f` a)


commuteProd :: Cartesian cat => Product cat a b `cat` Product cat b a
commuteProd = snd &&& fst

commuteCoprod :: Cocartesian cat => Coproduct cat a b `cat` Coproduct cat b a
commuteCoprod = rght ||| lft


instance                          CatCommutative HASK       (,)       where commute = commuteProd
instance                          CatCommutative HASK       Either    where commute = commuteCoprod
instance CatCommutative cat  f => CatCommutative (Dual cat) f         where commute = Dual commute
instance CatCommutative HASK f => CatCommutative (~>)       (Prod1 f) where commute = NT (liftProd1 commute)
instance CatCommutative cat  f => CatCommutative (Iso cat)  f         where commute = commute :<-> commute
instance       Category cat    => CatCommutative HASK       (Iso cat) where commute = invCat
