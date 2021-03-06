module Std.Cat.Commutative where

import "base" Data.Either
import "base" Data.Kind

import "this" Std.Cat.Class
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.Product
import "this" Std.Cat.NaturalTransformation


class Category cat => CatCommutative (cat :: k1 -> k1 -> Type) (f :: k0 -> k0 -> k1) where
    commute :: (a `f` b) `cat` (b `f` a)
type Commutative = CatCommutative HASK

swap :: CatCommutative cat f => (a `f` b) `cat` (b `f` a)
swap = commute


commuteProd :: Cartesian cat => Product cat a b `cat` Product cat b a
commuteProd = snd &&& fst

commuteCoprod :: Cocartesian cat => Coproduct cat a b `cat` Coproduct cat b a
commuteCoprod = rght ||| lft

instance                          CatCommutative HASK     (,)       where commute = commuteProd
instance                          CatCommutative HASK     Either    where commute = commuteCoprod
instance CatCommutative HASK f => CatCommutative (~>)     (Prod1 f) where commute = NT (liftProd1 commute)
