{-# LANGUAGE TupleSections #-}
module Std.Cat.Monoidal
    ( CatMonoidal(..)
    ) where

import "base" Data.Kind
import "base" Data.Either
import "base" Data.Void
import "base" Data.Functor.Const

import "this" Std.Cat.Class
import "this" Std.Cat.Iso
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.NaturalTransformation
import "this" Std.Cat.Commutative
import "this" Std.Cat.Op
import "this" Std.Cat.Product


class CatCommutative cat f => CatMonoidal (cat :: k -> k -> Type) (f :: k -> k -> k) where
    type Id cat f :: k
    idl :: Iso cat (Id cat f `f` a) a
    idl = (to idr . commute) :<-> (commute . from idr)
    idr :: Iso cat (a `f` Id cat f) a
    idr = (to idl . commute) :<-> (commute . from idl)
    {-# MINIMAL idl | idr #-}

instance CatMonoidal cat f => CatMonoidal (Op cat) f where
    type Id (Op cat) f = Id cat f
    idl = Op (from idl) :<-> Op (to idl)

instance CatMonoidal cat f => CatMonoidal (Iso cat) f where
    type Id (Iso cat) f = Id cat f
    idl = idl :<-> commute idl

instance (LeftFunctor f, CatMonoidal HASK f) => CatMonoidal (~>) (Prod1 f) where
    type Id (~>) (Prod1 f) = Const (Id HASK f)
    idl = NT (to idl . left getConst . unProd1)
     :<-> NT (Prod1 . left Const . from idl)

instance CatMonoidal HASK (,) where
    type Id HASK (,) = ()
    idl = snd :<-> ((),)

instance CatMonoidal HASK Either where
    type Id HASK Either = Void
    idl = (absurd ||| id) :<-> Right

