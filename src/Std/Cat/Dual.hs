{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Dual where

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.Associative
import "this" Std.Cat.Bifunctor


newtype Dual cat a b = Dual
    { unDual :: b `cat` a
    }

instance Semigroupoid cat => Semigroupoid (Dual cat) where
    Dual g . Dual f = Dual (f . g)
instance CatId cat => CatId (Dual cat) where
    id = Dual id
instance Category cat => Category (Dual cat)

pam :: CatFunctor (Dual c0) c1 f => b `c0` a -> f a `c1` f b
pam = map . Dual


instance CatLeftFunctor c c0 c1 f => CatLeftFunctor c (Dual c0) (Dual c1) f where
    left (Dual f) = Dual (left f)
instance CatRightFunctor c c0 c1 f => CatRightFunctor c (Dual c0) (Dual c1) f where
    right (Dual f) = Dual (right f)
instance CatBifunctor c c0 c1 c2 f => CatBifunctor c (Dual c0) (Dual c1) (Dual c2) f where
    catBimap (Dual f) (Dual g) = Dual (catBimap f g)

instance Cocartesian cat => Cartesian (Dual cat) where
    type Product (Dual cat) = Coproduct cat
    fst = Dual lft
    snd = Dual rght
    copy = Dual fuse
    Dual f &&& Dual g = Dual (f ||| g)

instance Cartesian cat => Cocartesian (Dual cat) where
    type Coproduct (Dual cat) = Product cat
    lft = Dual fst
    rght = Dual snd
    fuse = Dual copy
    Dual f ||| Dual g = Dual (f &&& g)


instance CatAssociative cat f => CatAssociative (Dual cat) f where
    assoc = Dual assoc
