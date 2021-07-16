{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Op where

import "base" Data.Coerce

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.Limit
import "this" Std.Cat.Bifunctor


newtype Op cat a b = Op { unOp :: b `cat` a }

type family OpOf cat where
    OpOf (Op (Op cat)) = OpOf cat
    OpOf (Op cat) = cat
    OpOf cat = Op cat

liftOp :: (a `cat` b -> a' `cat'` b') -> Op cat b a -> Op cat' b' a'
liftOp = coerce

instance Semigroupoid cat => Semigroupoid (Op cat) where Op g . Op f = Op (f . g)
instance CatId        cat => CatId        (Op cat) where id = Op id
instance Category     cat => Category     (Op cat)

pam :: CatFunctor (Op c0) c1 f => b `c0` a -> f a `c1` f b
pam = catMap . Op
(>$<) :: CatFunctor (Op c0) c1 f => b `c0` a -> f a `c1` f b
(>$<) = pam


instance CatFunctor' c cat0 cat1 f => CatFunctor' c (Op cat0) (Op cat1) f where catMap (Op f) = Op (catMap f)


instance CatLeftFunctor'  c c' c0 c1    f => CatLeftFunctor'  c c' (Op c0) (Op c1) f where left'  (Op f) = Op (left' f)
instance CatRightFunctor' c c' c0 c1    f => CatRightFunctor' c c' (Op c0) (Op c1) f where right' (Op f) = Op (right' f)
instance CatBifunctor'    c c' c0 c1 c2 f => CatBifunctor'    c c' (Op c0) (Op c1) (Op c2) f where
    catBimap' (Op f) (Op g) = Op (catBimap' f g)

instance Cocartesian cat => Cartesian (Op cat) where
    type Product (Op cat) = Coproduct cat
    fst = Op lft
    snd = Op rght
    diagonal = Op codiagonal
    Op f &&& Op g = Op (f ||| g)

instance Cartesian cat => Cocartesian (Op cat) where
    type Coproduct (Op cat) = Product cat
    lft = Op fst
    rght = Op snd
    codiagonal = Op diagonal
    Op f ||| Op g = Op (f &&& g)

instance CatInitial cat => CatTerminal (Op cat) where
    type Terminal (Op cat) = Initial cat
    terminate = Op initiate
instance CatTerminal cat => CatInitial (Op cat) where
    type Initial (Op cat) = Terminal cat
    initiate = Op terminate
