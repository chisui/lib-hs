{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Kleisli where

import "base" Data.Void

import "this" Std.Type
import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.Applicative
import "this" Std.Cat.Monad
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Closed
import "this" Std.Cat.Limit
import "this" Std.Cat.Monoidal
import "this" Std.Cat.Distributive
import "this" Std.Cat.Associative
import "this" Std.Cat.Op
import "this" Std.Cat.Arrow
import "this" Std.Cat.Iso


newtype CatKleisli cat m a b = Kleisli
    { unKleisli :: a `cat` m b
    }
type Kleisli = CatKleisli HASK
type CatCokleisli cat = CatKleisli (Op cat)
type Cokleisli = CatCokleisli HASK

instance CatIsomorphic HASK (CatKleisli cat m a b) (a `cat` m b) where catIso = coerce

liftKleisli :: (a `cat` m b -> a' `cat'` m' b') -> CatKleisli cat m a b -> CatKleisli cat' m' a' b'
liftKleisli f = Kleisli . f . unKleisli


instance CatMonad cat m => CatArrow HASK cat (CatKleisli cat m) where
    catArr f = Kleisli (catPure . f)

instance (CatMonad cat m, CatMonoidalClosed cat) => Semigroupoid' Unconstrained (CatKleisli cat m) where
    (.) :: forall a b c. CatKleisli cat m b c -> CatKleisli cat m a b -> CatKleisli cat m a c
    (.) = to coerce ((<=<) :: b `cat` m c -> a `cat` m b -> a `cat` m c)
instance (CatPure cat m, CatId cat) => CatId' Unconstrained (CatKleisli cat m) where
    id :: forall a. CatKleisli cat m a a
    id = to coerce (catPure :: a `cat` m a)
instance (CatMonad cat m, CatMonoidalClosed cat) => Category' Unconstrained (CatKleisli cat m)


instance ( CatMonad cat m
         , CatMonoidalClosed cat
         , EndoBifunctor (CatKleisli cat m) (Product cat)
         ) => Cartesian (CatKleisli cat m) where
    type Product (CatKleisli cat m) = Product cat
    fst      = arr @cat fst
    snd      = arr @cat snd
    diagonal = arr @cat diagonal

instance ( CatMonad cat m
         , Cocartesian cat
         , EndoBifunctor (CatKleisli cat m) (Coproduct cat)
         ) => Cocartesian (CatKleisli cat m) where
    type Coproduct (CatKleisli cat m) = Coproduct cat
    lft        = arr @cat lft
    rght       = arr @cat rght
    codiagonal = arr @cat codiagonal

instance (Monad m, Distributive m) => Closed (Kleisli m) where
    type Exp (Kleisli m) = Kleisli m
    apply   = Kleisli (uncurry unKleisli)
    curry   = liftKleisli $ \f -> catPure . Kleisli . curry f
    uncurry = liftKleisli $ \f (a, b) -> ($ b) . unKleisli =<< f a

instance Monad m => CatTerminal (Kleisli m) where
    type Terminal (Kleisli m) = Terminal HASK
    terminate = Kleisli (catPure . terminate)
instance Monad m => CatInitial (Kleisli m) where
    type Initial (Kleisli m) = Initial HASK
    initiate = Kleisli absurd

instance (Associative f, LeftFunctor' c0 c1 f, Monad m, Distributive m) => CatLeftFunctor' c0 c1 (Kleisli m) (Kleisli m) f where
    left' = liftKleisli $ \f -> map unLeft . distribute . map f . MkLeft
instance (Associative f, Monad m, Distributive m, RightFunctor' c0 c1 f) => CatRightFunctor' c0 c1 (Kleisli m) (Kleisli m) f where
    right' = liftKleisli $ \f -> map unRight . distribute . map f . MkRight
instance (Associative f, Monad m, Distributive m, Bifunctor' c0 c1 f) => CatBifunctor' c0 c1 (Kleisli m) (Kleisli m) (Kleisli m) f


instance CatMonad cat m => CatLeftFunctor' Unconstrained Unconstrained (Op cat) HASK (CatKleisli cat m) where
    left' (Op f) (Kleisli g) = Kleisli (g . f)
instance (CatMonoidalClosed cat, CatMonad cat m) => CatRightFunctor' Unconstrained Unconstrained cat HASK (CatKleisli cat m) where
    right' f (Kleisli g) = Kleisli (catPure . f <=< g)
instance (CatMonoidalClosed cat, CatMonad cat m) => CatBifunctor' Unconstrained Unconstrained (Op cat) cat HASK (CatKleisli cat m)
