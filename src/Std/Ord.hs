{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Ord
    ( module Eq
    , Ord'(..), Ord, PartialOrd, compare, (<), (<=), (>), (>=)
    , Res(..), Totallity(..), Bool(..)
    , Ordered(..), Unordered(..)
    , Ordering(..)
    ) where

import "base" Data.Eq as Eq ( Eq(..) )
import "base" Prelude qualified as Base
import "base" Data.Bool ( Bool(..), bool )
import "base" Data.Ord ( Ordering(..) )
import "base" Data.Semigroup qualified as Base

import "this" Std.Partial
import "this" Std.IfThenElse
import "this" Std.Generic
import "this" Std.Cat
import "this" Std.Basic


class Eq a => Ord' (t :: Totallity) a | a -> t where
    compare'                 :: a -> a -> Res t Ordering
    (<?), (<=?), (>?), (>=?) :: a -> a -> Res t Bool

    compare' x y = if x == y then pure EQ
                  -- NB: must be '<=' not '<' to validate the
                  -- above claim about the minimal things that
                  -- can be defined for an instance of Ord:
                  else bool LT GT <$> (x <=? y)

    x <?  y = (== LT) <$> compare' x y
    x <=? y = (/= GT) <$> compare' x y
    x >?  y = (== GT) <$> compare' x y
    x >=? y = (/= LT) <$> compare' x y
    {-# MINIMAL compare' | (<=?) #-}
type Ord = Ord' 'Total
type PartialOrd = Ord' 'Partial

compare              :: Ord' 'Total a => a -> a -> Ordering
(<), (<=), (>), (>=) :: Ord' 'Total a => a -> a -> Bool
compare = total2 compare'
(<)  = total2 (<?)
(<=) = total2 (<=?)
(>)  = total2 (>?)
(>=) = total2 (>=?)

newtype Ordered a = Ordered a
  deriving newtype Eq
deriving newtype instance Ord' t a => Ord' t (Ordered a)

newtype Unordered a = Unordered a
  deriving newtype Eq
instance Eq a => Ord' 'Partial (Unordered a) where
    (<=?) a b = if a == b then pure True else empty
    (>=?) a b = if a == b then pure True else empty
    (<?)     = undefined
    (>?)     = undefined

instance Base.Ord a => Ord' 'Total (Basic a) where
    compare' = liftTotal2 (Base.compare @a)
    (<?)  = liftTotal2 ((Base.<)  @a)
    (<=?) = liftTotal2 ((Base.<=) @a)
    (>?)  = liftTotal2 ((Base.>)  @a)
    (>=?) = liftTotal2 ((Base.>=) @a)
instance Base.Ord a => Ord' 'Partial (Unsafe a) where
    compare' = errorToPartial2 (Base.compare @a)
    (<?)  = errorToPartial2 ((Base.<)  @a)
    (<=?) = errorToPartial2 ((Base.<=) @a)
    (>?)  = errorToPartial2 ((Base.>)  @a)
    (>=?) = errorToPartial2 ((Base.>=) @a)
instance Ord' 'Total a => Base.Ord (Basic a) where
    compare = to coerce (compare @a)
    (<)  = to coerce ((<)  @a)
    (<=) = to coerce ((<=) @a)
    (>)  = to coerce ((>)  @a)
    (>=) = to coerce ((>=) @a)

instance (Eq a, GThrough (GOrd t) a) => Ord' t (Generically a) where
    compare' (Generically a) (Generically b) = gcompare (to rep a) (to rep b)
class GOrd t a | a -> t where
    gcompare :: a x -> a x -> Res t Ordering
instance GOrd t f => GOrd t (M1 m n f) where
    gcompare :: forall x. M1 m n f x -> M1 m n f x -> Res t Ordering
    gcompare = to coerce (gcompare :: f x -> f x -> Res t Ordering)
instance (GOrd tf f, GOrd tg g, MinTotallity tf tg ~ t) => GOrd t (f :+: g) where
    gcompare (L1 a) (L1 b) = joinRes (pure @(Res tg) <$> gcompare a b)
    gcompare (R1 a) (R1 b) = joinRes . pure @(Res tf) $ gcompare a b
    gcompare (L1 _) _      = pure GT
    gcompare _      _      = pure LT
instance (GOrd tf f, GOrd tg g, MinTotallity tf tg ~ t) => GOrd t (f :*: g) where
    gcompare (a :*: b) (a' :*: b') = zipRes (Base.<>) (gcompare a a') (gcompare b b')
instance Ord' t a => GOrd t (K1 i a) where
    gcompare (K1 a) (K1 b) = compare' a b
instance GOrd 'Total U1 where
    gcompare _ _ = pure EQ
instance GOrd 'Total V1 where
    gcompare = absurdV1


deriving via (Basic Ordering)     instance Ord' 'Total Ordering
deriving via (Basic Base.Int)     instance Ord' 'Total Base.Int
deriving via (Basic Base.Char)    instance Ord' 'Total Base.Char
deriving via (Basic Base.Integer) instance Ord' 'Total Base.Integer
deriving via (Basic Base.Bool)    instance Ord' 'Total Base.Bool

instance (MinTotallity t t ~ t, Ord' t a) => Ord' t [a] where
    (a : as) `compare'` (b : bs) = zipRes @t @t (Base.<>) (a `compare'` b) (as `compare'` bs)
    [] `compare'` [] = pure EQ
    [] `compare'` _  = pure LT
    _  `compare'` _  = pure GT

instance (Ord' u a, Ord' v b, t ~ MinTotallity u v) => Ord' t (Base.Either a b) where
    compare' (Base.Left  a) (Base.Left  b) = joinRes @u @v (pure <$> compare' a b)
    compare' (Base.Right a) (Base.Right b) = joinRes @u @v (pure  $  compare' a b)
    compare' (Base.Left  _) _ = pure GT
    compare' _ _ = pure LT

deriving newtype instance Ord' t a => Ord' t (Base.Min a)
deriving newtype instance Ord' t a => Ord' t (Base.Max a)
deriving newtype instance Ord' t a => Ord' t (Base.Sum a)
deriving newtype instance Ord' t a => Ord' t (Base.Product a)
