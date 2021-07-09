{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Ord
    ( module Eq
    , Ord'(..), Ord, PartialOrd, compare, (<), (<=), (>), (>=), max, min
    , Res(..), Totallity(..), Bool(..)
    , Unordered(..)
    , Ordering(..)
    ) where

import "base" Data.Eq as Eq ( Eq(..) )
import "base" Prelude qualified as Base
import "base" Data.Bool ( Bool(..), bool )
import "base" Data.Ord ( Ordering(..) )

import "this" Std.Partial
import "this" Std.IfThenElse
import "this" Std.Cat
import "this" Std.Basic


class Eq a => Ord' (t :: Totallity) a | a -> t where
    compare'                 :: a -> a -> Res t Ordering
    (<?), (<=?), (>?), (>=?) :: a -> a -> Res t Bool
    max', min'               :: a -> a -> Res t a

    compare' x y = if x == y then pure EQ
                  -- NB: must be '<=' not '<' to validate the
                  -- above claim about the minimal things that
                  -- can be defined for an instance of Ord:
                  else bool LT GT <$> (x <=? y)

    x <?  y = (== LT) <$> compare' x y
    x <=? y = (/= GT) <$> compare' x y
    x >?  y = (== GT) <$> compare' x y
    x >=? y = (/= LT) <$> compare' x y

        -- These two default methods use '<=' rather than 'compare'
        -- because the latter is often more expensive
    max' x y = bool x y <$> (x <=? y)
    min' x y = bool y x <$> (x <=? y)
    {-# MINIMAL compare' | (<=?) #-}
type Ord = Ord' 'Total
type PartialOrd = Ord' 'Partial

compare              :: Ord' 'Total a => a -> a -> Ordering
(<), (<=), (>), (>=) :: Ord' 'Total a => a -> a -> Bool
max, min             :: Ord' 'Total a => a -> a -> a
compare = total2 compare'
(<)  = total2 (<?)
(<=) = total2 (<=?)
(>)  = total2 (>?)
(>=) = total2 (>=?)
max  = total2 max'
min  = total2 min'


newtype Unordered a = Unordered a
  deriving newtype Eq
instance Eq a => Ord' 'Partial (Unordered a) where
    compare' = undefined
    (<?)     = undefined
    (<=?)    = undefined
    (>?)     = undefined
    (>=?)    = undefined
    max'     = undefined
    min'     = undefined

instance Base.Ord a => Ord' 'Total (Basic a) where
    compare' = liftTotal2 (Base.compare @a)
    (<?)  = liftTotal2 ((Base.<)  @a)
    (<=?) = liftTotal2 ((Base.<=) @a)
    (>?)  = liftTotal2 ((Base.>)  @a)
    (>=?) = liftTotal2 ((Base.>=) @a)
    max'  = liftTotal2 (Base.max @a)
    min'  = liftTotal2 (Base.min @a)
instance Base.Ord a => Ord' 'Partial (Unsafe a) where
    compare' = errorToPartial2 (Base.compare @a)
    (<?)  = errorToPartial2 ((Base.<)  @a)
    (<=?) = errorToPartial2 ((Base.<=) @a)
    (>?)  = errorToPartial2 ((Base.>)  @a)
    (>=?) = errorToPartial2 ((Base.>=) @a)
    max'  = errorToPartial2 (Base.max @a)
    min'  = errorToPartial2 (Base.min @a)
instance Ord' 'Total a => Base.Ord (Basic a) where
    compare = to coerce (compare @a)
    (<)  = to coerce ((<)  @a)
    (<=) = to coerce ((<=) @a)
    (>)  = to coerce ((>)  @a)
    (>=) = to coerce ((>=) @a)
    max  = to coerce (max @a)
    min  = to coerce (min @a)


deriving via (Basic Ordering) instance Ord' 'Total Ordering
deriving via (Basic Base.Int) instance Ord' 'Total Base.Int
deriving via (Basic Base.Char) instance Ord' 'Total Base.Char
deriving via (Basic Base.Integer) instance Ord' 'Total Base.Integer

instance (Min t t ~ t, Ord' t a) => Ord' t [a] where
    (a : as) `compare'` (b : bs) = zipRes @t @t (Base.<>) (a `compare'` b) (as `compare'` bs)
    [] `compare'` [] = pure EQ
    [] `compare'` _  = pure LT
    _  `compare'` _  = pure GT

instance (Ord' u a, Ord' v b, t ~ Min u v) => Ord' t (Base.Either a b) where
    compare' (Base.Left  a) (Base.Left  b) = joinRes @u @v (pure <$> compare' a b)
    compare' (Base.Right a) (Base.Right b) = joinRes @u @v (pure  $  compare' a b)
    compare' (Base.Left  _) _ = pure GT
    compare' _ _ = pure LT
--deriving via (Basic Base.Int) instance Ord' 'Total Base.Int
