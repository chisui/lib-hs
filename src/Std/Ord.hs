{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Ord
    ( Eq(..), (==), (==!), (/=)
    , Ord(..), compare, (<), (<=), (>), (>=), max, min
    , Res(..), Totallity(..), Bool(..)
    , Unordered(..)
    , Ordering(..)
    ) where

import "base" Prelude qualified as Base
import "base" Data.Bool ( Bool(..), not, (||), bool, (&&) )
import "base" Data.Ord ( Ordering(..) )

import "this" Std.Partial
import "this" Std.IfThenElse
import "this" Std.Cat
import "this" Std.Basic
import "this" Std.Generic


class Eq (t :: Totallity) a | a -> t where
    (==?), (/=?) :: a -> a -> Res t Bool
    {-# MINIMAL (==?) | (/=?) #-}
    a ==? b = not <$> (a /=? b)
    a /=? b = not <$> (a ==? b)

(==!) :: forall a t. Eq t a => a -> a -> Bool
a ==! b = fromRes False (a ==? b)
(==), (/=) :: forall a. Eq 'Total a => a -> a -> Bool
a == b = total (a ==? b)
a /= b = total (a /=? b)

instance Base.Eq a => Eq 'Total (Basic a) where
    (==?) = liftTotal2 ((Base.==) @a)
    (/=?) = liftTotal2 ((Base./=) @a)
instance Base.Eq a => Eq 'Partial (Unsafe a) where
    (==?) = errorToPartial2 ((Base.==) @a)
    (/=?) = errorToPartial2 ((Base./=) @a)
instance Eq 'Total a => Base.Eq (Basic a) where
    (==) = to coerce ((==) @a)
    (/=) = to coerce ((/=) @a)
instance GThrough GEq a => Eq 'Total (Generically a) where
    Generically a ==? Generically b = from rep a `gEq` from rep b

class GEq f where
    gEq :: f x -> f x -> Res 'Total Bool
instance GEq f => GEq (M1 i c f) where
    M1 a `gEq` M1 b = a `gEq` b
instance Eq 'Total c => GEq (K1 i c) where
    K1 a `gEq` K1 b = a ==? b
instance (GEq f, GEq g) => GEq (f :+: g) where
    L1 a `gEq` L1 b = a `gEq` b
    R1 a `gEq` R1 b = a `gEq` b
    _ `gEq` _ = pure False
instance (GEq f, GEq g) => GEq (f :*: g) where
    (a0 :*: b0) `gEq` (a1 :*: b1) = lift2 (||) (gEq a0 a1) (gEq b0 b1)
instance GEq U1 where
    _ `gEq` _ = pure True

instance Eq t (f x) => Eq t (M1 i c f x) where
    (==?) = to coerce ((==?) :: f x -> f x -> Res t Bool)
    (/=?) = to coerce ((/=?) :: f x -> f x -> Res t Bool)
instance Eq t a => Eq t (K1 i a x) where
    (==?) = to coerce ((==?) :: a -> a -> Res t Bool)
    (/=?) = to coerce ((/=?) :: a -> a -> Res t Bool)
instance (Eq u (f x), Eq v (g x), t ~ Min u v) => Eq t ((f :+: g) x) where
    L1 a ==? L1 b = joinRes @u @v (pure <$> (a ==? b))
    R1 a ==? R1 b = joinRes @u @v $ pure (a ==? b)
    _ ==? _ = pure False
instance (Eq u (f x), Eq v (g x), t ~ Min u v) => Eq t ((f :*: g) x) where
    (a :*: b) ==? (a' :*: b') = zipRes (&&) (a ==? a') (b ==? b')
instance Eq 'Total (U1 x) where
    _ ==? _ = pure True
    _ /=? _ = pure False
instance Eq 'Total (V1 x) where
    (==?) = absurdV1
    (/=?) = absurdV1

class Eq t a => Ord (t :: Totallity) a | a -> t where
    compare'                 :: a -> a -> Res t Ordering
    (<?), (<=?), (>?), (>=?) :: a -> a -> Res t Bool
    max', min'               :: a -> a -> Res t a

    compare' x y = if x ==! y then pure EQ
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

compare              :: Ord 'Total a => a -> a -> Ordering
(<), (<=), (>), (>=) :: Ord 'Total a => a -> a -> Bool
max, min             :: Ord 'Total a => a -> a -> a
compare = total2 compare'
(<)  = total2 (<?)
(<=) = total2 (<=?)
(>)  = total2 (>?)
(>=) = total2 (>=?)
max  = total2 max'
min  = total2 min'


newtype Unordered a = Unordered a
instance Eq 'Partial (Unordered a) where
    (==?) = undefined
    (/=?) = undefined
instance Ord 'Partial (Unordered a) where
    compare' = undefined
    (<?)     = undefined
    (<=?)    = undefined
    (>?)     = undefined
    (>=?)    = undefined
    max'     = undefined
    min'     = undefined

instance Base.Ord a => Ord 'Total (Basic a) where
    compare' = liftTotal2 (Base.compare @a)
    (<?)  = liftTotal2 ((Base.<)  @a)
    (<=?) = liftTotal2 ((Base.<=) @a)
    (>?)  = liftTotal2 ((Base.>)  @a)
    (>=?) = liftTotal2 ((Base.>=) @a)
    max'  = liftTotal2 (Base.max @a)
    min'  = liftTotal2 (Base.min @a)
instance Base.Ord a => Ord 'Partial (Unsafe a) where
    compare' = errorToPartial2 (Base.compare @a)
    (<?)  = errorToPartial2 ((Base.<)  @a)
    (<=?) = errorToPartial2 ((Base.<=) @a)
    (>?)  = errorToPartial2 ((Base.>)  @a)
    (>=?) = errorToPartial2 ((Base.>=) @a)
    max'  = errorToPartial2 (Base.max @a)
    min'  = errorToPartial2 (Base.min @a)
instance Ord 'Total a => Base.Ord (Basic a) where
    compare = to coerce (compare @a)
    (<)  = to coerce ((<)  @a)
    (<=) = to coerce ((<=) @a)
    (>)  = to coerce ((>)  @a)
    (>=) = to coerce ((>=) @a)
    max  = to coerce (max @a)
    min  = to coerce (min @a)


deriving via (Basic Ordering) instance Eq  'Total Ordering
deriving via (Basic Ordering) instance Ord 'Total Ordering
deriving via (Basic Base.Int) instance Eq  'Total Base.Int
deriving via (Basic Base.Int) instance Ord 'Total Base.Int
deriving via (Basic Base.Char) instance Eq  'Total Base.Char
deriving via (Basic Base.Char) instance Ord 'Total Base.Char
deriving via (Basic Base.Integer) instance Eq  'Total Base.Integer
deriving via (Basic Base.Integer) instance Ord 'Total Base.Integer

instance (Min t t ~ t, Eq t a) => Eq t [a] where
    [] ==? [] = pure True
    (a : as) ==? (b : bs) = zipRes @t @t (&&) (a ==? b) (as ==? bs)
    _ ==? _ = pure False

instance (Min t t ~ t, Ord t a) => Ord t [a] where
    (a : as) `compare'` (b : bs) = zipRes @t @t (Base.<>) (a `compare'` b) (as `compare'` bs)
    [] `compare'` [] = pure EQ
    [] `compare'` _  = pure LT
    _  `compare'` _  = pure GT

instance (Eq u a, Eq v b, t ~ Min u v) => Eq t (Base.Either a b) where
    Base.Left  a ==? Base.Left  b = joinRes @u @v (pure <$> (a ==? b))
    Base.Right a ==? Base.Right b = joinRes @u @v (pure (a ==? b))
    _ ==? _ = pure False
instance (Ord u a, Ord v b, t ~ Min u v) => Ord t (Base.Either a b) where
    compare' (Base.Left  a) (Base.Left  b) = joinRes @u @v (pure <$> compare' a b)
    compare' (Base.Right a) (Base.Right b) = joinRes @u @v (pure  $  compare' a b)
    compare' (Base.Left  _) _ = pure GT
    compare' _ _ = pure LT
--deriving via (Basic Base.Int) instance Ord 'Total Base.Int
