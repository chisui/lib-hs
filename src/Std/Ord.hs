{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Ord where

import "base" GHC.Generics
import "base" Prelude qualified as Base
import "base" Data.Bool ( Bool(..), not, (||) )
import "base" Data.Ord ( Ordering(..) )

import "this" Std.Partial
import "this" Std.Cat
import "this" Std.Basic
import "this" Std.Generic


class Eq (t :: Totallity) a | a -> t where
    (==?), (/=?) :: a -> a -> Res t Bool
    {-# MINIMAL (==?) | (/=?) #-}
    a ==? b = not <$> (a /=? b)
    a /=? b = not <$> (a ==? b)

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
    (==) = coerce ((==) @a)
    (/=) = coerce ((/=) @a)
instance GThrough GEq a => Eq 'Total (Generically a) where
    Generically a ==? Generically b = toRep a `gEq` toRep b
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

class Eq t a => Ord (t :: Totallity) a | a -> t where
    compare'                 :: a -> a -> Res t Ordering
    (<?), (<=?), (>?), (>=?) :: a -> a -> Res t Bool
    max', min'               :: a -> a -> Res t a

    compare' x y = if fromRes False (x ==? y) then pure EQ
                  -- NB: must be '<=' not '<' to validate the
                  -- above claim about the minimal things that
                  -- can be defined for an instance of Ord:
                  else map (\ a -> if a then LT else GT) (x <=? y)

    x <?  y = (== LT) <$> compare' x y
    x <=? y = (/= GT) <$> compare' x y
    x >?  y = (== GT) <$> compare' x y
    x >=? y = (/= LT) <$> compare' x y

        -- These two default methods use '<=' rather than 'compare'
        -- because the latter is often more expensive
    max' x y = (\b -> if b then x else y) <$> (x <=? y)
    min' x y = (\b -> if b then y else x) <$> (x <=? y)
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
    compare = coerce (compare @a)
    (<)  = coerce ((<)  @a)
    (<=) = coerce ((<=) @a)
    (>)  = coerce ((>)  @a)
    (>=) = coerce ((>=) @a)
    max  = coerce (max @a)
    min  = coerce (min @a)


deriving via (Basic Ordering) instance Eq  'Total Ordering
deriving via (Basic Ordering) instance Ord 'Total Ordering
deriving via (Basic Base.Int) instance Eq  'Total Base.Int
deriving via (Basic Base.Int) instance Ord 'Total Base.Int
