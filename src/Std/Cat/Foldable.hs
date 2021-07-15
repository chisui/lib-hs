{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Std.Cat.Foldable where

import "base" Data.Monoid ( First(..), Last(..) )
import "base" Data.Semigroup ( Min(..), Max(..) )
import "base" Prelude qualified as Base

import "this" Std.Type
import "this" Std.Group
import "this" Std.Ord
import "this" Std.Basic
import "this" Std.Maybe
import "this" Std.Bool
import "this" Std.Literal
import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Applicative
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Closed
import "this" Std.Cat.Endo
import "this" Std.Cat.Dual


class Functor' c f => Foldable' c f | f -> c where
    
    foldMap :: forall m a. (c a, c m, CanonicMonoid m) => (a -> m) -> f a -> m
    foldMap = foldMapSemi mempty

    foldMap# :: forall m op a. (c a, c m, Monoid op m) => Proxy# op -> (a -> m) -> f a -> m
    foldMap# p = foldMapSemi# p (identity# p)
    
    fold :: forall a.  (c a, CanonicMonoid a) => f a -> a
    fold = foldSemi mempty

    fold# :: forall op a. (c a, Monoid op a) => Proxy# op -> f a -> a
    fold# p = foldSemi# p (identity# p)
    
    foldMapSemi :: forall m a.  (c a, c m, CanonicSemigroup m) => m -> (a -> m) -> f a -> m
    foldMapSemi = foldMapSemi# (proxy# @'Canonic)

    foldMapSemi# :: forall m op a. (c a, c m, Semigroup op m) => Proxy# op -> m -> (a -> m) -> f a -> m
    foldMapSemi# p m f = foldSemi# p m . map f
    
    foldSemi :: forall a. (c a, CanonicSemigroup a) => a -> f a -> a
    foldSemi = foldSemi# (proxy# @'Canonic)

    foldSemi# :: forall op a. (c a, Semigroup op a) => Proxy# op -> a -> f a -> a
    foldSemi# p = foldr (totalOp# p)
    
    foldr :: c a => (a -> b -> b) -> b -> f a -> b
    default foldr :: (c a, c (Endo b)) => (a -> b -> b) -> b -> f a -> b
    foldr f z t = appEndo (foldMap (Endo . f) t) z
    
    foldl :: c a => (b -> a -> b) -> b -> f a -> b 
    default foldl :: (c a, c (Dual (Endo b))) => (b -> a -> b) -> b -> f a -> b 
    foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
    {-# MINIMAL foldr | foldMap #-}

    sum :: (c a, Monoid 'Add a) => f a -> a
    sum = fold# (proxy# @'Add)

    product :: (c a, Monoid 'Mult a) => f a -> a
    product = fold# (proxy# @'Mult)

    minimum, maximum :: (c a, Ord a) => a -> f a -> a
    default minimum :: (c a, c (Min a), Ord a) => a -> f a -> a 
    minimum a = getMin . foldMapSemi (Min a) Min 
    default maximum :: (c a, c (Max a), Ord a) => a -> f a -> a 
    maximum a = getMax . foldMapSemi (Max a) Max

    length :: (c a, Iterable i) => f a -> i
    length = foldr (const succ) zero

    null :: c a => f a -> Bool
    null = foldr (const (const False)) True

    head :: (c a, Alternative t) => f a -> t a
    default head :: (c a, c (First a), Alternative t) => f a -> t a
    head = unpackMaybe . getFirst . foldMap pure

    last :: (c a, Alternative t) => f a -> t a
    default last :: (c a, c (Last a), Alternative t) => f a -> t a
    last = unpackMaybe . getLast . foldMap pure

    find :: (c a, Alternative t) => (a -> Bool) -> f a -> t a
    default find :: (c a, c (First a), Alternative t) => (a -> Bool) -> f a -> t a
    find f = unpackMaybe . getFirst . foldMap (\a -> guard (f a) $> a)

    elem :: (c a, Eq a) => a -> f a -> Bool
    elem a = maybe False (const True) . find (== a)

    notElem :: (c a, Eq a) => a -> f a -> Bool
    notElem a = not . elem a

    lookup :: (Alternative t, c a, Eq a) => a -> f (a, b) -> t b
    default lookup :: (Alternative t, c a, c (a, b), Eq a) => a -> f (a, b) -> t b
    lookup a = map snd . find ((== a) . fst)

type Foldable = Foldable' Unconstrained

instance (Base.Functor f, Base.Foldable f) => Foldable' Unconstrained (Basic1 f) where
    foldMap f = getMonoidal . Base.foldMap (Monoidal . f) . getBasic1

deriving via (Basic1 []) instance Foldable' Unconstrained []
deriving via (Basic1 ((,) a)) instance Foldable' Unconstrained ((,) a)
deriving via (Basic1 Base.Maybe) instance Foldable' Unconstrained Base.Maybe
deriving via (Basic1 (Base.Either e)) instance Foldable' Unconstrained (Base.Either e)
