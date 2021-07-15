{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Foldable where

import "base" Data.Semigroup ( Min(..), Max(..) )
import "base" Data.Monoid ( First(..), Last(..) )
import "base" Prelude qualified as Base

import "ghc-prim" GHC.Prim ( Proxy#, proxy# )

import "this" Std.Group
import "this" Std.Ord
import "this" Std.IfThenElse
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


class Functor f => Foldable f where
    
    foldMap :: forall m a. CanonicMonoid m => (a -> m) -> f a -> m
    foldMap = foldMapSemi mempty

    foldMap# :: forall m op a. Monoid op m => Proxy# op -> (a -> m) -> f a -> m
    foldMap# p = foldMapSemi# p (identity# p)
    
    fold :: forall a. CanonicMonoid a => f a -> a
    fold = foldSemi mempty

    fold# :: forall op a. Monoid op a => Proxy# op -> f a -> a
    fold# p = foldSemi# p (identity# p)
    
    foldMapSemi :: forall m a. CanonicSemigroup m => m -> (a -> m) -> f a -> m
    foldMapSemi = foldMapSemi# (proxy# @'Canonic)

    foldMapSemi# :: forall m op a. Semigroup op m => Proxy# op -> m -> (a -> m) -> f a -> m
    foldMapSemi# p m f = foldSemi# p m . map f
    
    foldSemi :: forall a. CanonicSemigroup a => a -> f a -> a
    foldSemi = foldSemi# (proxy# @'Canonic)

    foldSemi# :: forall op a. Semigroup op a => Proxy# op -> a -> f a -> a
    foldSemi# p = foldr (totalOp# p)
    
    foldr :: (a -> b -> b) -> b -> f a -> b
    foldr f z t = appEndo (foldMap (Endo . f) t) z
    
    foldl :: (b -> a -> b) -> b -> f a -> b 
    foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
    {-# MINIMAL foldr | foldMap #-}

    sum :: Monoid 'Add a => f a -> a
    sum = fold# (proxy# @'Add)

    product :: Monoid 'Mult a => f a -> a
    product = fold# (proxy# @'Mult)

    minimum, maximum :: Ord a => a -> f a -> a 
    minimum a = getMin . foldMapSemi (Min a) Min 
    maximum a = getMax . foldMapSemi (Max a) Max

    length :: Iterable i => f a -> i
    length = foldr (const succ) zero

    null :: f a -> Bool
    null = foldr (const (const False)) True

    head :: Alternative t => f a -> t a
    head = unpackMaybe . getFirst . foldMap pure

    last :: Alternative t => f a -> t a
    last = unpackMaybe . getLast . foldMap pure

    find :: Alternative t => (a -> Bool) -> f a -> t a
    find f = unpackMaybe . getFirst . foldMap (\a -> guard (f a) $> a)

    elem :: Eq a => a -> f a -> Bool
    elem a = maybe False (const True) . find (== a)

    notElem :: Eq a => a -> f a -> Bool
    notElem a = not . elem a

    lookup :: (Alternative t, Eq a) => a -> f (a, b) -> t b
    lookup a = map snd . find ((== a) . fst)

    splitAt :: (Alternative t, Iterable i) => i -> f a -> (t a, t a)
    splitAt i = res . foldr go (zero, empty, empty)
      where
        res (_, l, r) = (l, r)
        go a (j, l, r) = if (i <= j)
            then (succ j, l <|> pure a, r)
            else (succ j, l, r <|> pure a)

    take :: (Alternative t, Iterable i) => i -> f a -> t a
    take i = fst . splitAt i

    drop :: (Alternative t, Iterable i) => i -> f a -> t a
    drop i = snd . splitAt i

    index :: (Empty t, Pure t, Iterable i) => i -> f a -> t a
    index i = snd . foldr go (zero, empty)
      where
        go a (j, r) = (succ j, if j == i then pure a else r)
    
    (!!) :: (Empty t, Pure t, Iterable i) => f a -> i -> t a
    (!!) = flip index

instance (Base.Functor f, Base.Foldable f) => Foldable (Basic1 f) where
    foldMap f = getMonoidal . Base.foldMap (Monoidal . f) . getBasic1

deriving via (Basic1 []) instance Foldable []
deriving via (Basic1 ((,) a)) instance Foldable ((,) a)
deriving via (Basic1 Base.Maybe) instance Foldable Base.Maybe
deriving via (Basic1 (Base.Either e)) instance Foldable (Base.Either e)
