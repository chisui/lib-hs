{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Foldable where

import "ghc-prim" GHC.Prim ( Proxy#, proxy# )

import "this" Std.Group
import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Closed
import "this" Std.Cat.Endo
import "this" Std.Cat.Dual


class Functor f => Foldable f where
    
    foldMap :: forall m a. CanonicMonoid m => (a -> m) -> f a -> m
    foldMap = foldMap# (proxy# @'Canonic)

    foldMap# :: forall m op a. Monoid op m => Proxy# op -> (a -> m) -> f a -> m
    foldMap# p f = fold# p . map f
    
    fold :: forall a. CanonicMonoid a => f a -> a
    fold = fold# (proxy# @'Canonic)

    fold# :: forall op a. Monoid op a => Proxy# op -> f a -> a
    fold# p = foldr (totalOp# p) (identity# p)
    
    foldr :: (a -> b -> b) -> b -> f a -> b
    foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
    
    foldl :: (b -> a -> b) -> b -> f a -> b 
    foldr f z t = appEndo (foldMap (Endo . f) t) z


sum :: (Monoid 'Add a, Foldable f) => f a -> a
sum = fold# (proxy# @'Add)

product :: (Monoid 'Mult a, Foldable f) => f a -> a
product = fold# (proxy# @'Mult)
