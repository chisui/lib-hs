{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Set where

import "containers" Data.Set ( Set )
import "containers" Data.Set qualified as Set

import "this" Std.Cat
import "this" Std.Ord
import "this" Std.Maybe
import "this" Std.Cat.Foldable


instance CatFunctor' Ord HASK HASK Set where
    catMap f = Set.mapMonotonic getOrdered . Set.map (Ordered . f)

instance Foldable' Ord Set where
    foldr    = Set.foldr
    foldl    = Set.foldl
    head     = unpackMaybe . Set.lookupMax
    last     = unpackMaybe . Set.lookupMin
    find p   = find p . Set.toList
    lookup a = map snd . find ((== a) . fst) . Set.toList
