{-# LANGUAGE AllowAmbiguousTypes #-}
module Std.Cat.FullyFaithful where

import "base" Data.Functor.Identity

import "this" Std.Type
import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Iso


class CatFunctor' c cat0 cat1 f => CatFullyFaithful' c cat0 cat1 f | f cat0 -> c where
    catUnmap :: (c a, c b) => f a `cat1` f b -> a `cat0` b
type CatFullyFaithful         = CatFullyFaithful' Unconstrained
type EndoFullyFaithful' c cat = CatFullyFaithful' c cat cat
type EndoFullyFaithful    cat = CatFullyFaithful cat cat
type FullyFaithful'     c     = EndoFullyFaithful' c HASK
type FullyFaithful            = EndoFullyFaithful HASK

unmap :: forall f a b c. (FullyFaithful' c f, c a, c b) => (f a -> f b) -> a -> b
unmap = catUnmap

instance CatFullyFaithful' Unconstrained HASK HASK Identity where catUnmap = to coerce
