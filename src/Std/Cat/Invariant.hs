{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Invariant where

import "this" Std.Cat.Functor
import "this" Std.Cat.Class
import "this" Std.Cat.Iso


class    CatFunctor (Iso c0) c1 f => CatInvariant c0 c1 f
instance CatFunctor (Iso c0) c1 f => CatInvariant c0 c1 f

type Invariant = CatInvariant HASK HASK

catInvmap :: CatInvariant c0 c1 f => a `c0` b -> b `c0` a -> f a `c1` f b 
catInvmap f g = catMap (f :<-> g)

invmap :: Invariant f => (a -> b) -> (b -> a) -> f a -> f b 
invmap = catInvmap
