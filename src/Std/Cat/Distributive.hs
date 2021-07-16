module Std.Cat.Distributive where

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Monad


class (Category cat, CatMonad cat g) => CatDistributive cat g where
    distribute :: EndoFunctor cat f => f (g a) `cat` g (f a)
type Distributive = CatDistributive HASK

instance CatDistributive HASK ((->) a) where
    distribute :: forall f b. Functor f => f (a -> b) -> a -> f b
    distribute f a = map ($ a) f
