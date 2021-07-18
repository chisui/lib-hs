module Std.Cat.Distributive where

import "this" Std.Cat.Class
import "this" Std.Cat.Functor


class (Category cat, EndoFunctor cat g) => CatDistributive cat g where
    distribute :: EndoFunctor cat f => f (g a) `cat` g (f a)
    distribute = collect id
    collect :: EndoFunctor cat f => (a `cat` g b) -> f a `cat` g (f b)
    collect f = distribute . catMap f
    {-# MINIMAL distribute | collect #-}
type Distributive = CatDistributive HASK

instance CatDistributive HASK ((->) a) where
    distribute f a = map ($ a) f
    collect f = distribute . map f
