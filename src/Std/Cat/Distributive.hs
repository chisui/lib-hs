module Std.Cat.Distributive where

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Closed


class CatDistributive cat g where
    distribute :: EndoFunctor cat f => f (g a) `cat` g (f a)
type Distributive = CatDistributive HASK


instance CatDistributive HASK ((->) a) where
    distribute = flip (map . flip id)
