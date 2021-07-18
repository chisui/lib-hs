module Std.Cat.Representable where

import "this" Std.Type
import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Applicative
import "this" Std.Cat.Iso
--import "this" Std.Cat.Distributive


class (Category cat1, CatFunctor cat0 cat1 f)--, CatDistributive cat f)
        => CatRepresentable (cat0 :: k0 -> k0 -> Type) (cat1 :: Type -> Type -> Type) (f :: k0 -> Type) | f -> cat0 where
    type CatRep cat0 cat1 f :: k0
    catRep :: Iso cat1 (CatRep cat0 cat1 f `cat0` a) (f a)
type Representable = CatRepresentable HASK HASK

apRep :: Representable f => f (a -> b) -> f a -> f b
apRep f g = to catRep (from catRep f <**> from catRep g)
