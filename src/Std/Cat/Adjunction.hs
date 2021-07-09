module Std.Cat.Adjunction where

import "base" Data.Functor.Identity

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Iso


class (Category c0, Category c1, CatFunctor c0 c1 g, CatFunctor c1 c0 f) => CatAdjunction c0 c1 f g | f -> g, g -> f, f c0 -> c1, f c1 -> c0, g c0 -> c1, f c0 -> c1 where
    unit :: y `c1` g (f y)
    unit = to adjoint id
    counit :: f (g x) `c0` x
    counit = from adjoint id
    adjoint :: f y `c0` x <-> y `c1` g x
    {-# MINIMAL adjoint #-}
type EndoAdjunction cat = CatAdjunction cat cat
type Adjunction = EndoAdjunction HASK

defaultAjoint :: Adjunction f g => (f a -> b) <-> (a -> g b)
defaultAjoint = (\f -> map f . unit) :<-> (\f -> counit . map f)


instance CatAdjunction HASK HASK Identity Identity where adjoint = coerce
