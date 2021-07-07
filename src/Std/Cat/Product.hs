{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Product where

import "base" Data.Either
import "base" Data.Coerce

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Bifunctor
import "this" Std.Constraint


newtype Prod1 p f g a = Prod1
    { unProd1 :: f a `p` g a
    }
type Product1 = Prod1 (,)
type Coproduct1 = Prod1 Either

liftProd1 :: (f a `p` g a -> f' a' `p'` g' a') -> Prod1 p f g a -> Prod1 p' f' g' a'
liftProd1 f = Prod1 . f . unProd1


instance ( EndoFunctor c f
         , EndoFunctor c g
         , CatBifunctor Unconstraint c c HASK p
         ) => CatFunctor c HASK (Prod1 p f g) where
    map :: forall a b. a `c` b -> Prod1 p f g a -> Prod1 p f g b
    map f = coerce (catBimap @Unconstraint @c @c (map f) (map f) :: p (f a) (g a) -> p (f b) (g b))
