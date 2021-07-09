{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Product where

import "base" Data.Either
import "base" Data.Coerce

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Bifunctor


newtype Prod1 p f g a = Prod1
    { unProd1 :: f a `p` g a
    }
type Product1   = Prod1 (,)
type Coproduct1 = Prod1 Either

liftProd1 :: (f a `p` g a -> f' a' `p'` g' a') -> Prod1 p f g a -> Prod1 p' f' g' a'
liftProd1 f = Prod1 . f . unProd1


instance ( EndoFunctor c f
         , EndoFunctor c g
         , CatBifunctor c c HASK p
         ) => CatFunctor c HASK (Prod1 p f g) where
    catMap :: forall a b. a `c` b -> Prod1 p f g a -> Prod1 p f g b
    catMap = coerce catMap'
      where
        catMap' :: a `c` b -> f a `p` g a -> f b `p` g b
        catMap' f = catBimap @c @c (catMap f) (catMap f)
