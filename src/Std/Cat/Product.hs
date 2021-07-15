{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Product where

import "base" Data.Either
import "base" Data.Coerce

import "this" Std.Type
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


instance ( EndoFunctor cat f
         , EndoFunctor cat g
         , CatBifunctor cat cat HASK p
         ) => CatFunctor' Unconstrained cat HASK (Prod1 p f g) where
    catMap :: forall a b. a `cat` b -> Prod1 p f g a -> Prod1 p f g b
    catMap = coerce catMap'
      where
        catMap' :: a `cat` b -> f a `p` g a -> f b `p` g b
        catMap' f = catBimap @cat @cat (catMap f) (catMap f)
