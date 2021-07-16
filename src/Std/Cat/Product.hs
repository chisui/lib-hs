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



newtype Prod2 p f g a b = Prod2
    { unProd2 :: f a b `p` g a b
    }
type Product2   = Prod2 (,)
type Coproduct2 = Prod2 Either

liftProd2 :: (f a b `p` g a b -> f' a' b' `p'` g' a' b') -> Prod2 p f g a b -> Prod2 p' f' g' a' b'
liftProd2 f = Prod2 . f . unProd2


instance ( EndoLeftFunctor cat f
         , EndoLeftFunctor cat g
         , CatBifunctor cat cat HASK p
         ) => CatLeftFunctor' Unconstrained Unconstrained cat HASK (Prod2 p f g) where
    left' :: forall a b c. a `cat` b -> Prod2 p f g a c -> Prod2 p f g b c
    left' = coerce left''
      where
        left'' :: a `cat` b -> f a c `p` g a c -> f b c `p` g b c
        left'' f = catBimap @cat @cat @HASK (left f) (left f)

instance ( EndoRightFunctor cat f
         , EndoRightFunctor cat g
         , CatBifunctor cat cat HASK p
         ) => CatRightFunctor' Unconstrained Unconstrained cat HASK (Prod2 p f g) where
    right' :: forall a b c. a `cat` b -> Prod2 p f g c a -> Prod2 p f g c b
    right' = coerce right''
      where
        right'' :: a `cat` b -> f c a `p` g c a -> f c b `p` g c b
        right'' f = catBimap @cat @cat @HASK (right f) (right f)

instance ( EndoBifunctor cat f
         , EndoBifunctor cat g
         , CatBifunctor cat cat HASK p
         ) => CatBifunctor' Unconstrained Unconstrained cat cat HASK (Prod2 p f g)
