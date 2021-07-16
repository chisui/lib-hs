{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Cat.Hom where

import "base" Data.Coerce

import "this" Std.Type
import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.Op


newtype Hom (cat :: k -> k -> Type) (a :: k) (b :: k) = Hom
    { getHom :: a `cat` b
    }

instance Category cat => CatFunctor' Unconstrained cat HASK (Hom cat a) where
    catMap :: forall b c. b `cat` c -> Hom cat a b -> Hom cat a c
    catMap = coerce ((.) :: b `cat` c -> a `cat` b -> a `cat` c)

instance Category cat => CatLeftFunctor' Unconstrained Unconstrained (Op cat) HASK (Hom cat) where
    left' :: forall a b c. Op cat a b -> Hom cat a c -> Hom cat b c
    left' (Op f) (Hom g) = Hom (g . f)

instance Category cat => CatRightFunctor' Unconstrained Unconstrained cat HASK (Hom cat) where
    right' = catMap

instance Category cat => CatBifunctor' Unconstrained Unconstrained (Op cat) cat HASK (Hom cat)


instance Category cat => CatLeftFunctor' Unconstrained Unconstrained cat HASK (Op cat) where left' f (Op g) = Op (f . g)
instance Category cat => CatBifunctor'   Unconstrained Unconstrained cat (Op cat) HASK (Op cat)
deriving via (Hom (Op cat))   instance Category cat => CatRightFunctor' Unconstrained Unconstrained (Op cat) HASK (Op cat)
deriving via (Hom (Op cat) a) instance Category cat => CatFunctor'      Unconstrained (Op cat) HASK (Op cat a)
