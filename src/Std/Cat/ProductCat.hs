{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.ProductCat where

import "base" Data.Kind
import "base" Data.Coerce

import "this" Std.Type
import "this" Std.Cat.Class
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.Biapplicative


newtype ProdCat
        (c0 :: o0 -> o0 -> Type)
        (c1 :: o1 -> o1 -> Type)
        (a :: (o0, o1)) (b :: (o0, o1)) 
    = ProdCat (ProdCat' c0 c1 a b)

type ProdCat' c0 c1 a b = (FstCat c0 a b, SndCat c1 a b)
type FstCat cat a b = Fst a `cat` Fst b
type SndCat cat a b = Snd a `cat` Snd b

type (×) = ProdCat

instance (Semigroupoid l, Semigroupoid r) => Semigroupoid' Unconstrained (l × r) where
    (.) :: forall a b c. (l × r) b c -> (l × r) a b -> (l × r) a c
    (.) = coerce ((.) `bilift2` (.) :: ProdCat' l r b c -> ProdCat' l r a b -> ProdCat' l r a c)

instance (CatId l, CatId r) => CatId' Unconstrained (l × r) where
    id :: forall a. (l × r) a a
    id = coerce (id `bipure` id :: ProdCat' l r a a)

instance (Category l, Category r) => Category' Unconstrained (l × r)

instance (Groupoid l, Groupoid r) => Groupoid' Unconstrained (l × r) where
    catInv :: forall a b. (l × r) a b -> (l × r) b a
    catInv = coerce (catInv *** catInv :: ProdCat' l r a b -> ProdCat' l r b a)
