{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.ProductCat where

import "base" Data.Kind
import "base" Data.Coerce

import "this" Std.Cat.Class
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.Biapplicative


newtype ProdCat (p :: ml -> mr -> Type) (l :: ol -> ol -> ml) (r :: or -> or -> mr) (a :: (ol, or)) (b :: (ol, or)) 
    = ProdCat ((Fst a `l` Fst b) `p` (Snd a `r` Snd b))

fromCat :: (CatId l, CatId r, Bipure p) => ProdCat p l r (al `f` ar) (bl `g` br) -> (al `l` bl) `p` (ar `r` br)
fromCat = coerce

instance (Bilift2 p, Semigroupoid l, Semigroupoid r) => Semigroupoid (ProdCat p l r) where
    (.) :: forall a b c. ProdCat p l r b c -> ProdCat p l r a b -> ProdCat p l r a c
    (.) = coerce ((.) `bilift2` (.) :: (Fst b `l` Fst c) `p` (Snd b `r` Snd c) -> (Fst a `l` Fst b) `p` (Snd a `r` Snd b) -> (Fst a `l` Fst c) `p` (Snd a `r` Snd c))

instance (Bipure p, CatId l, CatId r) => CatId (ProdCat p l r) where
    id :: forall a. ProdCat p l r a a
    id = coerce (id `bipure` id :: (Fst a `l` Fst a) `p` (Snd a `r` Snd a))

instance (Biapplicative p, Category l, Category r) => Category (ProdCat p l r)

instance (Biapplicative p, Groupoid l, Groupoid r) => Groupoid (ProdCat p l r) where
    catInv :: forall a b. ProdCat p l r a b -> ProdCat p l r b a
    catInv = coerce (catInv *** catInv :: (Fst a `l` Fst b) `p` (Snd a `r` Snd b) -> (Fst b `l` Fst a) `p` (Snd b `r` Snd a))
