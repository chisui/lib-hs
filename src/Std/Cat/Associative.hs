module Std.Cat.Associative where

import "base" Data.Either
import "base" Data.Kind

import "this" Std.Cat.Class
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.Dual
import "this" Std.Cat.NaturalTransformation
import "this" Std.Cat.Commutative
import "this" Std.Cat.Product
import "this" Std.Cat.Iso
import "this" Std.Constraint


class (EndoBifunctor Unconstraint cat f, Category cat) => CatAssociative (cat :: k -> k -> Type) (f :: k -> k -> k) where
    assoc :: Iso cat ((a `f` b) `f` c) (a `f` (b `f` c))


assocProd :: Cartesian cat => Iso cat (Product cat (Product cat a b) c) (Product cat a (Product cat b c))
assocProd = fst . fst &&& (snd . fst &&& snd)
       :<-> (fst &&& fst . snd) &&& snd . snd

assocCoprod :: Cocartesian cat => Iso cat (Coproduct cat (Coproduct cat a b) c) (Coproduct cat a (Coproduct cat b c))
assocCoprod = rightEndo lft ||| rght . rght
         :<-> lft . lft ||| leftEndo rght

instance CatAssociative HASK (,)       where assoc = assocProd
instance CatAssociative HASK Either    where assoc = assocCoprod

instance CatAssociative cat f => CatAssociative (Dual cat) f where
    assoc = liftIso (liftDual id) (liftDual id) assoc
instance CatAssociative HASK f => CatAssociative (~>) (Prod1 f) where
    assoc = NT (liftProd1 (right Prod1 . to assoc . left unProd1))
       :<-> NT (liftProd1 (left Prod1 . from assoc . right unProd1))
instance CatAssociative cat  f => CatAssociative (Iso cat) f where
    assoc = assoc :<-> commute assoc
