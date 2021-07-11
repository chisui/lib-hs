module Std.Cat.NaturalTransformation where

import "this" Std.Cat.Class
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.Product
import "this" Std.Cat.Bifunctor
import "this" Std.Type


newtype NT cat f g = NT (forall a. f a `cat` g a)

eta :: forall f g cat. NT cat f g -> (forall a. f a `cat` g a)
eta (NT f) = f
η :: forall f g cat. NT cat f g -> (forall a. f a `cat` g a)
η = eta
type (~>) = NT HASK

instance Semigroupoid cat => Semigroupoid (NT cat) where
    NT f . NT g = NT (f . g)
instance CatId cat => CatId (NT cat) where id = NT id
instance Category cat => Category (NT cat)



instance Cartesian (~>) where
    type Product (~>) = Product1
    NT f &&& NT g = NT (Prod1 . (f &&& g))
    fst = NT (fst . unProd1)
    snd = NT (snd . unProd1)

instance Cocartesian (~>) where
    type Coproduct (~>) = Coproduct1
    lft = NT (Prod1 . lft)
    rght = NT (Prod1 . rght)
    NT f ||| NT g = NT ((f ||| g) . unProd1)

instance EndoLeftFunctor HASK f => CatLeftFunctor' Unconstrained Unconstrained (~>) (~>) (Prod1 f) where
    left' (NT f) = NT (liftProd1 (left f))
instance EndoRightFunctor HASK f => CatRightFunctor' Unconstrained Unconstrained (~>) (~>) (Prod1 f) where
    right' (NT f) = NT (liftProd1 (right f))
instance EndoBifunctor HASK f => CatBifunctor' Unconstrained Unconstrained (~>) (~>) (~>) (Prod1 f) where
    catBimap' (NT f) (NT g) = NT (liftProd1 (catBimap f g))