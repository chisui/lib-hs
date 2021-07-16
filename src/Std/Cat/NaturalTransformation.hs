module Std.Cat.NaturalTransformation where

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Hom
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

instance Semigroupoid cat => Semigroupoid (NT cat) where NT f . NT g = NT (f . g)
instance CatId        cat => CatId        (NT cat) where id = NT id
instance Category     cat => Category     (NT cat)
instance Groupoid     cat => Groupoid     (NT cat) where catInv (NT f) = NT (catInv f)

deriving via (Hom (NT cat) a) instance Category cat => CatFunctor' Unconstrained (NT cat) HASK (NT cat a)

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

yoneda :: Category cat => cat a ~> f -> f a
yoneda = (`eta` id)

instance EndoLeftFunctor  HASK f => CatLeftFunctor'  Unconstrained Unconstrained (~>) (~>)      (Prod1 f) where left'     (NT f)        = NT (liftProd1 (left f))
instance EndoRightFunctor HASK f => CatRightFunctor' Unconstrained Unconstrained (~>) (~>)      (Prod1 f) where right'    (NT f)        = NT (liftProd1 (right f))
instance EndoBifunctor    HASK f => CatBifunctor'    Unconstrained Unconstrained (~>) (~>) (~>) (Prod1 f) where catBimap' (NT f) (NT g) = NT (liftProd1 (catBimap f g))


newtype NT2 cat f g = NT2 (forall a b. f a b `cat` g a b)

eta2 :: forall f g cat. NT2 cat f g -> (forall a b. f a b `cat` g a b)
eta2 (NT2 f) = f
type (:->) = NT2 HASK

instance Semigroupoid cat => Semigroupoid (NT2 cat) where NT2 f . NT2 g = NT2 (f . g)
instance CatId        cat => CatId        (NT2 cat) where id = NT2 id
instance Category     cat => Category     (NT2 cat)
instance Groupoid     cat => Groupoid     (NT2 cat) where catInv (NT2 f) = NT2 (catInv f)


instance Cartesian (:->) where
    type Product (:->) = Product2
    NT2 f &&& NT2 g = NT2 (Prod2 . (f &&& g))
    fst = NT2 (fst . unProd2)
    snd = NT2 (snd . unProd2)

instance Cocartesian (:->) where
    type Coproduct (:->) = Coproduct2
    lft = NT2 (Prod2 . lft)
    rght = NT2 (Prod2 . rght)
    NT2 f ||| NT2 g = NT2 ((f ||| g) . unProd2)

instance EndoLeftFunctor HASK f => CatLeftFunctor' Unconstrained Unconstrained (:->) (:->) (Prod2 f) where
    left' (NT2 f) = NT2 (liftProd2 (left f))
instance EndoRightFunctor HASK f => CatRightFunctor' Unconstrained Unconstrained (:->) (:->) (Prod2 f) where
    right' (NT2 f) = NT2 (liftProd2 (right f))
instance EndoBifunctor HASK f => CatBifunctor' Unconstrained Unconstrained (:->) (:->) (:->) (Prod2 f) where
    catBimap' (NT2 f) (NT2 g) = NT2 (liftProd2 (catBimap f g))
