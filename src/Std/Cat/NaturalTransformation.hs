module Std.Cat.NaturalTransformation where

import "this" Std.Cat.Class
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.Product
import "this" Std.Cat.Associative
import "this" Std.Cat.Bifunctor
import "this" Std.Constraint


newtype NT cat f g = NT
    { eta :: forall a. f a `cat` g a
    }
η :: NT cat f g -> (forall a. f a `cat` g a)
η = eta
type (~>) = NT HASK

instance Semigroupoid cat => Semigroupoid (NT cat) where
    NT f . NT g = NT (f . g)
instance CatId cat => CatId (NT cat) where
    id = NT id
instance Category cat => Category (NT cat)



instance Cartesian (~>) where
    type Product (~>) = Product1
    NT f &&& NT g = NT (Prod1 . (f &&& g))
    fst = NT (fst . prod1)
    snd = NT (snd . prod1)

instance Cocartesian (~>) where
    type Coproduct (~>) = Coproduct1
    lft = NT (Prod1 . lft)
    rght = NT (Prod1 . rght)
    NT f ||| NT g = NT ((f ||| g) . prod1)

instance CatAssociative HASK f => CatAssociative (~>) (Prod1 f) where
    assoc = NT (Prod1 . assoc . prod1)


instance EndoLeftFunctor Unconstraint HASK f => CatLeftFunctor Unconstraint (~>) (~>) (Prod1 f) where
    left (NT f) = NT (Prod1 . left f . prod1)
instance EndoRightFunctor Unconstraint HASK f => CatRightFunctor Unconstraint (~>) (~>) (Prod1 f) where
    right (NT f) = NT (Prod1 . right f . prod1)
instance EndoBifunctor Unconstraint HASK f => CatBifunctor Unconstraint (~>) (~>) (~>) (Prod1 f) where
    catBimap (NT f) (NT g) = NT (Prod1 . catBimap f g . prod1)