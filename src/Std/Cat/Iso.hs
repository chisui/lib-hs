{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
module Std.Cat.Iso
    ( Iso(..), type(<->), type (≅), type (<~>), pattern (:<->), to, from, liftIso
    , product, coproduct, product1, coproduct1, same, coerce, Base.Coercible
    , CatIsomorphic(..), Isomorphic, iso, type (<-->), type (<~~>)
    , etaIso, isoThrough
    ) where

import "base" Data.Either qualified as Base
import "base" Data.Coerce qualified as Base
import "base" Data.Functor.Identity qualified as Base

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.NaturalTransformation
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.Limit
import "this" Std.Cat.Commutative
import "this" Std.Cat.Product
import "this" Std.Cat.Op
import "this" Std.Cat.Hom
import "this" Std.Cat.ProductCat
import "this" Std.Type


newtype Iso cat a b = Iso ((cat × (Op cat)) '(a, a) '(b, b))
pattern (:<->) :: (Category' c cat, c a, c b) => a `cat` b -> b `cat` a -> Iso cat a b
pattern t :<-> f = Iso (ProdCat (t, Op f))
infix 1 :<->
{-# COMPLETE (:<->) #-}

to :: (Category' c cat, c a, c b) => Iso cat a b -> a `cat` b
to (t :<-> _) = t
from :: (Category' c cat, c a, c b) => Iso cat a b -> b `cat` a
from (_ :<-> f) = f

type (<->) = Iso HASK
infixr 1 <->
type (<~>) = Iso (~>)
infixr 0 <~>

infixr 1 ≅
type (≅) (l :: k) (r :: k) = IsoFor k l r
class    ChooseIso k           where type IsoFor k :: k -> k -> Type
instance ChooseIso Type        where type IsoFor Type        = (<->)
instance ChooseIso (k -> Type) where type IsoFor (k -> Type) = (<~>)


liftIso :: (Category cat, Category cat')
        => (a `cat` b -> a' `cat'` b')
        -> (b `cat` a -> b' `cat'` a')
        -> Iso cat a b -> Iso cat' a' b'
liftIso f g h = f (to h) :<-> g (from h)


coerce :: forall a b. Base.Coercible a b => a <-> b
coerce = Base.coerce @a @b :<-> Base.coerce @b @a

product :: Cartesian cat
        => a `cat` b
        -> a `cat` c
        -> Product cat b c `cat` a
        -> Iso cat a (Product cat b c)
product l r f = l &&& r :<-> f

coproduct :: Cocartesian cat
          => b `cat` a
          -> c `cat` a
          -> a `cat` Coproduct cat b c
          -> Iso cat a (Coproduct cat b c)
coproduct l r f = f :<-> l ||| r

product1 :: (f a, g a) <-> Product1 f g a
product1 = coerce

coproduct1 :: Base.Either (f a) (g a) <-> Coproduct1 f g a
coproduct1 = coerce

same :: forall a b. a == b => a <-> b
same = case eq @a @b of Refl -> id

instance Category' c cat => Semigroupoid' c (Iso cat) where f . g = to f . to g :<-> from g . from f
instance Category' c cat => CatId'        c (Iso cat) where id = id :<-> id
instance Category' c cat => Category'     c (Iso cat)
instance Category' c cat => Groupoid'     c (Iso cat) where catInv f = from f :<-> to f

instance (CatTerminal cat, CatInitial cat, Terminal cat ~ Initial cat) => CatTerminal (Iso cat) where
    type Terminal (Iso cat) = Initial cat
    terminate = terminate :<-> initiate
instance (CatTerminal cat, CatInitial cat, Terminal cat ~ Initial cat) => CatInitial (Iso cat) where
    type Initial (Iso cat) = Terminal cat
    initiate = initiate :<-> terminate

deriving via (Hom (Iso cat))   instance Category cat => CatLeftFunctor'  Unconstrained Unconstrained (Op (Iso cat)) HASK (Iso cat)
deriving via (Hom (Iso cat))   instance Category cat => CatRightFunctor' Unconstrained Unconstrained (Iso cat) HASK (Iso cat)
deriving via (Hom (Iso cat))   instance Category cat => CatBifunctor'    Unconstrained Unconstrained (Op (Iso cat)) (Iso cat) HASK (Iso cat)
deriving via (Hom (Iso cat) a) instance Category cat => CatFunctor'      Unconstrained (Iso cat) HASK (Iso cat a)

instance EndoLeftFunctor'  c c' cat f => CatLeftFunctor'  c c' (Iso cat) (Iso cat) f where left'  = liftIso left'   left'
instance EndoRightFunctor' c c' cat f => CatRightFunctor' c c' (Iso cat) (Iso cat) f where right' = liftIso right'  right'
instance EndoBifunctor'    c c' cat f => CatBifunctor'    c c' (Iso cat) (Iso cat) (Iso cat) f
instance EndoFunctor'      c    cat f => CatFunctor'      c    (Iso cat) (Iso cat) f where catMap = liftIso catMap catMap


instance CatCommutative cat  f => CatCommutative (Iso cat) f         where commute = commute :<-> commute
instance       Category cat    => CatCommutative HASK      (Iso cat) where commute = catInv


class CatIsomorphic cat a b where catIso :: Iso cat a b
type Isomorphic = CatIsomorphic HASK
type (<-->) = Isomorphic
type (<~~>) = CatIsomorphic (~>)
iso :: forall b a. a <--> b => a <-> b
iso = catIso

isoThrough :: forall b a c. (a <--> b, b <--> c) => a <-> c
isoThrough = (iso :: b <-> c) . (iso :: a <-> b)

etaIso :: forall g f a. f <~~> g => f a <-> g a
etaIso = t' :<-> f'
  where
    (NT t' :<-> NT f') = catIso

instance CatIsomorphic HASK (Op cat b a) (a `cat` b) where catIso = coerce


instance CatIsomorphic HASK (Base.Identity a) a where catIso = coerce
