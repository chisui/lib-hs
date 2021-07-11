{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
module Std.Cat.Iso where

import "base" Data.Either qualified as Base
import "base" Data.Coerce qualified as Base
import "base" Data.Functor.Identity qualified as Base

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.NaturalTransformation
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.Commutative
import "this" Std.Cat.Product
import "this" Std.Cat.Op
--import "this" Std.Cat.Arrow
import "this" Std.Type


data Iso cat a b = (:<->)
    { to   :: a `cat` b
    , from :: b `cat` a
    }
infix 1 :<->

type (<->) = Iso HASK
infixr 1 <->
type (<~>) = Iso (~>)
infixr 0 <~>


liftIso :: (a `cat` b -> a' `cat'` b')
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

instance   Semigroupoid cat   => Semigroupoid        (Iso cat)   where f . g = to f . to g :<-> from g . from f
instance          CatId cat   => CatId               (Iso cat)   where id = id :<-> id
instance       Category cat   => Category            (Iso cat)
instance       Category cat   => Groupoid            (Iso cat)   where invCat f = from f :<-> to f

instance      EndoFunctor       cat f => CatFunctor            (Iso cat) (Iso cat) f where catMap = liftIso catMap catMap
instance  EndoLeftFunctor' c c' cat f => CatLeftFunctor'  c c' (Iso cat) (Iso cat) f where left'   = liftIso left'   left'
instance EndoRightFunctor' c c' cat f => CatRightFunctor' c c' (Iso cat) (Iso cat) f where right'  = liftIso right'  right'
instance    EndoBifunctor' c c' cat f => CatBifunctor'    c c' (Iso cat) (Iso cat) (Iso cat) f

instance CatCommutative cat  f => CatCommutative (Iso cat) f         where commute = commute :<-> commute
instance       Category cat    => CatCommutative HASK      (Iso cat) where commute = invCat


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
