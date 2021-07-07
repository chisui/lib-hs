{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module Std.Cat.Iso
    ( Iso(..), type (<->), type (<~>)
    , liftIso
    , coerce, coproduct, product, dual, same
    ) where

import "base" Data.Coerce qualified as Base

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.NaturalTransformation
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.Dual
import "this" Std.Type


data Iso cat a b = (:<->)
    { to   :: a `cat` b
    , from :: b `cat` a
    }
infix 1 :<->

type (<->) = Iso HASK
type (<~>) = Iso (~>)


liftIso :: (a `cat` b -> a' `cat` b')
        -> (b `cat` a -> b' `cat` a')
        -> Iso cat a b -> Iso cat a' b'
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

dual :: a `cat` b <-> Dual cat b a
dual = coerce

same :: forall a b. a == b => a <-> b
same = case eq @a @b of Refl -> id


instance   Semigroupoid cat   => Semigroupoid        (Iso cat)   where f . g = to f . to g :<-> from g . from f
instance          CatId cat   => CatId               (Iso cat)   where id = id :<-> id
instance       Category cat   => Category            (Iso cat)
instance       Category cat   => Groupoid            (Iso cat)   where invCat f = from f :<-> to f

instance      EndoFunctor   cat f => CatFunctor        (Iso cat) (Iso cat) f where map   = liftIso map   map
instance  EndoLeftFunctor c cat f => CatLeftFunctor  c (Iso cat) (Iso cat) f where left  = liftIso left  left
instance EndoRightFunctor c cat f => CatRightFunctor c (Iso cat) (Iso cat) f where right = liftIso right right
instance    EndoBifunctor c cat f => CatBifunctor    c (Iso cat) (Iso cat) (Iso cat) f
