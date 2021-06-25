module Std.Generic
    ( Base.Generic, Base.Rep, Generically(..), fromRep, toRep
    , GThrough
    , Base.Generic1, Base.Rep1, Generically1(..), fromRep1, toRep1
    , GThrough1
    ) where

import "base" GHC.Generics qualified as Base

import "this" Std.Debug
import "this" Std.Cat


newtype Generically a = Generically a
  deriving stock (Show, Base.Generic)
  deriving (Functor, Pure, Bind, Join, Ap, Lift2, Applicative, Monad) via Identity

type GThrough c a = (Base.Generic a, c (Base.Rep a))

fromRep :: forall a x. Base.Generic a => Base.Rep a x -> a
fromRep = Base.to

toRep :: forall a x. Base.Generic a => a -> Base.Rep a x
toRep = Base.from


newtype Generically1 f a = Generically1 (f a)
  deriving stock (Show, Base.Generic, Base.Generic1)

type GThrough1 c a = (Base.Generic1 a, c (Base.Rep1 a))

fromRep1 :: forall f a. Base.Generic1 f => Base.Rep1 f a -> f a
fromRep1 = Base.to1

toRep1 :: forall f a. Base.Generic1 f => f a -> Base.Rep1 f a
toRep1 = Base.from1
