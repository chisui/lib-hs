{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Std.Generic
    ( module Exp
    , Generically(..), rep, GThrough
    , Generically1(..), rep1, GThrough1
    , absurdV1
    , prodRep, sumRep, k1, m1, v1, u1
    ) where

import "base" Data.Void
import "base" Prelude ( Either(..) )
import "base" GHC.Generics as Exp
    ( Generic, Rep, Generic1, Rep1
    , V1, U1(..), K1(..), M1(..), type (:+:)(..), type (:*:)(..)
    )
import "base" GHC.Generics qualified as Base

import "this" Std.Debug
import "this" Std.Cat


newtype Generically a = Generically a
  deriving stock (Show, Generic)
  deriving (Functor, Pure, Bind, Join, Ap, Lift2, Applicative, Monad) via Identity

type GThrough c a = (Generic a, c (Rep a))

rep :: forall a x. Generic a => Rep a x <-> a
rep = Base.to :<-> Base.from

newtype Generically1 f a = Generically1 (f a)
  deriving stock (Show, Generic, Generic1)

type GThrough1 c a = (Generic1 a, c (Rep1 a))

rep1 :: forall f. Generic1 f => Rep1 f <~> f
rep1 = NT Base.to1 :<-> NT Base.from1

prodRep :: (f :*: g) x <-> (f x, g x)
prodRep = (\(a :*: b) -> (a, b)) :<-> uncurry (:*:)

sumRep :: (f :+: g) x <-> Either (f x) (g x)
sumRep = coproductIso L1 R1 $ \case
    L1 a -> Left a
    R1 a -> Right a

k1 :: K1 i a x <-> a
k1 = coerceIso

m1 :: M1 i m f x <-> f x
m1 = coerceIso

u1 :: U1 x <-> ()
u1 = const () :<-> const U1

v1 :: V1 x <-> Void
v1 = absurdV1 :<-> absurd

absurdV1 :: V1 x -> a
absurdV1 a = case a of {}
