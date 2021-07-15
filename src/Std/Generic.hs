{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns  #-}
{-# OPTIONS_GHC -Wno-orphans  #-}
module Std.Generic
    ( module Exp
    , fromSum
    , Generically(..), rep, GThrough
    , Generically1(..), rep1, GThrough1
    , absurdV1
    ) where

import "base" Prelude qualified as Base
import "base" Text.Show
import "base" Data.Void
import "base" Data.Either ( Either(..), either )
import "base" GHC.Generics as Exp
    ( Generic, Rep, Generic1, Rep1
    , V1, U1(..), K1(..), M1(..), type (:+:)(..), type (:*:)(..)
    )
import "base" GHC.Generics qualified as Base

import "this" Std.Cat


newtype Generically a = Generically a
  deriving stock (Show, Generic)
  deriving newtype (Base.Eq, Base.Ord)
  deriving (Functor, Pure, Bind, Join, Ap, Lift2, Applicative, Monad) via Identity

type GThrough c a = (Generic a, c (Rep a))

fromSum :: (f x -> c) -> (g x -> c) -> (f :+: g) x -> c
fromSum f _ (L1 a) = f a
fromSum _ f (R1 a) = f a


rep :: forall a x. Generic a => a <-> Rep a x
rep = Base.from :<-> Base.to

newtype Generically1 f a = Generically1 (f a)
  deriving stock (Show, Generic, Generic1)

type GThrough1 c a = (Generic1 a, c (Rep1 a))

rep1 :: forall f. Generic1 f => Rep1 f <~> f
rep1 = NT Base.to1 :<-> NT Base.from1

instance CatIsomorphic (~>) (Product1 f g) (f :*: g) where
    catIso = NT (uncurry (:*:) . unProd1)
        :<-> NT (\(a :*: b) -> Prod1 (a, b))
instance CatIsomorphic (~>) (f :*: g) (Product1 f g) where catIso = commute catIso
instance CatIsomorphic HASK (f x, g x) ((f :*: g) x) where catIso = etaIso . product1
instance CatIsomorphic HASK ((f :*: g) x) (f x, g x) where catIso = commute catIso

instance CatIsomorphic (~>) (Coproduct1 f g) (f :+: g) where
    catIso = NT (either L1 R1 . unProd1) :<-> NT (Prod1 . fromSum Left Right)
instance CatIsomorphic (~>) (f :+: g) (Coproduct1 f g) where catIso = commute catIso
instance CatIsomorphic HASK (Either (f x) (g x)) ((f :+: g) x) where catIso = etaIso . coproduct1
instance CatIsomorphic HASK ((f :+: g) x) (Either (f x) (g x)) where catIso = commute catIso

instance CatIsomorphic (~>) (Const a) (K1 i a) where
    catIso = NT (to iso . getConst) :<-> NT (Const . from iso)
instance CatIsomorphic (~>) (K1 i a) (Const a) where catIso = commute catIso
instance CatIsomorphic HASK a (K1 i a x) where catIso = coerce
instance CatIsomorphic HASK (K1 i a x) a where catIso = coerce

instance CatIsomorphic (~>) f (M1 i m f) where
    catIso = NT (to iso) :<-> NT (from iso)
instance CatIsomorphic (~>) (M1 i m f) f where catIso = commute catIso
instance CatIsomorphic HASK (f x) (M1 i m f x) where catIso = coerce
instance CatIsomorphic HASK (M1 i m f x) (f x) where catIso = coerce

instance CatIsomorphic (~>) Proxy U1 where
    catIso = NT (const U1) :<-> NT (const Proxy)
instance CatIsomorphic (~>) U1 Proxy where catIso = commute catIso
instance CatIsomorphic HASK () (U1 x) where catIso = const U1 :<-> const ()
instance CatIsomorphic HASK (U1 x) () where catIso = commute catIso

instance CatIsomorphic HASK Void (V1 x) where catIso = absurd :<-> absurdV1
instance CatIsomorphic HASK (V1 x) Void where catIso = commute catIso

absurdV1 :: V1 x -> a
absurdV1 a = case a of {}
