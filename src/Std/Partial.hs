{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Std.Partial where

import "base" GHC.Generics ( Generic )
import "base" Data.Kind ( Type )
import "base" Data.Coerce ( coerce )
import "base" Data.Maybe ( Maybe, fromMaybe )

import "this" Std.Cat
import "this" Std.Debug


data Totallity
    = Partial
    | Total
  deriving (Show, Generic)

class Monad (Res t) => FromRes t where
    data family Res (t :: Totallity) (a :: Type)
    fromRes :: a -> Res t a -> a

instance FromRes 'Total where
    newtype instance Res 'Total a = TotalRes a
      deriving stock (Show, Generic)
      deriving (Functor, Pure, Ap, Lift2, Bind, Join, Applicative, Monad) via Identity
    fromRes _ = coerce

total :: Res 'Total a -> a
total = coerce

instance FromRes 'Partial where
    newtype instance Res 'Partial a = PartialRes (Maybe a)
      deriving stock (Show, Generic)
      deriving newtype (Functor, Pure, Ap, Lift2, Bind, Join, Applicative, Monad)
    fromRes :: forall a. a -> Res 'Partial a -> a
    fromRes = coerce (fromMaybe @a)

type family Min (t0 :: Totallity) (t1 :: Totallity) :: Totallity where
    Min 'Total 'Total = 'Total
    Min a b = 'Partial

class (FromRes t0, FromRes t1) => ZipRes t0 t1 where
    joinRes :: Res t0 (Res t1 a) -> Res (t0 `Min` t1) a
    zipRes :: (a -> b -> c) -> Res t0 a -> Res t1 b -> Res (t0 `Min` t1) c

instance ZipRes 'Total 'Total where
    joinRes = coerce
    zipRes = coerce

instance ZipRes 'Total 'Partial where
    joinRes = coerce
    zipRes f a = map (f (coerce a))

instance ZipRes 'Partial 'Total where
    joinRes = coerce
    zipRes f a b = map (\ a' -> f a' (coerce b)) a

instance ZipRes 'Partial 'Partial where
    joinRes = join
    zipRes :: forall a b c. (a -> b -> c) -> Res 'Partial a -> Res 'Partial b -> Res ('Partial `Min` 'Partial) c
    zipRes = coerce (lift2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c)

zipRes3 :: (ZipRes t0 t1, ZipRes (t0 `Min` t1) t2) => (a -> b -> c -> d) -> Res t0 a -> Res t1 b -> Res t2 c -> Res (t0 `Min` t1 `Min` t2) d
zipRes3 f a b = zipRes (\f' x -> f' x) (zipRes f a b)

zipRes4 :: (ZipRes t0 t1, ZipRes (t0 `Min` t1) t2, ZipRes (t0 `Min` t1 `Min` t2) t3) => (a -> b -> c -> d -> e) -> Res t0 a -> Res t1 b -> Res t2 c -> Res t3 d-> Res (t0 `Min` t1 `Min` t2 `Min` t3) e
zipRes4 f a b c = zipRes (\f' x -> f' x) (zipRes3 f a b c)


(.?) :: (ZipRes t0 t1) => (b -> Res t1 c) -> (a -> Res t0 b) -> a -> Res (t0 `Min` t1) c
(.?) f g a = joinRes (f <$> g a)

type (-!>) a b = a -> Res 'Total b
type (-?>) a b = a -> Res 'Partial b
