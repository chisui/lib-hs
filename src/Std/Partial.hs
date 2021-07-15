{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Std.Partial
    ( Totallity(..), Res(..), DirectRes, TotalRes, PartialRes
    , MapDirectRes(..), ZipDirectRes(..)
    , Undefinable(..)
    , unpackRes
    , fromRes, toRes, total, total2
    , MinTotallity, joinRes, zipRes, zipRes3, zipRes4
    , (.?), (=<<?), (?>>=)
    , type (-!>), type (-?>)
    ) where

import "base" Text.Show ( Show )
import "base" Data.Eq ( Eq(..) )
import "base" Data.Bool ( Bool(..) )
import "base" Data.Maybe ( Maybe, maybe )

import "this" Std.Type
import "this" Std.Generic
import "this" Std.Cat


data Totallity
    = Partial
    | Total
  deriving (Show, Generic)

data Res (t :: Totallity) (a :: Type) where
    FullRes  :: a -> Res t a
    EmptyRes :: Res 'Partial a

type TotalRes   = Res 'Total
type PartialRes = Res 'Partial

type (-!>) a b = a -> Res 'Total b
type (-?>) a b = a -> Res 'Partial b

type family DirectRes (t :: Totallity) a where
    DirectRes 'Total   a = a
    DirectRes 'Partial a = PartialRes a

class MapDirectRes (t :: Totallity) where
    mapDirectRes# :: Proxy# t -> (a -> b) -> DirectRes t a -> DirectRes t b
    mapDirectRes :: forall a b proxy. proxy t -> (a -> b) -> DirectRes t a -> DirectRes t b
    mapDirectRes _ = mapDirectRes# (proxy# @t)
instance MapDirectRes 'Partial where
    mapDirectRes# _ f (FullRes a) = FullRes (f a)
    mapDirectRes# _ _ EmptyRes    = EmptyRes
instance MapDirectRes 'Total where
    mapDirectRes# _ = id

class (MapDirectRes t, MapDirectRes t') => ZipDirectRes (t :: Totallity) (t' :: Totallity) where
    zipDirectRes# :: Proxy# t -> Proxy# t' -> (a -> b -> c) -> DirectRes t a -> DirectRes t' b -> DirectRes (MinTotallity t t') c

instance ZipDirectRes 'Total 'Total where
    zipDirectRes# _ _ f a b = f a b
instance ZipDirectRes 'Total 'Partial where
    zipDirectRes# _ _ _ _ EmptyRes    = EmptyRes
    zipDirectRes# _ _ f a (FullRes b) = FullRes (f a b)
instance ZipDirectRes 'Partial 'Total where
    zipDirectRes# _ _ _ EmptyRes    _ = EmptyRes
    zipDirectRes# _ _ f (FullRes a) b = FullRes (f a b)
instance ZipDirectRes 'Partial 'Partial where
    zipDirectRes# _ _ _ EmptyRes    _           = EmptyRes
    zipDirectRes# _ _ _ _           EmptyRes    = EmptyRes
    zipDirectRes# _ _ f (FullRes a) (FullRes b) = FullRes (f a b)

class    Undefinable a where                         undefined :: Undefinable a => a
instance Undefinable b => Undefinable (a -> b) where undefined _ = undefined
instance Undefinable (Res 'Partial a) where          undefined = EmptyRes

instance Eq a => Eq (Res t a) where
    EmptyRes == EmptyRes = True
    FullRes a == FullRes b = a == b
    _ == _ = False

instance CatFunctor' Unconstrained HASK HASK (Res t) where
    catMap f (FullRes a) = FullRes (f a)
    catMap _ EmptyRes = EmptyRes
instance CatPure' Unconstrained HASK (Res t) where
    catPure = FullRes
instance CatAp' Unconstrained HASK (Res t) where
    FullRes f <**> FullRes a = FullRes (f a)
    EmptyRes  <**> _         = EmptyRes
    _         <**> EmptyRes  = EmptyRes
instance CatLift2' Unconstrained HASK (Res t) where
    lift2 f (FullRes a) (FullRes b) = pure (f a b)
    lift2 _ EmptyRes _ = EmptyRes
    lift2 _ _ EmptyRes = EmptyRes
instance CatBind' Unconstrained HASK (Res t) where
    (=<<) f (FullRes a) = f a
    (=<<) _ EmptyRes = EmptyRes
instance CatJoin' Unconstrained HASK (Res t) where
    join (FullRes r) = r
    join EmptyRes = EmptyRes

instance CatApplicative' Unconstrained HASK (Res t)
instance CatMonad' Unconstrained HASK (Res t)

instance CatEmpty' Unconstrained HASK (Res 'Partial) where
    catEmpty _ = EmptyRes
instance CatCombine' Unconstrained HASK (Res t) where
    combine (EmptyRes, r) = r
    combine (r,        _) = r
instance CatAlternative' Unconstrained HASK (Res 'Partial)

instance CatMonadFail' Unconstrained HASK (Res 'Partial) where
    fail _ = empty

instance CatIsomorphic HASK (Res 'Total a) (Identity a) where
    catIso = etaIso
instance CatIsomorphic (~>) (Res 'Total) Identity where
    catIso = NT (\(FullRes a) -> pure a) :<-> NT (\(Identity a) -> pure a)

unpackRes :: Alternative f => Res t a -> f a
unpackRes (FullRes a) = pure a
unpackRes EmptyRes    = empty

fromRes :: a -> Res t a -> a
fromRes _ (FullRes a) = a
fromRes a _ = a

toRes :: Maybe a -> Res 'Partial a
toRes = maybe EmptyRes FullRes

total :: Res 'Total a -> a
total (FullRes a) = a

total2 :: (a -> b -> Res 'Total c) -> a -> b -> c
total2 f a b = total (f a b)


type family MinTotallity (t0 :: Totallity) (t1 :: Totallity) :: Totallity where
    MinTotallity 'Total 'Total = 'Total
    MinTotallity a b = 'Partial

joinRes :: Res t0 (Res t1 a) -> Res (t0 `MinTotallity` t1) a
joinRes (FullRes (FullRes a)) = FullRes a
joinRes (FullRes EmptyRes) = EmptyRes
joinRes EmptyRes = EmptyRes

zipRes :: forall t0 t1 a b c. (a -> b -> c) -> Res t0 a -> Res t1 b -> Res (t0 `MinTotallity` t1) c
zipRes f (FullRes a) (FullRes b) = pure (f a b)
zipRes _ EmptyRes _ = EmptyRes
zipRes _ _ EmptyRes = EmptyRes

zipRes3 :: (a -> b -> c -> d) -> Res t0 a -> Res t1 b -> Res t2 c -> Res (t0 `MinTotallity` t1 `MinTotallity` t2) d
zipRes3 f a = zipRes id . zipRes f a

zipRes4 :: (a -> b -> c -> d -> e) -> Res t0 a -> Res t1 b -> Res t2 c -> Res t3 d-> Res (t0 `MinTotallity` t1 `MinTotallity` t2 `MinTotallity` t3) e
zipRes4 f a b = zipRes id . zipRes3 f a b


(.?) :: (b -> Res t1 c) -> (a -> Res t0 b) -> a -> Res (t0 `MinTotallity` t1) c
(.?) f g a = f =<<? g a

(=<<?) :: (a -> Res t1 b) -> Res t0 a -> Res (t0 `MinTotallity` t1) b
(=<<?) f = joinRes . map f

(?>>=) :: Res t0 a -> (a -> Res t1 b) -> Res (t0 `MinTotallity` t1) b
(?>>=) = flip (=<<?)
