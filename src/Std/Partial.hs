{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Std.Partial where

import "base" GHC.Generics ( Generic )
import "base" Data.Kind ( Type )
import "base" Data.Maybe ( Maybe, maybe )

import "this" Std.Cat
import "this" Std.Debug


data Totallity
    = Partial
    | Total
  deriving (Show, Generic)

data Res (t :: Totallity) (a :: Type) where
    FullRes :: a -> Res t a
    EmptyRes :: Res 'Partial a

type (-!>) a b = a -> Res 'Total b
type (-?>) a b = a -> Res 'Partial b

instance CatFunctor HASK HASK (Res t) where
    map f (FullRes a) = FullRes (f a)
    map _ EmptyRes = EmptyRes
instance CatPure HASK (Res t) where
    pure = FullRes
instance CatAp HASK (Res t) where
    FullRes f <*> FullRes a = FullRes (f a)
    EmptyRes <*> _ = EmptyRes
    _ <*> EmptyRes = EmptyRes
instance CatLift2 HASK (Res t) where
    lift2 f (FullRes a) (FullRes b) = pure (f a b)
    lift2 _ EmptyRes _ = EmptyRes
    lift2 _ _ EmptyRes = EmptyRes
instance CatBind HASK (Res t) where
    (=<<) f (FullRes a) = f a
    (=<<) _ EmptyRes = EmptyRes
instance CatJoin HASK (Res t) where
    join (FullRes r) = r
    join EmptyRes = EmptyRes

instance CatApplicative HASK (Res t)
instance CatMonad HASK (Res t)


fromRes :: a -> Res t a -> a
fromRes _ (FullRes a) = a
fromRes a _ = a

toRes :: Maybe a -> Res 'Partial a
toRes = maybe EmptyRes FullRes

total :: Res 'Total a -> a
total (FullRes a) = a

total2 :: (a -> b -> Res 'Total c) -> a -> b -> c
total2 f a b = total (f a b)


type family Min (t0 :: Totallity) (t1 :: Totallity) :: Totallity where
    Min 'Total 'Total = 'Total
    Min a b = 'Partial

joinRes :: Res t0 (Res t1 a) -> Res (t0 `Min` t1) a
joinRes (FullRes (FullRes a)) = FullRes a
joinRes (FullRes EmptyRes) = EmptyRes
joinRes EmptyRes = EmptyRes

zipRes :: (a -> b -> c) -> Res t0 a -> Res t1 b -> Res (t0 `Min` t1) c
zipRes f (FullRes a) (FullRes b) = pure (f a b)
zipRes _ EmptyRes _ = EmptyRes
zipRes _ _ EmptyRes = EmptyRes

zipRes3 :: (a -> b -> c -> d) -> Res t0 a -> Res t1 b -> Res t2 c -> Res (t0 `Min` t1 `Min` t2) d
zipRes3 f a = zipRes id . zipRes f a

zipRes4 :: (a -> b -> c -> d -> e) -> Res t0 a -> Res t1 b -> Res t2 c -> Res t3 d-> Res (t0 `Min` t1 `Min` t2 `Min` t3) e
zipRes4 f a b = zipRes id . zipRes3 f a b


(.?) :: (b -> Res t1 c) -> (a -> Res t0 b) -> a -> Res (t0 `Min` t1) c
(.?) f g a = f =<<? g a

(=<<?) :: (a -> Res t1 b) -> Res t0 a -> Res (t0 `Min` t1) b
(=<<?) f = joinRes . map f

(?>>=) :: Res t0 a -> (a -> Res t1 b) -> Res (t0 `Min` t1) b
(?>>=) = flip (=<<?)
