{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Literal
    ( FromInteger(..), ToInteger(..), IsInteger
    , FromString(..)
    , FromInt(..)
    , FromList(..), ToList(..), IsList
    , toEnum, fromEnum
    , MinBound(..), MaxBound(..), Bounded
    , Pred(..), Succ(..)
    , Coercible, coerce
    ) where

import "base" Prelude qualified as Base
import "base" Data.String qualified as Base
import "base" GHC.Exts qualified as Base
import "base" Prelude ( Int, Integer, String )

import "this" Std.Partial
import "this" Std.Basic
import "this" Std.Cat


class FromInteger t a | a -> t where
    fromInteger :: Integer -> Res t a

class ToInteger t a | a -> t where
    toInteger :: a -> Res t Integer

class (FromInteger 'Total a, ToInteger 'Total a) => IsInteger a
instance (FromInteger 'Total a, ToInteger 'Total a) => IsInteger a

class FromString t a | a -> t where
    fromString :: String -> Res t a

class FromInt t a | a -> t where
    fromInt :: Int -> Res t a


class HasItems l where
    type Item l

class HasItems l => FromList (t :: Totallity) l | l -> t where
    fromList :: [Item l] -> Res t l

class HasItems l => ToList (t :: Totallity) l | l -> t where
    toList :: l -> Res t [Item l]

class (FromList 'Total t, ToList 'Total t) => IsList t
instance (FromList 'Total t, ToList 'Total t) => IsList t

-- Bridge for rebindable syntax
toEnum :: forall a t. FromInt t a => Int -> Res t a
toEnum = fromInt @t @a

class ToInt t a | a -> t where
    toInt :: a -> Res t Int

-- Bridge for rebindable syntax
fromEnum :: ToInt t a => a -> Res t Int
fromEnum = toInt

class MinBound a where
    minBound :: a

class MaxBound a where
    maxBound :: a

class (MinBound a, MaxBound a) => Bounded a
instance (MinBound a, MaxBound a) => Bounded a

class Pred t a | a -> t where
    pred :: a -> Res t a

class Succ t a | a -> t where
    succ :: a -> Res t a

-- instances 


instance Base.Num a => FromInteger 'Total (Basic a) where
    fromInteger = pure . coerce (Base.fromInteger :: Integer -> a)

instance Base.Integral a => ToInteger 'Total (Basic a) where
    toInteger = pure . coerce (Base.toInteger :: a -> Integer)

instance Base.IsString a => FromString 'Total (Basic a) where
    fromString = pure . coerce (Base.fromString :: String -> a)

instance Base.Enum a => FromInt 'Total (Basic a) where
    fromInt = pure . coerce (Base.toEnum :: Int -> a)
instance Base.Enum a => Pred 'Total (Basic a) where
    pred = pure . coerce (Base.pred :: a -> a)
instance Base.Enum a => Succ 'Total (Basic a) where
    succ = pure . coerce (Base.succ :: a -> a)

instance Base.Bounded a => MinBound (Basic a) where
    minBound = coerce @a Base.minBound
instance Base.Bounded a => MaxBound (Basic a) where
    maxBound = coerce @a Base.maxBound


instance Base.Num a => FromInteger 'Partial (Unsafe a) where
    fromInteger = errorToPartial1 @(Base.Integer -> a) Base.fromInteger

instance Base.Integral a => ToInteger 'Partial (Unsafe a) where
    toInteger = errorToPartial1 @(a -> Base.Integer) Base.toInteger

instance Base.IsString a => FromString 'Partial (Unsafe a) where
    fromString = errorToPartial1 @(Base.String -> a) Base.fromString

instance Base.Enum a => FromInt 'Partial (Unsafe a) where
    fromInt = errorToPartial1 @(Base.Int -> a) Base.toEnum
instance Base.Enum a => ToInt 'Partial (Unsafe a) where
    toInt = errorToPartial1 @(a -> Base.Int) Base.fromEnum
instance Base.Enum a => Pred 'Partial (Unsafe a) where
    pred = errorToPartial1 @(a -> a) Base.pred
instance Base.Enum a => Succ 'Partial (Unsafe a) where
    succ = errorToPartial1 @(a -> a) Base.succ

instance Base.IsList l => HasItems (Basic l) where
    type Item (Basic l) = Base.Item l
instance Base.IsList l => FromList 'Total (Basic l) where
    fromList = pure . coerce (Base.fromList @l)
instance Base.IsList l => ToList 'Total (Basic l) where
    toList = pure . coerce (Base.toList @l)

instance IsList l => Base.IsList (Basic l) where
    type Item (Basic l) = Item l
    fromList = total . coerce (Std.Literal.fromList @_ @l)
    toList = total . coerce (Std.Literal.toList @'Total @l)

instance Base.IsList l => HasItems (Unsafe l) where
    type Item (Unsafe l) = Base.Item l
instance Base.IsList l => FromList 'Partial (Unsafe l) where
    fromList = errorToPartial1 (Base.fromList @l)
instance Base.IsList l => ToList 'Partial (Unsafe l) where
    toList = errorToPartial1 (Base.toList @l)

-- use to derive instances for basic datatypes

deriving via (Unsafe Base.Bool) instance FromInt 'Partial Base.Bool
deriving via (Unsafe Base.Bool) instance ToInt 'Partial Base.Bool

deriving via (Basic [a]) instance HasItems [a]
deriving via (Basic [a]) instance FromList 'Total [a]
deriving via (Basic [a]) instance ToList 'Total [a]
