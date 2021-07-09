{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Literal
    ( FromInteger(..), ToInteger(..), IsInteger
    , FromString(..)
    , FromInt(..)
    , HasItems(..), FromList(..), ToList(..), IsList
    , toEnum, fromEnum
    , MinBound(..), MaxBound(..), Bounded
    , Pred(..), Succ(..)
    ) where

import "base" Prelude qualified as Base
import "base" Data.String qualified as Base
import "base" GHC.Exts qualified as Base
import "base" Prelude ( Int, Integer, String )

import "this" Std.Partial
import "this" Std.Basic
import "this" Std.Cat


class FromInteger a where
    fromInteger :: Integer -> a

class ToInteger t a | a -> t where
    toInteger :: a -> Res t Integer

class (FromInteger a, ToInteger 'Total a) => IsInteger a
instance (FromInteger a, ToInteger 'Total a) => IsInteger a

class FromString a where
    fromString :: String -> a

class FromInt t a | a -> t where
    fromInt :: Int -> Res t a


class HasItems l where
    type Item l

class HasItems l => FromList l where
    fromList :: [Item l] -> l

class HasItems l => ToList (t :: Totallity) l | l -> t where
    toList :: l -> Res t [Item l]

class (FromList t, ToList 'Total t) => IsList t
instance (FromList t, ToList 'Total t) => IsList t

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


instance Base.Num a => FromInteger (Basic a) where
    fromInteger = to coerce (Base.fromInteger :: Integer -> a)

instance Base.Integral a => ToInteger 'Total (Basic a) where
    toInteger = pure . to coerce (Base.toInteger :: a -> Integer)

instance Base.IsString a => FromString (Basic a) where
    fromString = to coerce (Base.fromString :: String -> a)

instance Base.Enum a => FromInt 'Total (Basic a) where
    fromInt = pure . to coerce (Base.toEnum :: Int -> a)
instance Base.Enum a => ToInt 'Total (Basic a) where
    toInt = pure . to coerce (Base.fromEnum :: a -> Int)
instance Base.Enum a => Pred 'Total (Basic a) where
    pred = pure . to coerce (Base.pred :: a -> a)
instance Base.Enum a => Succ 'Total (Basic a) where
    succ = pure . to coerce (Base.succ :: a -> a)

instance Base.Bounded a => MinBound (Basic a) where
    minBound = to coerce (Base.minBound :: a)
instance Base.Bounded a => MaxBound (Basic a) where
    maxBound = to coerce (Base.maxBound :: a)


instance Base.Integral a => ToInteger 'Partial (Unsafe a) where
    toInteger = errorToPartial1 @(a -> Base.Integer) Base.toInteger

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
instance Base.IsList l => FromList (Basic l) where
    fromList = to coerce (Base.fromList @l)
instance Base.IsList l => ToList 'Total (Basic l) where
    toList = pure . to coerce (Base.toList @l)

instance IsList l => Base.IsList (Basic l) where
    type Item (Basic l) = Item l
    fromList = to coerce (Std.Literal.fromList @l)
    toList = total . to coerce (Std.Literal.toList @'Total @l)

instance Base.IsList l => HasItems (Unsafe l) where
    type Item (Unsafe l) = Base.Item l

-- use to derive instances for basic datatypes

deriving via (Unsafe Base.Bool) instance FromInt 'Partial Base.Bool
deriving via (Unsafe Base.Bool) instance ToInt 'Partial Base.Bool

deriving via (Basic [a]) instance HasItems [a]
deriving via (Basic [a]) instance FromList [a]
deriving via (Basic [a]) instance ToList 'Total [a]

deriving via (Basic  Base.Integer) instance FromInteger Base.Integer
deriving via (Basic  Base.Integer) instance FromInt     'Total   Base.Integer
deriving via (Unsafe Base.Integer) instance ToInt       'Partial Base.Integer
deriving via (Basic  Base.Integer) instance ToInteger   'Total   Base.Integer
deriving via (Basic  Base.Integer) instance Pred        'Total   Base.Integer
deriving via (Basic  Base.Integer) instance Succ        'Total   Base.Integer


deriving via (Basic  Base.Int) instance FromInteger  Base.Int
deriving via (Basic  Base.Int) instance FromInt     'Total   Base.Int
deriving via (Basic  Base.Int) instance ToInt       'Total   Base.Int
deriving via (Basic  Base.Int) instance ToInteger   'Total   Base.Int
deriving via (Basic  Base.Int) instance Pred        'Total   Base.Int
deriving via (Basic  Base.Int) instance Succ        'Total   Base.Int

