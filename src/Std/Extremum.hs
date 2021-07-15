{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Extremum where

import "base" Prelude qualified as Base

import "ghc-prim" GHC.Prim ( proxy# )

import "this" Std.Debug
import "this" Std.Generic
import "this" Std.Ord
import "this" Std.Group


data Extremum
    = Minimum
    | Maximum
  deriving stock (Generic, Eq, Base.Ord, Show)
  deriving Ord via (Generically Extremum)
  deriving (BinOp 'Minimum, BinOp 'Maximum) via (Ordered Extremum)

max :: BinOp 'Maximum a => a -> a -> OpRes 'Maximum a
max = op# (proxy# @'Maximum)
min :: BinOp 'Minimum a => a -> a -> OpRes 'Minimum a
min = op# (proxy# @'Minimum)

instance Ord a => BinOp         'Minimum (Ordered a) where op# _ = min
instance Ord a => AssociativeOp 'Minimum (Ordered a)
instance Ord a => Idempotent    'Minimum (Ordered a)
instance Ord a => BinOp         'Maximum (Ordered a) where op# _ = max
instance Ord a => AssociativeOp 'Maximum (Ordered a)
instance Ord a => Idempotent    'Maximum (Ordered a)

deriving via (Ordered Base.Int) instance BinOp         'Minimum Base.Int
deriving via (Ordered Base.Int) instance AssociativeOp 'Minimum Base.Int
deriving via (Ordered Base.Int) instance Idempotent    'Minimum Base.Int
deriving via (Ordered Base.Int) instance BinOp         'Maximum Base.Int
deriving via (Ordered Base.Int) instance AssociativeOp 'Maximum Base.Int
deriving via (Ordered Base.Int) instance Idempotent    'Maximum Base.Int

deriving via (Ordered Base.Integer) instance BinOp         'Minimum Base.Integer
deriving via (Ordered Base.Integer) instance AssociativeOp 'Minimum Base.Integer
deriving via (Ordered Base.Integer) instance Idempotent    'Minimum Base.Integer
deriving via (Ordered Base.Integer) instance BinOp         'Maximum Base.Integer
deriving via (Ordered Base.Integer) instance AssociativeOp 'Maximum Base.Integer
deriving via (Ordered Base.Integer) instance Idempotent    'Maximum Base.Integer

deriving via (Ordered Base.Bool) instance BinOp         'Minimum Base.Bool
deriving via (Ordered Base.Bool) instance AssociativeOp 'Minimum Base.Bool
deriving via (Ordered Base.Bool) instance Idempotent    'Minimum Base.Bool
deriving via (Ordered Base.Bool) instance BinOp         'Maximum Base.Bool
deriving via (Ordered Base.Bool) instance AssociativeOp 'Maximum Base.Bool
deriving via (Ordered Base.Bool) instance Idempotent    'Maximum Base.Bool
