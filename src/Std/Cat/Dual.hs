{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Cat.Dual
    ( Dual(..)
    ) where

import "base" Data.Monoid ( Dual(..) )

import "ghc-prim" GHC.Prim ( Proxy#, proxy# )

import "this" Std.Cat
import "this" Std.Group


instance TotalBinOp 'Canonic a => BinOp 'Canonic (Dual a) where
    op# p a b = to coerce (op# p :: a -> a -> OpRes 'Canonic a) a b
instance TotalBinOp op a => BinOp ('Dual op) a where
    op# _ a b = op# (proxy# @op) a b

instance AssociativeOp 'Canonic a => AssociativeOp 'Canonic (Dual a)
instance Monoid 'Canonic a => IdentityOp 'Canonic (Dual a) where
    identity# = to coerce (identity# :: Proxy# 'Canonic -> a)
