{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Bool
    ( Bool(..)
    , BoolOp(..)
    , (&&), And
    , (||), Or
    , (/&), Nand
    , (/|), Nor
    , (==>), Impl
    , xor, Xor
    , Base.not, Base.otherwise, Base.bool
    ) where

import "base" Data.Bool qualified as Base
import "base" Data.Bits qualified as Base

import "ghc-prim" GHC.Prim ( proxy# )

import "this" Std.BinOp
import "this" Std.Group
import "this" Std.Ord
import "this" Std.Basic


data BoolOp
    = And
    | Or
    | Xor
    | Impl
    | Nand
    | Nor

class    BinOp 'And a => And a
instance BinOp 'And a => And a
infixr 3 &&
(&&) :: And a => a -> a -> OpRes 'And a
(&&) = op# (proxy# @'And)
class    BinOp 'Nand a => Nand a
instance BinOp 'Nand a => Nand a
infixr 3 /&
(/&) :: Nand a => a -> a -> OpRes 'Nand a
(/&) = op# (proxy# @'Nand)
class    BinOp 'Or a => Or a
instance BinOp 'Or a => Or a
infixr 2 ||
(||) :: Or a => a -> a -> OpRes 'Or a
(||) = op# (proxy# @'Or)
class    BinOp 'Nor a => Nor a
instance BinOp 'Nor a => Nor a
infixr 2 /|
(/|) :: Nor a => a -> a -> OpRes 'Nor a
(/|) = op# (proxy# @'Nor)
class    BinOp 'Xor a => Xor a
instance BinOp 'Xor a => Xor a
infixl 6 `xor`
xor :: Xor a => a -> a -> OpRes 'Xor a
xor = op# (proxy# @'Xor)
class    BinOp 'Impl a => Impl a
instance BinOp 'Impl a => Impl a
infixr 0 ==>
(==>) :: Impl a => a -> a -> OpRes 'Impl a
(==>) = op# (proxy# @'Impl)

instance BinOp 'And Bool where
    op# _ = (Base.&&)
instance IdentityOp 'And Bool where
    identity# _ = True
instance AssociativeOp 'And Bool
instance Idempotent 'And Bool
instance Commutative 'And Bool
instance BinOp 'Nand Bool where
    op# _ True True = False
    op# _ _    _    = True
instance InverseOp 'And Bool where
    type InvOp 'And Bool = 'Nand
    inv# _ = Base.not
instance InverseOp 'Nand Bool where
    type InvOp 'Nand Bool = 'And
    inv# _ = Base.not
instance BinOp 'Or Bool where
    op# _ = (Base.||)
instance AssociativeOp 'Or Bool
instance Idempotent 'Or Bool
instance Commutative 'Or Bool
instance IdentityOp 'Or Bool where
    identity# _ = False
instance BinOp 'Nor Bool where
    op# _ False False = True
    op# _ _     _     = False
instance InverseOp 'Or Bool where
    type InvOp 'Or Bool = 'Nor
    inv# _ = Base.not
instance InverseOp 'Nor Bool where
    type InvOp 'Nor Bool = 'Or
    inv# _ = Base.not

instance DistributiveOp 'Or 'And Bool
instance DistributiveOp 'And 'Or Bool

instance BinOp 'Xor Bool where
    op# _ = Base.xor
instance AssociativeOp 'Xor Bool
instance Commutative 'Xor Bool

instance BinOp 'Impl Bool where
    op# _ True  True = True
    op# _ False _    = True
    op# _ _     _    = False

deriving via (Monoidal Ordering) instance BinOp       'Canonic Ordering
deriving via (Monoidal Ordering) instance IdentityOp  'Canonic Ordering
instance AssociativeOp 'Canonic Ordering
instance Idempotent    'Canonic Ordering
instance Commutative   'Canonic Ordering
