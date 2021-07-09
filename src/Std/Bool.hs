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
import "this" Std.Partial
import "this" Std.Ord
import "this" Std.Basic


data BoolOp
    = And
    | Or
    | Xor
    | Impl
    | Nand

class    BinOp 'And a => And a
instance BinOp 'And a => And a
infixr 3 &&
(&&) :: And a => a -> a -> OpRes 'And a
(&&) = op# (proxy# @'And)
class    InvertibleOp 'And a => Nand a
instance InvertibleOp 'And a => Nand a
infixr 3 /&
(/&) :: Nand a => a -> a -> InvOpRes 'And a
(/&) = invOp# (proxy# @'And)
class    BinOp 'Or a => Or a
instance BinOp 'Or a => Or a
infixr 2 ||
(||) :: Or a => a -> a -> OpRes 'Or a
(||) = op# (proxy# @'Or)
class    InvertibleOp 'Or a => Nor a
instance InvertibleOp 'Or a => Nor a
infixr 2 /|
(/|) :: Nor a => a -> a -> InvOpRes 'Or a
(/|) = invOp# (proxy# @'Or)
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
    type OpTotallity 'And Bool = 'Total
    op# _ = (Base.&&)
instance IdentityOp 'And Bool where
    identity# _ = True
instance AssociativeOp 'And Bool
instance Idempotent 'And Bool
instance Commutative 'And Bool
instance InvertibleOp 'And Bool where
    type InvOpTotallity 'And Bool = 'Total
    inv# _ = Base.not
    invOp# _ True True = False
    invOp# _ _    _    = True
instance BinOp 'Or Bool where
    type OpTotallity 'Or Bool = 'Total
    op# _ = (Base.||)
instance AssociativeOp 'Or Bool
instance Idempotent 'Or Bool
instance Commutative 'Or Bool
instance IdentityOp 'Or Bool where
    identity# _ = False
instance InvertibleOp 'Or Bool where
    type InvOpTotallity 'Or Bool = 'Total
    inv# _ = Base.not
    invOp# _ False False = True
    invOp# _ _     _     = False

instance DistributiveOp 'Or 'And Bool
instance DistributiveOp 'And 'Or Bool

instance BinOp 'Xor Bool where
    type OpTotallity 'Xor Bool = 'Total
    op# _ = Base.xor
instance AssociativeOp 'Xor Bool
instance Commutative 'Xor Bool

instance BinOp 'Impl Bool where
    type OpTotallity 'Impl Bool = 'Total
    op# _ True  True = True
    op# _ False _    = True
    op# _ _     _    = False

deriving via (Basic Bool) instance Ord' 'Total Bool

deriving via (Monoidal Ordering) instance BinOp       'And Ordering
deriving via (Monoidal Ordering) instance IdentityOp  'And Ordering
instance AssociativeOp 'And Ordering
instance Idempotent    'And Ordering
instance Commutative   'And Ordering
