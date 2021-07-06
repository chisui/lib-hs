{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Bool
    ( Base.Bool(..)
    , BoolOp(..)
    , (&&), And
    , (||), Or
    , (/&), Nand
    , (/|), Nor
    , (==>), Impl
    , xor, Xor
    , Base.not, Base.otherwise, Base.bool
    ) where

import "base" Data.Semigroup qualified as Base
import "base" Data.Bool qualified as Base
import "base" Data.Bits qualified as Base

import "ghc-prim" GHC.Prim ( proxy# )

import "this" Std.Group
import "this" Std.Ord
import "this" Std.Basic


data BoolOp
    = And
    | Nand
    | Or
    | Nor
    | Xor
    | Impl

type And = BinOp 'And
infixr 3 &&
(&&) :: And a b => a -> a -> b
(&&) = op# (proxy# @'And)
type Nand = BinOp 'Nand
infixr 3 /&
(/&) :: Nand a b => a -> a -> b
(/&) = op# (proxy# @'Nand)
type Or = BinOp 'Or
infixr 2 ||
(||) :: Or a b => a -> a -> b
(||) = op# (proxy# @'Or)
type Nor = BinOp 'Nor
infixr 2 /|
(/|) :: Nor a b => a -> a -> b
(/|) = op# (proxy# @'Nor)
type Xor = BinOp 'Xor
infixl 6 `xor`
xor :: Xor a b => a -> a -> b
xor = op# (proxy# @'Xor)
type Impl = BinOp 'Impl
infixr 0 ==>
(==>) :: Impl a b => a -> a -> b
(==>) = op# (proxy# @'Impl)


instance BinOp       'And Base.Bool Base.Bool where op# _ = (Base.&&)
instance Magma       'And Base.Bool
instance IdentityOp  'And Base.Bool where identity# _ = Base.True
instance Associative 'And Base.Bool
instance Idempotent  'And Base.Bool
instance Commutative 'And Base.Bool
instance BinOp       'Or  Base.Bool Base.Bool where op# _ = (Base.||)
instance Magma       'Or  Base.Bool
instance Associative 'Or  Base.Bool
instance Idempotent  'Or  Base.Bool
instance Commutative 'Or  Base.Bool
instance IdentityOp  'Or  Base.Bool where identity# _ = Base.False
instance BinOp       'Xor Base.Bool Base.Bool where op# _ = Base.xor
instance LeftDistributive  'And 'Or Base.Bool
instance RightDistributive 'And 'Or Base.Bool
instance LeftDistributive  'Or 'And Base.Bool
instance RightDistributive 'Or 'And Base.Bool
instance BinOp       'Nand Base.Bool Base.Bool where
    op# _ Base.True Base.True = Base.False
    op# _ _ _ = Base.True
instance Magma       'Nand Base.Bool
instance IdentityOp  'Nand Base.Bool where identity# _ = Base.False
instance Associative 'Nand Base.Bool
instance Commutative 'Nand Base.Bool
instance BinOp       'Nor  Base.Bool Base.Bool where
    op# _ Base.False Base.False = Base.True 
    op# _ _ _ = Base.False
instance Magma       'Nor  Base.Bool
instance Associative 'Nor  Base.Bool
instance Commutative 'Nor  Base.Bool
instance IdentityOp  'Nor  Base.Bool where identity# _ = Base.False
instance LeftDistributive  'Nand 'Nor Base.Bool
instance RightDistributive 'Nand 'Nor Base.Bool
instance LeftDistributive  'Nor 'Nand Base.Bool
instance RightDistributive 'Nor 'Nand Base.Bool
instance Invertible 'And  Base.Bool where inv# _ = Base.not
instance Invertible 'Or   Base.Bool where inv# _ = Base.not
instance Invertible 'Nand Base.Bool where inv# _ = Base.not
instance Invertible 'Nor  Base.Bool where inv# _ = Base.not
instance Magma       'Xor Base.Bool
instance Associative 'Xor Base.Bool
instance Commutative 'Xor Base.Bool

instance BinOp       'Impl Base.Bool Base.Bool where
    op# _ Base.True Base.True = Base.True 
    op# _ Base.False _ = Base.True
    op# _ _ _ = Base.False
instance Magma       'Impl Base.Bool


deriving via (Basic Base.Bool) instance Eq 'Total Base.Bool
deriving via (Basic Base.Bool) instance Ord 'Total Base.Bool

instance BinOp 'And Ordering Ordering where op# _ = (Base.<>)
deriving via (Monoidal Ordering) instance Magma       'And Ordering
deriving via (Monoidal Ordering) instance Associative 'And Ordering
deriving via (Monoidal Ordering) instance Idempotent  'And Ordering
deriving via (Monoidal Ordering) instance Commutative 'And Ordering
deriving via (Monoidal Ordering) instance IdentityOp  'And Ordering
