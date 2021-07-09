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
    | Nor

class    BinOp t 'And a => And t a
instance BinOp t 'And a => And t a
infixr 3 &&
(&&) :: And t a => a -> a -> DirectRes t a
(&&) = op# (proxy# @'And)
class    BinOp t 'Nand a => Nand t a
instance BinOp t 'Nand a => Nand t a
infixr 3 /&
(/&) :: Nand t a => a -> a -> DirectRes t a
(/&) = op# (proxy# @'Nand)
class    BinOp t 'Or a => Or t a
instance BinOp t 'Or a => Or t a
infixr 2 ||
(||) :: Or t a => a -> a -> DirectRes t a
(||) = op# (proxy# @'Or)
class    BinOp t 'Nor a => Nor t a
instance BinOp t 'Nor a => Nor t a
infixr 2 /|
(/|) :: Nor t a => a -> a -> DirectRes t a
(/|) = op# (proxy# @'Nor)
class    BinOp t 'Xor a => Xor t a
instance BinOp t 'Xor a => Xor t a
infixl 6 `xor`
xor :: Xor t a => a -> a -> DirectRes t a
xor = op# (proxy# @'Xor)
class    BinOp t 'Impl a => Impl t a
instance BinOp t 'Impl a => Impl t a
infixr 0 ==>
(==>) :: Impl t a => a -> a -> DirectRes t a
(==>) = op# (proxy# @'Impl)

instance BinOp 'Total 'And Bool where
    op# _ = (Base.&&)
instance IdentityOp 'And Bool where
    identity# _ = True
instance AssociativeOp 'And Bool
instance Idempotent 'And Bool
instance Commutative 'And Bool
instance Invertible 'Total 'And Bool where
    inv# _ = Base.not
instance BinOp 'Total 'Or Bool where
    op# _ = (Base.||)
instance AssociativeOp 'Or Bool
instance Idempotent 'Or Bool
instance Commutative 'Or Bool
instance IdentityOp 'Or Bool where
    identity# _ = False
instance DistributiveOp 'Or 'And Bool
instance DistributiveOp 'And 'Or Bool
instance Invertible 'Total 'Or Bool where
    inv# _ = Base.not
instance BinOp 'Total 'Xor Bool where
    op# _ = Base.xor
instance AssociativeOp 'Xor Bool
instance Commutative 'Xor Bool
instance BinOp 'Total 'Nand Bool where
    op# _ True True = False
    op# _ _ _ = True
instance IdentityOp 'Nand Bool where identity# _ = False
instance AssociativeOp 'Nand Bool
instance Commutative 'Nand Bool
instance BinOp 'Total 'Nor Bool where
    op# _ False False = True 
    op# _ _ _ = False
instance AssociativeOp 'Nor Bool
instance Commutative 'Nor Bool
instance IdentityOp 'Nor Bool where
    identity# _ = False
instance DistributiveOp 'Nand 'Nor Bool
instance DistributiveOp 'Nor 'Nand Bool
instance Invertible 'Total 'Nand Bool where
    inv# _ = Base.not
instance Invertible 'Total 'Nor Bool where
    inv# _ = Base.not 

instance BinOp 'Total 'Impl Bool where
    op# _ True  True = True
    op# _ False _    = True
    op# _ _     _    = False

deriving via (Basic Bool) instance Ord' 'Total Bool

deriving via (Monoidal Ordering) instance BinOp 'Total       'And Ordering
deriving via (Monoidal Ordering) instance AssociativeOp 'And Ordering
deriving via (Monoidal Ordering) instance Idempotent    'And Ordering
deriving via (Monoidal Ordering) instance Commutative   'And Ordering
deriving via (Monoidal Ordering) instance IdentityOp    'And Ordering
