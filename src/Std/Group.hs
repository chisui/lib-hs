{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Std.Group where

import "base" Prelude qualified as Base
import "base" Control.Applicative qualified as Base

import "ghc-prim" GHC.Prim ( Proxy#, proxy# )

import "this" Std.Partial
import "this" Std.Basic


class BinOp (op :: k) a b | op a -> b where
    op :: proxy op -> a -> a -> b
    op _ = op# (proxy# @op)
    op# :: Proxy# op -> a -> a -> b

data BasicOps
    = Add
    | Sub
    | Mult
    | Div
    | Mod
type Add = BinOp 'Add
(+) :: forall a b. Add a b => a -> a -> b
(+) = op# (proxy# @'Add)
type Sub = BinOp 'Sub
(-) :: forall a b. Sub a b => a -> a -> b
(-) = op# (proxy# @'Sub)
type Mult = BinOp 'Mult
(*) :: forall a b. Mult a b => a -> a -> b
(*) = op# (proxy# @'Mult)
type Div = BinOp 'Div
(/) :: forall a b. Div a b => a -> a -> b
(/) = op# (proxy# @'Div)
type Mod = BinOp 'Mod
(%) :: forall a b. Mod a b => a -> a -> b
(%) = op# (proxy# @'Mod)


class BinOp op a a => Magma (op :: k) a

class Magma op a => Identity (op :: k) a where
    identity :: proxy op -> a
    identity _ = identity# (proxy# @op)
    identity# :: Proxy# op -> a

class Magma op a => Associative (op :: k) a

class Magma op a => Invertible (op :: k) a where
    inv :: proxy op -> a -> a
    inv _ = inv# (proxy# @op)
    inv# :: Proxy# op -> a -> a

zero :: Identity 'Add a => a
zero = identity# (proxy# @'Add)
negate :: Invertible 'Add a => a -> a
negate = inv# (proxy# @'Add)

one :: Identity 'Mult a => a
one = identity# (proxy# @'Mult)
recip :: Invertible 'Mult a => a -> a
recip = inv# (proxy# @'Mult)


class Magma op a => Commutative (op :: k) a

class Magma op a => Idempotent (op :: k) a

class Magma op a => Upper (op :: k) a where
    top :: proxy op -> a
    top _ = top# (proxy# @op)
    top# :: Proxy# op -> a

class Magma op a => Lower (op :: k) a where
    bot :: proxy op -> a
    bot _ = bot# (proxy# @op)
    bot# :: Proxy# op -> a
    

type Bounded op a = (Upper op a, Lower op a)

type Quasigroup = Invertible
type UnitalMagma = Identity
type Semigroup = Associative
type Loop op a = (Quasigroup op a, Identity op a)
type InverseSemigroup op a = (Semigroup op a, Invertible op a)
type Monoid op a = (Semigroup op a, Identity op a)
type Group op a = (Identity op a, Associative op a, Invertible op a)
type Abelian op a = (Group op a, Commutative op a)

type Semilattice op a = (Associative op a, Commutative op a, Idempotent op a)


class (Magma f a, Magma g a) => LeftDistributive f g a
class (Magma f a, Magma g a) => RightDistributive f g a


type Ring f g a =
    ( Abelian f a
    , Monoid g a
    , LeftDistributive f g a
    , RightDistributive f g a
    )

type CommutativeRing f g a =
    ( Ring f g a
    , Commutative g a
    )

type Field f g a = 
    ( CommutativeRing f g a
    , Abelian g a
    , Invertible f a, Invertible f a
    )

-- instances

instance Base.Num a => BinOp 'Add (Numeric a) (Numeric a) where
    op# _ = coerce ((Base.+) @a)
instance Base.Num a => Magma 'Add (Numeric a)
instance Base.Num a => Associative 'Add (Numeric a)

instance Base.Num a => BinOp 'Sub (Numeric a) (Numeric a) where
    op# _ = coerce ((Base.-) @a)
instance Base.Num a => Magma 'Sub (Numeric a)

instance Base.Num a => BinOp 'Mult (Numeric a) (Numeric a) where
    op# _ = coerce ((Base.*) @a)
instance Base.Num a => Magma 'Mult (Numeric a)

instance Base.Integral a => BinOp 'Div (PartialNumeric a) (Res 'Partial (PartialNumeric a)) where
    op# _ = errorToPartial2 (Base.div @a)

instance Base.Integral a => BinOp 'Mod (Numeric a) (Numeric a) where
    op# _ = coerce (Base.mod @a)

instance Base.Num a => Invertible 'Add (Numeric a) where
    inv# _ = coerce (Base.negate @a)
instance Base.Num a => Identity 'Add (Numeric a) where
    identity# _ = coerce (0 :: a)
instance Base.Num a => Commutative 'Add (Numeric a)

instance Base.Num a => Associative 'Mult (Numeric a)
instance Base.Num a => Identity 'Mult (Numeric a) where
    identity# _ = coerce (1 :: a)
instance Base.Num a => Commutative 'Mult (Numeric a)

instance Base.Num a => LeftDistributive 'Mult 'Add (Numeric a)
instance Base.Num a => RightDistributive 'Mult 'Add (Numeric a)


instance BinOp 'Add Base.Int Base.Int where op# _ = (Base.+)
instance BinOp 'Sub Base.Int Base.Int where op# _ = (Base.-)
deriving via (Numeric Base.Int) instance Magma 'Add Base.Int
deriving via (Numeric Base.Int) instance Associative 'Add Base.Int
deriving via (Numeric Base.Int) instance Invertible 'Add Base.Int
deriving via (Numeric Base.Int) instance Identity 'Add Base.Int
deriving via (Numeric Base.Int) instance Commutative 'Add Base.Int
instance BinOp 'Mult Base.Int Base.Int where op# _ = (Base.*)
instance BinOp 'Div Base.Int (Res 'Partial Base.Int) where op# _ = errorToPartial2 (Base.div @Base.Int)

deriving via (Numeric Base.Int) instance Magma 'Mult Base.Int
deriving via (Numeric Base.Int) instance Associative 'Mult Base.Int
deriving via (Numeric Base.Int) instance Identity 'Mult Base.Int
deriving via (Numeric Base.Int) instance LeftDistributive 'Mult 'Add Base.Int
deriving via (Numeric Base.Int) instance RightDistributive 'Mult 'Add Base.Int

instance BinOp 'Add Base.Integer Base.Integer where op# _ = (Base.+)
instance BinOp 'Sub Base.Integer Base.Integer where op# _ = (Base.-)
deriving via (Numeric Base.Integer) instance Magma 'Add Base.Integer
deriving via (Numeric Base.Integer) instance Associative 'Add Base.Integer
deriving via (Numeric Base.Integer) instance Invertible 'Add Base.Integer
deriving via (Numeric Base.Integer) instance Identity 'Add Base.Integer
deriving via (Numeric Base.Integer) instance Commutative 'Add Base.Integer
instance BinOp 'Mult Base.Integer Base.Integer where op# _ = (Base.*)
instance BinOp 'Div Base.Integer (Res 'Partial Base.Integer) where op# _ = errorToPartial2 (Base.div @Base.Integer)

deriving via (Numeric Base.Integer) instance Magma 'Mult Base.Integer
deriving via (Numeric Base.Integer) instance Associative 'Mult Base.Integer
deriving via (Numeric Base.Integer) instance Identity 'Mult Base.Integer
deriving via (Numeric Base.Integer) instance LeftDistributive 'Mult 'Add Base.Integer
deriving via (Numeric Base.Integer) instance RightDistributive 'Mult 'Add Base.Integer

instance Base.Semigroup a => BinOp 'Add (Monoidal a) (Monoidal a) where
    op# _ = coerce ((Base.<>) :: a -> a -> a)
instance Base.Semigroup a => Magma 'Add (Monoidal a)
instance Base.Semigroup a => Associative 'Add (Monoidal a)
instance Base.Monoid a => Identity 'Add (Monoidal a) where
    identity# _ = coerce (Base.mempty :: a)


instance Base.Alternative f => BinOp 'Add (Basic1 f a) (Basic1 f a) where
    op# _ = coerce ((Base.<|>) :: f a -> f a -> f a)
instance Base.Alternative f => Magma 'Add (Basic1 f a)
instance Base.Alternative f => Associative 'Add (Basic1 f a)
instance Base.Alternative f => Identity 'Add (Basic1 f a) where
    identity# _ = coerce (Base.empty :: f a)

instance BinOp 'Add [a] [a] where op# _ = (Base.<>)
deriving via (Basic1 [] a) instance Magma 'Add [a]
deriving via (Basic1 [] a) instance Associative 'Add [a]
deriving via (Basic1 [] a) instance Identity 'Add [a]
