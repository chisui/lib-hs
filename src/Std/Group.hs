{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Group where

import "base" Prelude qualified as Base

import "ghc-prim" GHC.Prim ( Proxy#, proxy# )

import "this" Std.BinOp
import "this" Std.Cat


class    TotalBinOp op a => Magma op a
instance TotalBinOp op a => Magma op a

class Magma op a => AssociativeOp (op :: k) a

class    TotalInvertibleOp op a => Quasigroup op a
instance TotalInvertibleOp op a => Quasigroup op a


class Magma op a => Commutative (op :: k) a

class Magma op a => Idempotent (op :: k) a

class Magma op a => Upper (op :: k) a where
    top :: proxy op -> a
    top _ = top# (proxy# @op)
    top# :: Proxy# op -> a
    top# _ = top (Proxy @op)
    {-# MINIMAL top | top# #-}

class Magma op a => Lower (op :: k) a where
    bot :: proxy op -> a
    bot _ = bot# (proxy# @op)
    bot# :: Proxy# op -> a
    bot# _ = bot (Proxy @op)
    {-# MINIMAL bot | bot# #-}

class    (Magma op a, IdentityOp op a) => UnitalMagma op a
instance (Magma op a, IdentityOp op a) => UnitalMagma op a

class    AssociativeOp op a => Semigroup op a
instance AssociativeOp op a => Semigroup op a

class    (Quasigroup op a, IdentityOp op a) => Loop op a
instance (Quasigroup op a, IdentityOp op a) => Loop op a

class    (Semigroup op a, Quasigroup op a) => InverseSemigroup op a
instance (Semigroup op a, Quasigroup op a) => InverseSemigroup op a

class    (Semigroup op a, IdentityOp op a) => Monoid op a
instance (Semigroup op a, IdentityOp op a) => Monoid op a

class    (IdentityOp op a, AssociativeOp op a, Quasigroup op a) => Group op a
instance (IdentityOp op a, AssociativeOp op a, Quasigroup op a) => Group op a

class    (Group op a, Commutative op a) => Abelian op a
instance (Group op a, Commutative op a) => Abelian op a

class    (AssociativeOp op a, Commutative op a, Idempotent op a) => Semilattice op a
instance (AssociativeOp op a, Commutative op a, Idempotent op a) => Semilattice op a

class    (Magma f a, Magma g a) => DistributiveOp f g a


class    (Abelian f a, Monoid g a, DistributiveOp f g a, DistributiveOp g f a) => Ring f g a
instance (Abelian f a, Monoid g a, DistributiveOp f g a, DistributiveOp g f a) => Ring f g a

class    (Ring f g a, Commutative g a) => CommutativeRing f g a
instance (Ring f g a, Commutative g a) => CommutativeRing f g a

class    (CommutativeRing f g a, Abelian g a, Quasigroup f a, Quasigroup f a) => Field f g a
instance (CommutativeRing f g a, Abelian g a, Quasigroup f a, Quasigroup f a) => Field f g a

-- instances

instance AssociativeOp 'Add Base.Int
instance Commutative 'Add Base.Int
instance AssociativeOp 'Mult Base.Int
instance DistributiveOp 'Mult 'Add Base.Int
instance DistributiveOp 'Add 'Mult Base.Int

instance AssociativeOp 'Add Base.Integer
instance Commutative 'Add Base.Integer
instance AssociativeOp 'Mult Base.Integer
instance DistributiveOp 'Mult 'Add Base.Integer
instance DistributiveOp 'Add 'Mult Base.Integer

instance AssociativeOp 'Add Base.String
