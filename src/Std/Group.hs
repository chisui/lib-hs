{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Group
    ( module Exp
    , Magma
    , AssociativeOp
    , Quasigroup
    , CommutativeOp
    , Idempotent
    , Upper(..)
    , Lower(..)
    , UnitalMagma
    , Semigroup
    , CanonicSemigroup
    , Loop
    , InverseSemigroup
    , CanonicInverseSemigroup
    , Monoid
    , CanonicMonoid
    , Group
    , Abelian
    , Semilattice
    , DistributiveOp
    , Ring
    , CommutativeRing
    , Field
    ) where

import "base" Data.Proxy
import "base" Prelude qualified as Base
import "base" Data.Semigroup qualified as Base ( Min(..), Max(..) )
import "base" Data.Monoid qualified as Base ( First(..), Last(..) )

import "ghc-prim" GHC.Prim ( Proxy#, proxy# )

import "this" Std.BinOp as Exp
import "this" Std.Ord


class    TotalBinOp op a => Magma op a
instance TotalBinOp op a => Magma op a

class Magma op a => AssociativeOp (op :: k) a

class    TotalInverseOp op a => Quasigroup op a
instance TotalInverseOp op a => Quasigroup op a


class Magma op a => CommutativeOp (op :: k) a

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

class    AssociativeOp 'Canonic a => CanonicSemigroup a
instance AssociativeOp 'Canonic a => CanonicSemigroup a

class    (Quasigroup op a, IdentityOp op a) => Loop op a
instance (Quasigroup op a, IdentityOp op a) => Loop op a

class    (Semigroup op a, Quasigroup op a) => InverseSemigroup op a
instance (Semigroup op a, Quasigroup op a) => InverseSemigroup op a

class    (Semigroup 'Canonic a, Quasigroup 'Canonic a) => CanonicInverseSemigroup a
instance (Semigroup 'Canonic a, Quasigroup 'Canonic a) => CanonicInverseSemigroup a

class    (Semigroup op a, IdentityOp op a) => Monoid op a
instance (Semigroup op a, IdentityOp op a) => Monoid op a

class    (Semigroup 'Canonic a, IdentityOp 'Canonic a) => CanonicMonoid a
instance (Semigroup 'Canonic a, IdentityOp 'Canonic a) => CanonicMonoid a

class    (IdentityOp op a, AssociativeOp op a, Quasigroup op a) => Group op a
instance (IdentityOp op a, AssociativeOp op a, Quasigroup op a) => Group op a

class    (Group op a, CommutativeOp op a) => Abelian op a
instance (Group op a, CommutativeOp op a) => Abelian op a

class    (AssociativeOp op a, CommutativeOp op a, Idempotent op a) => Semilattice op a
instance (AssociativeOp op a, CommutativeOp op a, Idempotent op a) => Semilattice op a

class    (Magma f a, Magma g a) => DistributiveOp f g a


class    (Abelian f a, Monoid g a, DistributiveOp f g a, DistributiveOp g f a) => Ring f g a
instance (Abelian f a, Monoid g a, DistributiveOp f g a, DistributiveOp g f a) => Ring f g a

class    (Ring f g a, CommutativeOp g a) => CommutativeRing f g a
instance (Ring f g a, CommutativeOp g a) => CommutativeRing f g a

class    (CommutativeRing f g a, Abelian g a, Quasigroup f a, Quasigroup f a) => Field f g a
instance (CommutativeRing f g a, Abelian g a, Quasigroup f a, Quasigroup f a) => Field f g a

-- instances

instance AssociativeOp  'Add Base.Int
instance CommutativeOp    'Add Base.Int
instance AssociativeOp  'Mult Base.Int
instance DistributiveOp 'Mult 'Add Base.Int
instance DistributiveOp 'Add 'Mult Base.Int

instance AssociativeOp  'Add Base.Integer
instance CommutativeOp    'Add Base.Integer
instance AssociativeOp  'Mult Base.Integer
instance DistributiveOp 'Mult 'Add Base.Integer
instance DistributiveOp 'Add 'Mult Base.Integer

instance AssociativeOp 'Canonic Base.String
instance Ord a => AssociativeOp 'Canonic (Base.Min a)
instance Ord a => AssociativeOp 'Canonic (Base.Max a)
instance Ord a => Idempotent    'Canonic (Base.Min a)
instance Ord a => Idempotent    'Canonic (Base.Max a)
instance AssociativeOp 'Canonic (Base.First a)
instance AssociativeOp 'Canonic (Base.Last a)

