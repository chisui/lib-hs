{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Std.Group
    ( BinOp(..), Magma
    , BasicOps(..), (+), (-), (*), (/), (%), zero, negate, one, recip
    , IdentityOp(..), Invertible(..)
    , AssociativeOp, Idempotent, Commutative
    , Upper(..), Lower(..), Bounded
    , Quasigroup, UnitalMagma, Semigroup, Loop, InverseSemigroup, Monoid, Group, Abelian
    , IntegralOp(..), Integral, divMod, div
    , Semilattice, DistributiveOp, Ring, CommutativeRing, Field
    ) where

import "base" Data.Proxy
import "base" Prelude qualified as Base
import "base" Control.Applicative qualified as Base

import "ghc-prim" GHC.Prim ( Proxy#, proxy# )

import "this" Std.Partial
import "this" Std.Basic
import "this" Std.Literal
import "this" Std.Cat


class BinOp (t :: Totallity) (op :: k) a | op a -> t where
    op :: proxy op -> a -> a -> DirectRes t a
    op _ = op# (proxy# @op)
    op# :: Proxy# op -> a -> a -> DirectRes t a
    op# _ = op (Proxy @op)
    {-# MINIMAL op | op# #-}

class BinOp 'Total op a => Magma op a
instance BinOp 'Total op a => Magma op a

data BasicOps
    = Add
    | Sub
    | Mult
    | Div
class    BinOp t 'Add a => Add t a
instance BinOp t 'Add a => Add t a
(+) :: forall a t. Add t a => a -> a -> DirectRes t a
(+) = op# (proxy# @'Add)
class    BinOp t 'Sub a => Sub t a
instance BinOp t 'Sub a => Sub t a
(-) :: forall a t. Sub t a => a -> a -> DirectRes t a
(-) = op# (proxy# @'Sub)
class    BinOp t 'Mult a => Mult t a
instance BinOp t 'Mult a => Mult t a
(*) :: forall a t. Mult t a => a -> a -> DirectRes t a
(*) = op# (proxy# @'Mult)
class    BinOp t 'Div a => Div t a
instance BinOp t 'Div a => Div t a
(/) :: forall a t. Div t a => a -> a -> DirectRes t a
(/) = op# (proxy# @'Div)

class Magma op a => IdentityOp (op :: k) a where
    identity :: proxy op -> a
    identity _ = identity# (proxy# @op)
    identity# :: Proxy# op -> a

class Magma op a => AssociativeOp (op :: k) a

class BinOp t op a => Invertible t (op :: k) a where
    inv :: proxy op -> a -> DirectRes t a
    inv _ = inv# (proxy# @op)
    inv# :: Proxy# op -> a -> DirectRes t a

class    Invertible 'Total op a => Quasigroup op a
instance Invertible 'Total op a => Quasigroup op a

zero :: IdentityOp 'Add a => a
zero = identity# (proxy# @'Add)
negate :: Invertible t 'Add a => a -> DirectRes t a
negate = inv# (proxy# @'Add)

one :: IdentityOp 'Mult a => a
one = identity# (proxy# @'Mult)
recip :: Invertible t 'Mult a => a -> DirectRes t a
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

class    IdentityOp op a => UnitalMagma op a
instance IdentityOp op a => UnitalMagma op a

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

class IntegralOp op a where
    divMod# :: Proxy# op -> a -> a -> (a, a)
    divMod# p a b = (div# p a b, mod# p a b)
    div# :: Proxy# op -> a -> a -> a
    div# p a = fst . divMod# p a
    mod# :: Proxy# op -> a -> a -> a
    mod# p a = snd . divMod# p a
    {-# MINIMAL divMod# | (div#, mod#) #-}

class IntegralOp 'Div a => Integral a
instance IntegralOp 'Div a => Integral a

divMod :: Integral a => a -> a -> (a, a)
divMod = divMod# (proxy# @'Div)

div :: Integral a => a -> a -> a
div = div# (proxy# @'Div)

(%) :: Integral a => a -> a -> a
(%) = mod# (proxy# @'Div)

-- instances

instance Base.Num a => BinOp 'Total 'Add (Numeric a) where
    op# _ = to coerce ((Base.+) @a)
instance Base.Num a => AssociativeOp 'Add (Numeric a)
instance Base.Num a => BinOp 'Total 'Sub (Numeric a) where
    op# _ = to coerce ((Base.-) @a)
instance Base.Num a => BinOp 'Total 'Mult (Numeric a) where
    op# _ = to coerce ((Base.*) @a)

instance Base.Integral a => IntegralOp 'Div (Numeric a) where
    div# _ = to coerce (Base.mod @a)
    mod# _ = to coerce (Base.mod @a)
instance Base.Num a => Invertible 'Total 'Add (Numeric a) where
    inv# _ = to coerce (Base.negate @a)
instance Base.Num a => IdentityOp 'Add (Numeric a) where
    identity# _ = to coerce (0 :: Basic a)
instance Base.Num a => Commutative 'Add (Numeric a)

instance Base.Num a => AssociativeOp 'Mult (Numeric a)
instance Base.Num a => IdentityOp 'Mult (Numeric a) where
    identity# _ = to coerce (1 :: Basic a)
instance Base.Num a => Commutative 'Mult (Numeric a)

instance Base.Num a => DistributiveOp 'Mult 'Add (Numeric a)
instance Base.Num a => DistributiveOp 'Add 'Mult (Numeric a)


instance Base.Num a => BinOp 'Partial 'Add (PartialNumeric a) where
    op# _ = errorToPartial2 ((Base.+) @a)
instance Base.Num a => BinOp 'Partial 'Sub (PartialNumeric a) where
    op# _ = errorToPartial2 ((Base.-) @a)
instance Base.Num a => BinOp 'Partial 'Mult (PartialNumeric a) where
    op# _ = errorToPartial2 ((Base.*) @a)
instance Base.Fractional a => BinOp 'Partial 'Div (PartialNumeric a) where
    op# _ = errorToPartial2 ((Base./) @a)
instance Base.Num a => Invertible 'Partial 'Add (PartialNumeric a) where
    inv# _ = errorToPartial1 (Base.negate @a)
instance Base.Fractional a => Invertible 'Partial 'Mult (PartialNumeric a) where
    inv# _ = errorToPartial1 (Base.recip @a)


deriving via (Numeric Base.Int) instance BinOp 'Total 'Add Base.Int
deriving via (Numeric Base.Int) instance BinOp 'Total 'Sub Base.Int
deriving via (Numeric Base.Int) instance AssociativeOp 'Add Base.Int
deriving via (Numeric Base.Int) instance Invertible 'Total 'Add Base.Int
deriving via (Numeric Base.Int) instance IdentityOp 'Add Base.Int
deriving via (Numeric Base.Int) instance Commutative 'Add Base.Int
deriving via (Numeric Base.Int) instance BinOp 'Total 'Mult Base.Int
deriving via (Numeric Base.Int) instance AssociativeOp 'Mult Base.Int
deriving via (Numeric Base.Int) instance IdentityOp 'Mult Base.Int
deriving via (Numeric Base.Int) instance IntegralOp 'Div Base.Int
deriving via (Numeric Base.Int) instance DistributiveOp 'Mult 'Add Base.Int
deriving via (Numeric Base.Int) instance DistributiveOp 'Add 'Mult Base.Int

deriving via (Numeric Base.Integer) instance BinOp 'Total 'Sub Base.Integer
deriving via (Numeric Base.Integer) instance BinOp 'Total 'Add Base.Integer
deriving via (Numeric Base.Integer) instance AssociativeOp 'Add Base.Integer
deriving via (Numeric Base.Integer) instance Invertible 'Total 'Add Base.Integer
deriving via (Numeric Base.Integer) instance IdentityOp 'Add Base.Integer
deriving via (Numeric Base.Integer) instance Commutative 'Add Base.Integer
deriving via (Numeric Base.Integer) instance BinOp 'Total 'Mult Base.Integer
deriving via (Numeric Base.Integer) instance AssociativeOp 'Mult Base.Integer
deriving via (Numeric Base.Integer) instance IdentityOp 'Mult Base.Integer
deriving via (Numeric Base.Integer) instance IntegralOp 'Div Base.Integer
deriving via (Numeric Base.Integer) instance DistributiveOp 'Mult 'Add Base.Integer
deriving via (Numeric Base.Integer) instance DistributiveOp 'Add 'Mult Base.Integer

instance Base.Semigroup a => BinOp 'Total op (Monoidal a) where
    op# _ = to coerce ((Base.<>) :: a -> a -> a)
instance Base.Semigroup a => AssociativeOp op (Monoidal a)
instance Base.Monoid a => IdentityOp op (Monoidal a) where
    identity# _ = to coerce (Base.mempty :: a)

instance Base.Alternative f => BinOp 'Total 'Add (Basic1 f a) where
    op# _ = to coerce ((Base.<|>) :: f a -> f a -> f a)
instance Base.Alternative f => AssociativeOp 'Add (Basic1 f a)
instance Base.Alternative f => IdentityOp 'Add (Basic1 f a) where
    identity# _ = to coerce (Base.empty :: f a)

deriving via (Basic1 [] a) instance BinOp 'Total 'Add [a]
deriving via (Basic1 [] a) instance AssociativeOp 'Add [a]
deriving via (Basic1 [] a) instance IdentityOp 'Add [a]

instance BinOp 'Total   op a => BinOp 'Total op (Res 'Total   a) where
    op# p (FullRes a) (FullRes b) = FullRes (op# p a b)
instance BinOp 'Partial op a => BinOp 'Total op (Res 'Partial a) where
    op# p (FullRes a) (FullRes b) = op# p a b
    op# _ EmptyRes _ = EmptyRes
    op# _ _ EmptyRes = EmptyRes
