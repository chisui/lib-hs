{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.BinOp where

import "base" Prelude qualified as Base
import "base" Data.Coerce ( Coercible )

import "ghc-prim" GHC.Prim ( Proxy#, proxy# )

import "this" Std.Literal
import "this" Std.Ord
import "this" Std.Cat
import "this" Std.Basic
import "this" Std.Partial

type OpRes op a = DirectRes (OpTotallity op a) a
class MapDirectRes (OpTotallity op a) => BinOp (op :: k) a where
    type OpTotallity op a :: Totallity
    type OpTotallity op a = 'Total
    op :: proxy op -> a -> a -> OpRes op a
    op _ = op# (proxy# @op)
    op# :: Proxy# op -> a -> a -> OpRes op a
    op# _ = op (Proxy @op)
    {-# MINIMAL op | op# #-}

class    (BinOp op a, OpTotallity op a ~ t) => BinOp' t op a | op a -> t
instance (BinOp op a, OpTotallity op a ~ t) => BinOp' t op a
class    BinOp' 'Partial op a => PartialBinOp op a
instance BinOp' 'Partial op a => PartialBinOp op a
class    BinOp' 'Total   op a => TotalBinOp   op a
instance BinOp' 'Total   op a => TotalBinOp   op a

class BinOp op a => IdentityOp (op :: k) a where
    identity :: proxy op -> a
    identity _ = identity# (proxy# @op)
    identity# :: Proxy# op -> a
    identity# _ = identity (Proxy @op)
    {-# MINIMAL identity | identity# #-}

class (BinOp op a, BinOp (InvOp op a) a) => InverseOp (op :: k) a where
    type InvOp op a :: k
    inv :: proxy op -> a -> OpRes (InvOp op a) a
    inv _ = inv# (proxy# @op)
    inv# :: Proxy# op -> a -> OpRes (InvOp op a) a
    inv# _ = inv (Proxy @op)
    {-# MINIMAL inv | inv# #-}

class    (InverseOp op a, OpTotallity (InvOp op a) a ~ t) => InverseOp' t op a | op a -> t
instance (InverseOp op a, OpTotallity (InvOp op a) a ~ t) => InverseOp' t op a
class    InverseOp' 'Partial op a => PartialInverseOp op a
instance InverseOp' 'Partial op a => PartialInverseOp op a
class    InverseOp' 'Total   op a => TotalInverseOp   op a
instance InverseOp' 'Total   op a => TotalInverseOp   op a

data BasicOps
    = Add
    | Sub
    | Mult
    | Div
    | Expo
class    BinOp 'Add a => Add a
instance BinOp 'Add a => Add a
(+) :: Add a => a -> a -> OpRes 'Add a
(+) = op# (proxy# @'Add)
infixr 5 +
class    BinOp 'Sub a => Sub a
instance BinOp 'Sub a => Sub a
(-) :: Sub a => a -> a -> OpRes 'Sub a
(-) = op# (proxy# @'Sub)
class    BinOp 'Mult a => Mult a
instance BinOp 'Mult a => Mult a
(*) :: Mult a => a -> a -> OpRes 'Mult a
(*) = op# (proxy# @'Mult)
class    BinOp 'Div a => Div a
instance BinOp 'Div a => Div a
(/) :: Div a => a -> a -> OpRes 'Div a
(/) = op# (proxy# @'Div)


zero :: IdentityOp 'Add a => a
zero = identity# (proxy# @'Add)
negate :: InverseOp 'Add a => a -> OpRes (InvOp 'Add a) a
negate = inv# (proxy# @'Add)

one :: IdentityOp 'Mult a => a
one = identity# (proxy# @'Mult)
recip :: InverseOp 'Mult a => a -> OpRes (InvOp 'Mult a) a
recip = inv# (proxy# @'Mult)


class    (Eq a, IdentityOp 'Add a, Pred 'Total a, Succ 'Total a) => Iterable a
instance (Eq a, IdentityOp 'Add a, Pred 'Total a, Succ 'Total a) => Iterable a

instance Base.Num a => BinOp 'Add (Numeric a) where
    type OpTotallity 'Add (Numeric a) = 'Total
    op# _ = to coerce ((Base.+) @a)

instance Base.Num a => BinOp 'Sub (Numeric a) where
    type OpTotallity 'Sub (Numeric a) = 'Total
    op# _ = to coerce ((Base.-) @a)

instance Base.Num a => InverseOp 'Add (Numeric a) where
    type InvOp 'Add (Numeric a) = 'Sub
    inv# _ = to coerce (Base.negate @a)

instance Base.Num a => InverseOp 'Sub (Numeric a) where
    type InvOp 'Sub (Numeric a) = 'Add
    inv# _ = to coerce (Base.negate @a)

instance Base.Num a => IdentityOp 'Add (Numeric a) where
    identity# _ = to coerce (0 :: Basic a)

instance Base.Num a => IdentityOp 'Sub (Numeric a) where
    identity# _ = to coerce (0 :: Basic a)

instance Base.Num a => BinOp 'Mult (Numeric a) where
    type OpTotallity 'Mult (Numeric a) = 'Total
    op# _ = to coerce ((Base.*) @a)

instance Base.Fractional a => BinOp 'Div (Numeric a) where
    type OpTotallity 'Div (Numeric a) = 'Total
    op# _ = to coerce ((Base./) @a)
instance Base.Fractional a => InverseOp 'Mult (Numeric a) where
    type InvOp 'Mult (Numeric a) = 'Div
    inv# _ = to coerce (Base.recip @a)
instance Base.Fractional a => InverseOp 'Div (Numeric a) where
    type InvOp 'Div (Numeric a) = 'Mult
    inv# _ = to coerce (Base.recip @a)

instance Base.Num a => IdentityOp 'Mult (Numeric a) where
    identity# _ = to coerce (1 :: Basic a)

instance Base.Fractional a => IdentityOp 'Div (Numeric a) where
    identity# _ = to coerce (1 :: Basic a)


instance Base.Num a => BinOp 'Add (PartialNumeric a) where
    type OpTotallity 'Add (PartialNumeric a) = 'Partial
    op# _ = errorToPartial2 ((Base.+) @a)

instance Base.Num a => BinOp 'Sub (PartialNumeric a) where
    type OpTotallity 'Sub (PartialNumeric a) = 'Partial
    op# _ = errorToPartial2 ((Base.-) @a)
instance Base.Num a => InverseOp 'Add (PartialNumeric a) where
    type InvOp 'Add (PartialNumeric a) = 'Sub
    inv# _ = errorToPartial1 (Base.negate @a)

instance Base.Num a => BinOp 'Mult (PartialNumeric a) where
    type OpTotallity 'Mult (PartialNumeric a) = 'Partial
    op# _ = errorToPartial2 ((Base.*) @a)

instance Base.Fractional a => BinOp 'Div (PartialNumeric a) where
    type OpTotallity 'Div (PartialNumeric a) = 'Partial
    op# _ = errorToPartial2 ((Base./) @a)
instance Base.Fractional a => InverseOp 'Mult (PartialNumeric a) where
    type InvOp 'Mult (PartialNumeric a) = 'Div
    inv# _ = errorToPartial1 (Base.recip @a)


instance Base.Semigroup a => BinOp op (Monoidal a) where
    type OpTotallity op (Monoidal a) = 'Total
    op# _ = to coerce ((Base.<>) :: a -> a -> a)

instance Base.Monoid a => IdentityOp op (Monoidal a) where
    identity# _ = to coerce (Base.mempty :: a)


opCoerced# :: forall a op b. (Coercible a b, BinOp op a) => Proxy# a -> Proxy# op -> b -> b -> DirectRes (OpTotallity op a) b
opCoerced# _ p a b = mapDirectRes (Proxy @(OpTotallity op a))
    (from coerce :: a -> b)
    (op# p (to coerce a :: a) (to coerce b :: a))

invCoerced# :: forall a op b. (Coercible a b, InverseOp op a) => Proxy# a -> Proxy# op -> b -> DirectRes (OpTotallity (InvOp op a) a) b
invCoerced# _ p a = mapDirectRes (Proxy @(OpTotallity (InvOp op a) a))
    (from coerce :: a -> b)
    (inv# p (to coerce a :: a))


instance PartialBinOp op a => BinOp op (Res 'Partial a) where
    type OpTotallity op (Res 'Partial a) = 'Total
    op# p (FullRes a) (FullRes b) = op# p a b
    op# _ EmptyRes _ = EmptyRes
    op# _ _ EmptyRes = EmptyRes

newtype Integrally a = Integrally a
deriving via (Numeric a) instance Base.Num a => BinOp 'Mult (Integrally a)
instance (FromInteger a, Base.Integral a) => BinOp 'Div (Integrally a) where
    type OpTotallity 'Div (Integrally a) = 'Partial
    op# _ = to coerce div :: Integrally a -> Integrally a -> PartialRes (Integrally a)
      where
        div :: a -> a -> PartialRes a
        div a b = case Base.quotRem a b of
            (r, 0) -> pure r
            _      -> empty
instance (FromInteger a, Base.Integral a) => InverseOp 'Mult (Integrally a) where
    type InvOp 'Mult (Integrally a) = 'Div
    inv# _ _ = EmptyRes

deriving via (Numeric    Base.Int) instance BinOp 'Add  Base.Int
deriving via (Numeric    Base.Int) instance BinOp 'Sub  Base.Int
deriving via (Numeric    Base.Int) instance BinOp 'Mult Base.Int
deriving via (Integrally Base.Int) instance BinOp 'Div  Base.Int
deriving via (Numeric    Base.Int) instance InverseOp 'Add  Base.Int
deriving via (Numeric    Base.Int) instance InverseOp 'Sub  Base.Int

deriving via (Numeric    Base.Integer) instance BinOp 'Add  Base.Integer
deriving via (Numeric    Base.Integer) instance BinOp 'Sub  Base.Integer
deriving via (Numeric    Base.Integer) instance BinOp 'Mult Base.Integer
deriving via (Integrally Base.Integer) instance BinOp 'Div  Base.Integer
deriving via (Numeric    Base.Integer) instance InverseOp 'Add  Base.Integer
deriving via (Numeric    Base.Integer) instance InverseOp 'Sub  Base.Integer

deriving via (Numeric    Base.Word) instance BinOp 'Add  Base.Word
deriving via (Numeric    Base.Word) instance BinOp 'Sub  Base.Word
deriving via (Numeric    Base.Word) instance BinOp 'Mult Base.Word
deriving via (Integrally Base.Word) instance BinOp 'Div  Base.Word

deriving via (Numeric        Base.Double) instance BinOp 'Add  Base.Double
deriving via (PartialNumeric Base.Double) instance BinOp 'Sub  Base.Double
deriving via (Numeric        Base.Double) instance BinOp 'Mult Base.Double
deriving via (PartialNumeric Base.Double) instance BinOp 'Div  Base.Double

deriving via (Monoidal Base.String) instance BinOp      'Add Base.String
deriving via (Monoidal Base.String) instance IdentityOp 'Add Base.String
