{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.BinOp where

import "base" Prelude qualified as Base

import "ghc-prim" GHC.Prim ( Proxy#, proxy# )

import "this" Std.Literal
import "this" Std.Ord
import "this" Std.Cat
import "this" Std.Basic
import "this" Std.Partial

type OpRes op a = DirectRes (OpTotallity op a) a
class BinOp (op :: k) a where
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

type InvOpRes op a = DirectRes (InvOpTotallity op a) a
class BinOp op a => InvertibleOp (op :: k) a where
    type InvOpTotallity op a :: Totallity
    type InvOpTotallity op a = 'Total
    inv :: proxy op -> a -> InvOpRes op a
    inv _ = inv# (proxy# @op)
    inv# :: Proxy# op -> a -> InvOpRes op a
    inv# _ = inv (Proxy @op)
    invOp :: proxy op -> a -> a -> InvOpRes op a
    invOp _ = invOp# (proxy# @op)
    invOp# :: Proxy# op -> a -> a -> InvOpRes op a
    invOp# _ = invOp (Proxy @op)
    {-# MINIMAL (inv | inv#), (invOp | invOp#) #-}

class    (InvertibleOp op a, InvOpTotallity op a ~ t) => InvertibleOp' t op a | op a -> t
instance (InvertibleOp op a, InvOpTotallity op a ~ t) => InvertibleOp' t op a
class    InvertibleOp' 'Partial op a => PartialInvertibleOp op a
instance InvertibleOp' 'Partial op a => PartialInvertibleOp op a
class    InvertibleOp' 'Total   op a => TotalInvertibleOp   op a
instance InvertibleOp' 'Total   op a => TotalInvertibleOp   op a

class BinOp op a => IdentityOp (op :: k) a where
    identity :: proxy op -> a
    identity _ = identity# (proxy# @op)
    identity# :: Proxy# op -> a
    identity# _ = identity (Proxy @op)
    {-# MINIMAL identity | identity# #-}



data BasicOps = Add | Mult
class    BinOp 'Add a => Add a
instance BinOp 'Add a => Add a
(+) :: Add a => a -> a -> OpRes 'Add a
(+) = op# (proxy# @'Add)
infixr 5 +
class    InvertibleOp 'Add a => Sub a
instance InvertibleOp 'Add a => Sub a
(-) :: Sub a => a -> a -> InvOpRes 'Add a
(-) = invOp# (proxy# @'Add)
class    BinOp 'Mult a => Mult a
instance BinOp 'Mult a => Mult a
(*) :: Mult a => a -> a -> OpRes 'Mult a
(*) = op# (proxy# @'Mult)
class    InvertibleOp 'Mult a => Div a
instance InvertibleOp 'Mult a => Div a
(/) :: Div a => a -> a -> InvOpRes 'Mult a
(/) = invOp# (proxy# @'Mult)


zero :: IdentityOp 'Add a => a
zero = identity# (proxy# @'Add)
negate :: InvertibleOp 'Add a => a -> InvOpRes 'Add a
negate = inv# (proxy# @'Add)

one :: IdentityOp 'Mult a => a
one = identity# (proxy# @'Mult)
recip :: InvertibleOp 'Mult a => a -> InvOpRes 'Mult a
recip = inv# (proxy# @'Mult)


instance Base.Num a => BinOp 'Add (Numeric a) where
    type OpTotallity 'Add (Numeric a) = 'Total
    op# _ = to coerce ((Base.+) @a)

instance Base.Num a => InvertibleOp 'Add (Numeric a) where
    type InvOpTotallity 'Add (Numeric a) = 'Total
    inv# _ = to coerce (Base.negate @a)
    invOp# _ = to coerce ((Base.-) @a)

instance Base.Num a => IdentityOp 'Add (Numeric a) where
    identity# _ = to coerce (0 :: Basic a)

instance Base.Num a => BinOp 'Mult (Numeric a) where
    type OpTotallity 'Mult (Numeric a) = 'Total
    op# _ = to coerce ((Base.*) @a)

instance Base.Fractional a => InvertibleOp 'Mult (Numeric a) where
    type InvOpTotallity 'Mult (Numeric a) = 'Total
    inv# _ = to coerce (Base.recip @a)
    invOp# _ = to coerce ((Base./) @a)

instance Base.Num a => IdentityOp 'Mult (Numeric a) where
    identity# _ = to coerce (1 :: Basic a)


instance Base.Num a => BinOp 'Add (PartialNumeric a) where
    type OpTotallity 'Add (PartialNumeric a) = 'Partial
    op# _ = errorToPartial2 ((Base.+) @a)

instance Base.Num a => InvertibleOp 'Add (PartialNumeric a) where
    type InvOpTotallity 'Add (PartialNumeric a) = 'Partial
    inv# _ = errorToPartial1 (Base.negate @a)
    invOp# _ = errorToPartial2 ((Base.-) @a)

instance Base.Num a => BinOp 'Mult (PartialNumeric a) where
    type OpTotallity 'Mult (PartialNumeric a) = 'Partial
    op# _ = errorToPartial2 ((Base.*) @a)

instance Base.Fractional a => InvertibleOp 'Mult (PartialNumeric a) where
    type InvOpTotallity 'Mult (PartialNumeric a) = 'Partial
    inv# _ = errorToPartial1 (Base.recip @a)
    invOp# _ = errorToPartial2 ((Base./) @a)


instance Base.Semigroup a => BinOp op (Monoidal a) where
    type OpTotallity op (Monoidal a) = 'Total
    op# _ = to coerce ((Base.<>) :: a -> a -> a)

instance Base.Monoid a => IdentityOp op (Monoidal a) where
    identity# _ = to coerce (Base.mempty :: a)


instance PartialBinOp op a => BinOp op (Res 'Partial a) where
    type OpTotallity op (Res 'Partial a) = 'Total
    op# p (FullRes a) (FullRes b) = op# p a b
    op# _ EmptyRes _ = EmptyRes
    op# _ _ EmptyRes = EmptyRes

newtype Integrally a = Integrally a
deriving via (Numeric a) instance Base.Num a => BinOp 'Mult (Integrally a)
instance (FromInteger a, Base.Integral a) => InvertibleOp 'Mult (Integrally a) where
    type InvOpTotallity 'Mult (Integrally a) = 'Partial
    inv# _ _ = EmptyRes
    invOp# _ = to coerce div :: Integrally a -> Integrally a -> PartialRes (Integrally a)
      where
        div :: a -> a -> PartialRes a
        div a b = case Base.quotRem a b of
            (r, 0) -> pure r
            _      -> empty


deriving via (Numeric    Base.Int) instance BinOp        'Add  Base.Int
deriving via (Numeric    Base.Int) instance InvertibleOp 'Add  Base.Int
deriving via (Numeric    Base.Int) instance BinOp        'Mult Base.Int
deriving via (Integrally Base.Int) instance InvertibleOp 'Mult Base.Int

deriving via (Numeric    Base.Integer) instance BinOp        'Add  Base.Integer
deriving via (Numeric    Base.Integer) instance InvertibleOp 'Add  Base.Integer
deriving via (Numeric    Base.Integer) instance BinOp        'Mult Base.Integer
deriving via (Integrally Base.Integer) instance InvertibleOp 'Mult Base.Integer

deriving via (Numeric    Base.Word) instance BinOp        'Add  Base.Word
deriving via (Numeric    Base.Word) instance InvertibleOp 'Add  Base.Word
deriving via (Numeric    Base.Word) instance BinOp        'Mult Base.Word
deriving via (Integrally Base.Word) instance InvertibleOp 'Mult Base.Word

deriving via (Numeric        Base.Double) instance BinOp        'Add  Base.Double
deriving via (PartialNumeric Base.Double) instance InvertibleOp 'Add  Base.Double
deriving via (Numeric        Base.Double) instance BinOp        'Mult Base.Double
deriving via (PartialNumeric Base.Double) instance InvertibleOp 'Mult Base.Double

deriving via (Monoidal Base.String) instance BinOp      'Add Base.String
deriving via (Monoidal Base.String) instance IdentityOp 'Add Base.String
