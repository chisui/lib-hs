{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.BinOp where

import "base" Prelude qualified as Base
import "base" Data.Semigroup qualified as Base ( Min(..), Max(..) )
import "base" Data.Monoid qualified as Base( First(..), Last(..) )

import "ghc-prim" GHC.Prim ( Proxy#, proxy# )

import "this" Std.Literal
import "this" Std.IfThenElse
import "this" Std.Ord
import "this" Std.Cat
import "this" Std.Basic
import "this" Std.Partial

type OpRes op a = DirectRes (OpTotality op a) a
class MapDirectRes (OpTotality op a) => BinOp (op :: k) a where
    type OpTotality op a :: Totality
    type OpTotality op a = 'Total
    op :: proxy op -> a -> a -> OpRes op a
    op _ = op# (proxy# @op)
    op# :: Proxy# op -> a -> a -> OpRes op a
    op# _ = op (Proxy @op)
    {-# MINIMAL op | op# #-}

class    (BinOp op a, OpTotality op a ~ t) => BinOp' t op a | op a -> t
instance (BinOp op a, OpTotality op a ~ t) => BinOp' t op a
class    BinOp' 'Partial op a => PartialBinOp op a
instance BinOp' 'Partial op a => PartialBinOp op a
class    (BinOp' 'Total   op a) => TotalBinOp   op a
instance (BinOp' 'Total   op a) => TotalBinOp   op a

totalOp# :: TotalBinOp op a => Proxy# op -> a -> a -> a
totalOp# = op#

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

class    (InverseOp op a, OpTotality (InvOp op a) a ~ t) => InverseOp' t op a | op a -> t
instance (InverseOp op a, OpTotality (InvOp op a) a ~ t) => InverseOp' t op a
class    InverseOp' 'Partial op a => PartialInverseOp op a
instance InverseOp' 'Partial op a => PartialInverseOp op a
class    InverseOp' 'Total   op a => TotalInverseOp   op a
instance InverseOp' 'Total   op a => TotalInverseOp   op a

data Canonic = Canonic | InvCanonic
data BasicOp
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
class    BinOp 'Canonic a => CanonicOp a
instance BinOp 'Canonic a => CanonicOp a
(++) :: CanonicOp a => a -> a -> OpRes 'Canonic a
(++) = op# (proxy# @'Canonic)
class    BinOp 'InvCanonic a => InvCanonicOp a
instance BinOp 'InvCanonic a => InvCanonicOp a
(/+) :: InvCanonicOp a => a -> a -> OpRes 'InvCanonic a
(/+) = op# (proxy# @'InvCanonic)


zero :: IdentityOp 'Add a => a
zero = identity# (proxy# @'Add)
negate :: InverseOp 'Add a => a -> OpRes (InvOp 'Add a) a
negate = inv# (proxy# @'Add)

one :: IdentityOp 'Mult a => a
one = identity# (proxy# @'Mult)
recip :: InverseOp 'Mult a => a -> OpRes (InvOp 'Mult a) a
recip = inv# (proxy# @'Mult)


mempty :: IdentityOp 'Canonic a => a
mempty = identity# (proxy# @'Canonic)

class    (Eq a, Ord a, IdentityOp 'Add a, TotalBinOp 'Add a, Pred 'Total a, Succ 'Total a, FromInt 'Total a) => Iterable a
instance (Eq a, Ord a, IdentityOp 'Add a, TotalBinOp 'Add a, Pred 'Total a, Succ 'Total a, FromInt 'Total a) => Iterable a

replicate :: (Alternative f, Iterable i) => i -> a -> f a
replicate i a = if (i == zero)
    then empty
    else cons a (replicate (pred i) a)

instance Base.Num a => BinOp 'Add (Numeric a) where
    type OpTotality 'Add (Numeric a) = 'Total
    op# _ = to coerce ((Base.+) @a)

instance Base.Num a => BinOp 'Sub (Numeric a) where
    type OpTotality 'Sub (Numeric a) = 'Total
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
    type OpTotality 'Mult (Numeric a) = 'Total
    op# _ = to coerce ((Base.*) @a)

instance Base.Fractional a => BinOp 'Div (Numeric a) where
    type OpTotality 'Div (Numeric a) = 'Total
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
    type OpTotality 'Add (PartialNumeric a) = 'Partial
    op# _ = errorToPartial2 ((Base.+) @a)

instance Base.Num a => BinOp 'Sub (PartialNumeric a) where
    type OpTotality 'Sub (PartialNumeric a) = 'Partial
    op# _ = errorToPartial2 ((Base.-) @a)
instance Base.Num a => InverseOp 'Add (PartialNumeric a) where
    type InvOp 'Add (PartialNumeric a) = 'Sub
    inv# _ = errorToPartial1 (Base.negate @a)

instance Base.Num a => BinOp 'Mult (PartialNumeric a) where
    type OpTotality 'Mult (PartialNumeric a) = 'Partial
    op# _ = errorToPartial2 ((Base.*) @a)

instance Base.Fractional a => BinOp 'Div (PartialNumeric a) where
    type OpTotality 'Div (PartialNumeric a) = 'Partial
    op# _ = errorToPartial2 ((Base./) @a)
instance Base.Fractional a => InverseOp 'Mult (PartialNumeric a) where
    type InvOp 'Mult (PartialNumeric a) = 'Div
    inv# _ = errorToPartial1 (Base.recip @a)


instance Base.Semigroup a => BinOp 'Canonic (Monoidal a) where
    type OpTotality 'Canonic (Monoidal a) = 'Total
    op# _ = to coerce ((Base.<>) :: a -> a -> a)

instance Base.Monoid a => IdentityOp 'Canonic (Monoidal a) where
    identity# _ = to coerce (Base.mempty :: a)

instance TotalBinOp 'Canonic a => Base.Semigroup (Monoidal a) where
    (<>) = to coerce ((++) :: a -> a -> a)

instance (TotalBinOp 'Canonic a, IdentityOp 'Canonic a) => Base.Monoid (Monoidal a) where
    mempty = to coerce (mempty :: a)


opCoerced# :: forall a op b. (Coercible a b, BinOp op a) => Proxy# a -> Proxy# op -> b -> b -> DirectRes (OpTotality op a) b
opCoerced# _ p a b = mapDirectRes (Proxy @(OpTotality op a))
    (from coerce :: a -> b)
    (op# p (to coerce a :: a) (to coerce b :: a))

invCoerced# :: forall a op b. (Coercible a b, InverseOp op a) => Proxy# a -> Proxy# op -> b -> DirectRes (OpTotality (InvOp op a) a) b
invCoerced# _ p a = mapDirectRes (Proxy @(OpTotality (InvOp op a) a))
    (from coerce :: a -> b)
    (inv# p (to coerce a :: a))


instance PartialBinOp op a => BinOp (op :: BasicOp) (Res 'Partial a) where
    type OpTotality op (Res 'Partial a) = 'Total
    op# p (FullRes a) (FullRes b) = op# p a b
    op# _ EmptyRes _ = EmptyRes
    op# _ _ EmptyRes = EmptyRes

newtype Integrally a = Integrally a
deriving via (Numeric a) instance Base.Num a => BinOp 'Mult (Integrally a)
instance (FromInteger a, Base.Integral a) => BinOp 'Div (Integrally a) where
    type OpTotality 'Div (Integrally a) = 'Partial
    op# _ = to coerce div :: Integrally a -> Integrally a -> PartialRes (Integrally a)
      where
        div :: a -> a -> PartialRes a
        div a b = case Base.quotRem a b of
            (r, 0) -> pure r
            _      -> empty
instance (FromInteger a, Base.Integral a) => InverseOp 'Mult (Integrally a) where
    type InvOp 'Mult (Integrally a) = 'Div
    inv# _ _ = EmptyRes

deriving via (Numeric    Base.Int) instance BinOp      'Add  Base.Int
deriving via (Numeric    Base.Int) instance IdentityOp 'Add  Base.Int
deriving via (Numeric    Base.Int) instance InverseOp  'Add  Base.Int
deriving via (Numeric    Base.Int) instance BinOp      'Sub  Base.Int
deriving via (Numeric    Base.Int) instance IdentityOp 'Sub  Base.Int
deriving via (Numeric    Base.Int) instance InverseOp  'Sub  Base.Int
deriving via (Numeric    Base.Int) instance BinOp      'Mult Base.Int
deriving via (Numeric    Base.Int) instance IdentityOp 'Mult  Base.Int
deriving via (Integrally Base.Int) instance BinOp      'Div  Base.Int

instance BinOp 'Add  Base.Integer where op# _ = (Base.+)
instance BinOp 'Sub  Base.Integer where op# _ = (Base.-)
instance BinOp 'Mult Base.Integer where op# _ = (Base.*)
instance BinOp 'Div  Base.Integer where op# _ = (Base.div)
instance InverseOp 'Add  Base.Integer where
    type InvOp 'Add Base.Integer = 'Sub
    inv# _ = Base.negate
instance InverseOp 'Sub  Base.Integer where
    type InvOp 'Sub Base.Integer = 'Add
    inv# _ = Base.negate

deriving via (Numeric    Base.Word) instance BinOp 'Add  Base.Word
deriving via (Numeric    Base.Word) instance BinOp 'Sub  Base.Word
deriving via (Numeric    Base.Word) instance BinOp 'Mult Base.Word
deriving via (Integrally Base.Word) instance BinOp 'Div  Base.Word

deriving via (Numeric        Base.Double) instance BinOp 'Add  Base.Double
deriving via (PartialNumeric Base.Double) instance BinOp 'Sub  Base.Double
deriving via (Numeric        Base.Double) instance BinOp 'Mult Base.Double
deriving via (PartialNumeric Base.Double) instance BinOp 'Div  Base.Double

deriving via (Monoidal Base.String) instance BinOp      'Canonic Base.String
deriving via (Monoidal Base.String) instance IdentityOp 'Canonic Base.String

instance Ord a => BinOp 'Canonic (Base.Min a) where
    op# _ a b = if a <= b then a else b 
instance Ord a => BinOp 'Canonic (Base.Max a) where
    op# _ a b = if a <= b then a else b 


deriving via (Monoidal (Base.First a)) instance BinOp      'Canonic (Base.First a)
deriving via (Monoidal (Base.First a)) instance IdentityOp 'Canonic (Base.First a)

deriving via (Monoidal (Base.Last a)) instance BinOp      'Canonic (Base.Last a)
deriving via (Monoidal (Base.Last a)) instance IdentityOp 'Canonic (Base.Last a)
