{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Group where

import "base" Data.Proxy
import "base" Data.Maybe
import "base" Prelude qualified as Base
import "base" Control.Applicative qualified as Base

import "this" Std.Partial
import "this" Std.Basic
import "this" Std.Cat hiding ( Identity )


type BinOp op a = a -> a -> Res (OpTotallity op a) a

class FromRes (OpTotallity op a) => Magma (op :: k) a where
    type OpTotallity op a :: Totallity
    op :: proxy op -> BinOp op a

class Magma op a => Identity (op :: k) a where
    identity :: proxy op -> a

class Magma op a => Associative (op :: k) a

type Inv op a = a -> Res (InvTotallity op a) a
type InvOp op a = a -> a -> Res (InvTotallity op a) a
class Magma op a => Invertible (op :: k) a where
    type InvTotallity op a :: Totallity
    inv :: proxy op -> Inv op a
    invOp :: proxy op -> InvOp op a

class Magma op a => Commutative (op :: k) a

class Magma op a => Idempotent (op :: k) a

class Magma op a => Upper (op :: k) a where
    top :: proxy op -> a

class Magma op a => Lower (op :: k) a where
    bot :: proxy op -> a

type Bounded op a = (Upper op a, Lower op a)

type Total op a = OpTotallity op a ~ 'Total
type Partial op a = OpTotallity op a ~ 'Partial

type Quasigroup = Invertible
type UnitalMagma = Identity
type Semigroup = Associative
type Loop op a = (Quasigroup op a, Identity op a)
type InverseSemigroup op a = (Semigroup op a, Invertible op a)
type Monoid op a = (Semigroup op a, Identity op a)
type Group op a = (Identity op a, Associative op a, Invertible op a)
type Abelian op a = (Group op a, Commutative op a)

type Semilattice op a = (Associative op a, Commutative op a, Idempotent op a)


data (:+) 
data (:*)

(+) :: forall a. (OpTotallity (:+) a ~ 'Total, Magma (:+) a) => a -> a -> a
(+) = coerce ((+?) :: BinOp (:+) a)
(+?) :: Magma (:+) a => BinOp (:+) a
(+?) = op (Proxy @(:+))
zero :: Identity (:+) a => a
zero = identity (Proxy @(:+))
(-) :: forall a. (InvTotallity (:+) a ~ 'Total, Invertible (:+) a) => a -> a -> a
(-) = coerce ((-?) :: InvOp (:+) a)
(-?) :: Invertible (:+) a => InvOp (:+) a
(-?) = invOp (Proxy @(:+))
negate' :: forall a. (InvTotallity (:+) a ~ 'Total, Invertible (:+) a) => a -> a
negate' = coerce (negate :: Inv (:+) a)
negate :: Invertible (:+) a => Inv (:+) a
negate = inv (Proxy @(:+))

empty :: forall f a. Identity (:+) (f a) => f a
empty = identity (Proxy @(:+))

(*) :: forall a. (OpTotallity (:*) a ~ 'Total, Magma (:*) a) => a -> a -> a
(*) = coerce ((*?) :: BinOp (:*) a)
(*?) :: Magma (:*) a => BinOp (:*) a
(*?) = op (Proxy @(:*))
one :: Identity (:*) a => a
one = identity (Proxy @(:*))
(/) :: forall a. (InvTotallity (:*) a ~ 'Total, Invertible (:*) a) => a -> a -> a
(/) = coerce ((/?) :: InvOp (:*) a)
(/?) :: Invertible (:*) a => InvOp (:*) a
(/?) = invOp (Proxy @(:*))
recip :: forall a. (InvTotallity (:*) a ~ 'Total, Invertible (:*) a) => a -> a
recip = coerce (recip' :: Inv (:*) a)
recip' :: Invertible (:*) a => Inv (:*) a
recip' = inv (Proxy @(:*))


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

instance Magma (:+) (Maybe a) where
    type OpTotallity (:+) (Maybe a) = 'Total
    op _ a@(Just _) _ = pure a
    op _ _ b = pure b
instance Idempotent (:+) (Maybe a)

newtype Numerically a = Numerically a

instance Base.Num a => Magma (:+) (Numerically a) where
    type OpTotallity (:+) (Numerically a) = 'Total
    op _ = coerce ((Base.+) @a)
instance Base.Num a => Associative (:+) (Numerically a)

instance Base.Num a => Invertible (:+) (Numerically a) where
    type InvTotallity (:+) (Numerically a) = 'Total
    inv _ = coerce (Base.negate @a)
    invOp _ = coerce ((Base.-) @a)
instance Base.Num a => Identity (:+) (Numerically a) where
    identity _ = coerce (0 :: a)
instance Base.Num a => Commutative (:+) (Numerically a)

instance Base.Num a => Magma (:*) (Numerically a) where
    type OpTotallity (:*) (Numerically a) = 'Total
    op _ = coerce ((Base.*) @a)
instance Base.Num a => Associative (:*) (Numerically a)
instance Base.Num a => Identity (:*) (Numerically a) where
    identity _ = coerce (1 :: a)
instance Base.Num a => Commutative (:*) (Numerically a)

instance Base.Num a => LeftDistributive (:*) (:+) (Numerically a)
instance Base.Num a => RightDistributive (:*) (:+) (Numerically a)


deriving via (Numerically Base.Int) instance Magma (:+) Base.Int
deriving via (Numerically Base.Int) instance Associative (:+) Base.Int
deriving via (Numerically Base.Int) instance Invertible (:+) Base.Int
deriving via (Numerically Base.Int) instance Identity (:+) Base.Int
deriving via (Numerically Base.Int) instance Commutative (:+) Base.Int
deriving via (Numerically Base.Int) instance Magma (:*) Base.Int
deriving via (Numerically Base.Int) instance Associative (:*) Base.Int
deriving via (Numerically Base.Int) instance Identity (:*) Base.Int
deriving via (Numerically Base.Int) instance LeftDistributive (:*) (:+) Base.Int
deriving via (Numerically Base.Int) instance RightDistributive (:*) (:+) Base.Int

deriving via (Numerically Base.Integer) instance Magma (:+) Base.Integer
deriving via (Numerically Base.Integer) instance Associative (:+) Base.Integer
deriving via (Numerically Base.Integer) instance Invertible (:+) Base.Integer
deriving via (Numerically Base.Integer) instance Identity (:+) Base.Integer
deriving via (Numerically Base.Integer) instance Commutative (:+) Base.Integer
deriving via (Numerically Base.Integer) instance Magma (:*) Base.Integer
deriving via (Numerically Base.Integer) instance Associative (:*) Base.Integer
deriving via (Numerically Base.Integer) instance Identity (:*) Base.Integer
deriving via (Numerically Base.Integer) instance LeftDistributive (:*) (:+) Base.Integer
deriving via (Numerically Base.Integer) instance RightDistributive (:*) (:+) Base.Integer

newtype Monoidal a = Monoidal a

instance Base.Semigroup a => Magma (:+) (Monoidal a) where
    type OpTotallity (:+) (Monoidal a) = 'Total
    op _ = coerce ((Base.<>) :: a -> a -> a)
instance Base.Semigroup a => Associative (:+) (Monoidal a)
instance Base.Monoid a => Identity (:+) (Monoidal a) where
    identity _ = coerce (Base.mempty :: a)


instance Base.Alternative f => Magma (:+) (Basic1 f a) where
    type OpTotallity (:+) (Basic1 f a) = 'Total
    op _ = coerce ((Base.<|>) :: f a -> f a -> f a)
instance Base.Alternative f => Associative (:+) (Basic1 f a)
instance Base.Alternative f => Identity (:+) (Basic1 f a) where
    identity _ = coerce (Base.empty :: f a)

deriving via (Basic1 [] a) instance Magma (:+) [a]
deriving via (Basic1 [] a) instance Associative (:+) [a]
deriving via (Basic1 [] a) instance Identity (:+) [a]
