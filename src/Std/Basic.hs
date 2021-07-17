{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Basic
    ( Basic(..), Basic1(..), Basic2(..)
    , Unsafe(..), Unsafe1(..)
    , Monoidal(..), Newtype(..)
    , Numeric(..), PartialNumeric(..)
    , errorToPartial0, liftTotal0
    , errorToPartial1, liftTotal1
    , errorToPartial2, liftTotal2
    , errorToPartial3, liftTotal3
    ) where

import "base" Data.Eq
import "base" Data.Either ( Either(..) )
import "base" GHC.IO.Unsafe ( unsafePerformIO )
import "base" Control.Exception ( SomeException, try, evaluate )

import "this" Std.Partial
import "this" Std.Cat


newtype Basic          a = Basic          { getBasic          :: a } deriving newtype Eq
newtype Newtype        a = Newtype        { getNewtype        :: a } deriving newtype Eq
newtype Unsafe         a = Unsafe         { getUnsafe         :: a } deriving newtype Eq
newtype Unsafe1        a = Unsafe1        { getUnsafe1        :: a } deriving newtype Eq
newtype Monoidal       a = Monoidal       { getMonoidal       :: a } deriving newtype Eq
newtype Numeric        a = Numeric        { getNumeric        :: a } deriving newtype Eq
newtype PartialNumeric a = PartialNumeric { getPartialNumeric :: a } deriving newtype Eq

errorToPartial0 :: forall a. a -> Res 'Partial a
errorToPartial0 x = toRes' (unsafePerformIO (try (evaluate x)))
  where
    toRes' :: Either SomeException a -> Res 'Partial a
    toRes' (Left _)  = EmptyRes
    toRes' (Right r) = pure r

errorToPartial1 :: forall f a b. Coercible f (a -> b) => f -> a -> Res 'Partial b
errorToPartial1 f a = errorToPartial0 ((to coerce f :: a -> b) a)

errorToPartial2 :: forall f a b c. Coercible f (a -> b -> c) => f -> a -> b -> Res 'Partial c
errorToPartial2 f a b = errorToPartial0 ((to coerce f :: a -> b -> c) a b)

errorToPartial3 :: forall f a b c d. Coercible f (a -> b -> c -> d) => f -> a -> b -> c -> Res 'Partial d
errorToPartial3 f a b c = errorToPartial0 ((to coerce f :: a -> b -> c -> d) a b c)


liftTotal0 :: a -> Res 'Partial a
liftTotal0 = pure

liftTotal1 :: forall f a b. Coercible f (a -> b) => f -> a -> Res 'Total b
liftTotal1 f a = pure ((to coerce f :: a -> b) a)

liftTotal2 :: forall f a b c. Coercible f (a -> b -> c) => f -> a -> b -> Res 'Total c
liftTotal2 f a b = pure ((to coerce f :: a -> b -> c) a b)

liftTotal3 :: forall f a b c d. Coercible f (a -> b -> c -> d) => f -> a -> b -> c -> Res 'Total d
liftTotal3 f a b c = pure ((to coerce f :: a -> b -> c -> d) a b c)
