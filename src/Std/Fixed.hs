{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
module Std.Fixed where

import "base" Data.Data

import "this" Std.Ord
import "this" Std.Generic
import "this" Std.BinOp
import "this" Std.Cat
import "this" Std.Type

{-
-- | Generalisation of 'div' to any instance of 'Real'
--div' :: (Real a, Integral b) => a -> a -> b
div' n d = floor (toRational n / toRational d)

-- | Generalisation of 'divMod' to any instance of 'Real'
--divMod' :: (Real a, Integral b) => a -> a -> (b,a)
divMod' n d = (f,n - (fromIntegral f) * d) where
    f = div' n d

-- | Generalisation of 'mod' to any instance of 'Real'
--mod' :: (Real a) => a -> a -> a
mod' n d = n - (fromInteger f) * d where
    f = div' n d
-}
-- | The type parameter should be an instance of 'Known'.
newtype Fixed (n :: Nat) a = Fixed
    { unFixed :: a
    }
  deriving stock (Generic, Data)
  deriving newtype Eq

deriving newtype instance Ord' t a => Ord' t (Fixed n a)
deriving via Identity instance CatFunctor' Unconstrained HASK HASK (Fixed n)

instance BinOp 'Add a => BinOp 'Add (Fixed n a) where
    type OpTotallity 'Add (Fixed n a) = OpTotallity 'Add a
    op# = opCoerced# (proxy# @a)
instance BinOp 'Sub a => BinOp 'Sub (Fixed n a) where
    type OpTotallity 'Sub (Fixed n a) = OpTotallity 'Sub a
    op# = opCoerced# (proxy# @a)
instance (InvOp 'Add a ~ 'Sub, InverseOp 'Add a) => InverseOp 'Add (Fixed n a) where
    type InvOp 'Add (Fixed n a) = 'Sub
    inv# = invCoerced# (proxy# @a)

{-
instance Enum (Fixed a) where
    succ (Fixed a) = Fixed (succ a)
    pred (Fixed a) = Fixed (pred a)
    toEnum = Fixed . toEnum
    fromEnum (Fixed a) = fromEnum a
    enumFrom (Fixed a) = fmap Fixed (enumFrom a)
    enumFromThen (Fixed a) (Fixed b) = fmap Fixed (enumFromThen a b)
    enumFromTo (Fixed a) (Fixed b) = fmap Fixed (enumFromTo a b)
    enumFromThenTo (Fixed a) (Fixed b) (Fixed c) = fmap Fixed (enumFromThenTo a b c)
-}
{-
instance (Known a) => Num (Fixed a) where
    (Fixed a) + (Fixed b) = Fixed (a + b)
    (Fixed a) - (Fixed b) = Fixed (a - b)
    fa@(Fixed a) * (Fixed b) = Fixed (div (a * b) (resolution fa))
    negate (Fixed a) = Fixed (negate a)
    abs (Fixed a) = Fixed (abs a)
    signum (Fixed a) = fromInteger (signum a)
    fromInteger i = withResolution (\res -> Fixed (i * res))
-}
{-
instance (Known a) => Real (Fixed a) where
    toRational fa@(Fixed a) = (toRational a) / (toRational (resolution fa))
-}
{-
instance (Known a) => Fractional (Fixed a) where
    fa@(Fixed a) / (Fixed b) = Fixed (div (a * (resolution fa)) b)
    recip fa@(Fixed a) = Fixed (div (res * res) a) where
        res = resolution fa
    fromRational r = withResolution (\res -> Fixed (floor (r * (toRational res))))
-}
{-
instance (Known a) => RealFrac (Fixed a) where
    properFraction a = (i,a - (fromIntegral i)) where
        i = truncate a
    truncate f = truncate (toRational f)
    round f = round (toRational f)
    ceiling f = ceiling (toRational f)
    floor f = floor (toRational f)
-}
{-
chopZeros :: Integer -> String
chopZeros 0 = ""
chopZeros a | mod a 10 == 0 = chopZeros (div a 10)
chopZeros a = show a

-- only works for positive a
showIntegerZeros :: Bool -> Int -> Integer -> String
showIntegerZeros True _ 0 = ""
showIntegerZeros chopTrailingZeros digits a = replicate (digits - length s) '0' ++ s' where
    s = show a
    s' = if chopTrailingZeros then chopZeros a else s

withDot :: String -> String
withDot "" = ""
withDot s = '.':s

-- | First arg is whether to chop off trailing zeros
showFixed :: (Known n, Show a) => Bool -> Fixed n a -> String
showFixed chopTrailingZeros fa@(Fixed a) | a < 0 = "-" ++ (showFixed chopTrailingZeros (asTypeOf (Fixed (negate a)) fa))
showFixed chopTrailingZeros fa@(Fixed a) = (show i) ++ (withDot (showIntegerZeros chopTrailingZeros digits fracNum)) where
    res = resolution fa
    (i,d) = divMod a res
    -- enough digits to be unambiguous
    digits = ceiling (logBase 10 (fromInteger res) :: Double)
    maxnum = 10 ^ digits
    -- read floors, so show must ceil for `read . show = id` to hold. See #9240
    fracNum = divCeil (d * maxnum) res
    divCeil x y = (x + y - 1) `div` y

instance (Show a, Known n) => Show (Fixed n a) where
    showsPrec p n = showParen (p > 6 && n < 0) $ showString $ showFixed False n
-}
-- | resolution of 1, this works the same as Integer
type Uni = Fixed 0

-- | resolution of 10^-1 = .1
type Deci = Fixed 1

-- | resolution of 10^-2 = .01, useful for many monetary currencies
type Centi = Fixed 2

-- | resolution of 10^-3 = .001
type Milli = Fixed 3

-- | resolution of 10^-6 = .000001
type Micro = Fixed 6

-- | resolution of 10^-9 = .000000001
type Nano = Fixed 9

-- | resolution of 10^-12 = .000000000001
type Pico = Fixed 12
