{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
module Std.IfThenElse
    ( IfThenElse(..)
    ) where

import "base" Data.Bool ( Bool(..), bool )

import "ghc-prim" GHC.Prim ( Proxy#, proxy# )

import "this" Std.Cat ( Functor, (<$>) )


-- | Allows functors to be used as conditions in if statements
-- @ifThenElse@ has two concrete implementations:
-- @ifThenElse :: Bool -> a -> a -> a@ which is the the standard one
-- @ifThenElse :: Functor f => f Bool -> a -> a -> f a@ where which is equivalent to @\fc a b -> map (\c -> if c then a else b) fc@
-- I considered a third implementation for @Monad@ where the branches are wrapped as well, but that would introduce to many ambiguities. 
class IfThenElse c b r | c b -> r, c r -> b where 
    ifThenElse :: c -> b -> b -> r

type family IsFunctor a where
    IsFunctor (f _) = 'True
    IsFunctor _     = 'False
instance IfThenElse' (IsFunctor c) c b r => IfThenElse c b r where
    ifThenElse = ifThenElse# (proxy# @(IsFunctor c))

class IfThenElse' (cf :: Bool) c b r | cf c b -> r, cf c r -> b where
    ifThenElse# :: Proxy# cf -> c -> b -> b -> r

instance              IfThenElse' 'False Bool     a a     where ifThenElse# _ c a b = bool a b c
instance Functor f => IfThenElse' 'True  (f Bool) a (f a) where ifThenElse# _ c a b = bool a b <$> c
