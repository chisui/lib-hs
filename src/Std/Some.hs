{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Std.Some
    ( Some(..), SomeT(..)
    , use, useT, use2, useT2
    , castSome, narrowSome
    , type (==>), Castable
    ) where

import "base" Prelude ( Maybe(..) )
import "base" GHC.Exception
import "base" Data.Typeable
import "base" Data.Kind

import "this" Std.Debug
import "this" Std.Literal
import "this" Std.Cat
import "this" Std.Ord
import "this" Std.Group


data Some c where
    Some :: c a => a -> Some c

data SomeT c f where
    SomeT :: c a => f a -> SomeT c f

use :: (forall a. c a => a -> b) -> Some c -> b
use f (Some a) = f a

useT :: (forall a. c a => f a -> b) -> SomeT c f -> b
useT f (SomeT a) = f a

use2 :: (forall a b. (c0 a, c1 b) => a -> b -> c) -> Some c0 -> Some c1 -> c
use2 f a = use (use f a)

useT2 :: (forall a b. (c0 a, c1 b) => f a -> g b -> c) -> SomeT c0 f -> SomeT c1 g -> c
useT2 f a = useT (useT f a)

type (==>) (c0 :: k -> Constraint) (c1 :: k -> Constraint) = (forall x. c0 x => c1 x) :: Constraint

class Typeable a => Castable a
instance Typeable a => Castable a

castSome :: (Castable a, c ==> Castable) => Some c -> Maybe a
castSome = use cast

narrowSome :: (c0 ==> c1) => Some c0 -> Some c1
narrowSome (Some a) = Some a


instance (c ==> Show) => Show (Some c) where
    showList = showListWith (\s -> showString "Some " . use shows s)
    show a = "Some " + use shows a ""
    showsPrec d (Some a)
        = showParen (d >= 10)
        $ showString "Some "
        . showsPrec 11 a

instance (Typeable c, c ==> Exception) => Exception (Some c) where
    toException = use toException
    displayException = use show
    fromException (SomeException e) = cast e
