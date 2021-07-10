{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Debug
    ( module Exp
    , HasCallStack
    ) where

import "base" GHC.Err as Exp ( error )
import "base" Text.Show as Exp
import "base" GHC.Stack ( HasCallStack )

import "this" Std.Partial
import "this" Std.Literal
import "this" Std.Ord
import "this" Std.Cat


instance Show a => Show (Res t a) where
    showsPrec _ EmptyRes = showString "EmptyRes"
    showsPrec d (FullRes a)
        = showParen (d >= 10)
        $ showString "FullRes "
        . showsPrec 11 a
