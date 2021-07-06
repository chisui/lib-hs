{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
module Std.Debug
    ( module Exp
    , HasCallStack
    ) where

import "base" GHC.Err as Exp ( error )
import "base" Text.Show as Exp
import "base" GHC.Stack ( HasCallStack )
