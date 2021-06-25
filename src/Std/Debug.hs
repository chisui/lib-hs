{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
module Std.Debug
    ( module Exp
    , error
    , HasCallStack
    ) where

import "base" Text.Show as Exp
import "base" Prelude qualified as Base
import "base" GHC.Stack ( HasCallStack )

import "this" Std.Quote
import "this" Std.Cat ( (.), ($) )


error :: QuasiQuoter
error = failingQuoter { quoteExp = makeError . parseFormatString "" }
  where
    makeError l = [e| Base.error $(makeErrorMessage l) |]

    makeErrorMessage = fromFormatAst
        (\l r -> [e| $l Base.++ $r |])
        (\l r -> [e| $l Base.++ ('\n' : $r) |])
        $ \case
            Literal   a -> string a
            AntiQuote a -> [e| show $(parseExp a) |]
