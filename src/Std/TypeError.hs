{-# LANGUAGE TemplateHaskell #-}
module Std.TypeError
    ( TypeError, type ErrorMessage(..)
    , typeError
    ) where

import "base" GHC.TypeLits ( TypeError, type ErrorMessage(..) )

import "this" Std.Quote
import "this" Std.Cat ( (.), ($) )
import "this" Std.Basic ()


typeError :: QuasiQuoter
typeError = failingQuoter { quoteType = makeTypeError . parseFormatString "" }
  where
    makeTypeError l = [t| TypeError $(makeErrorMessage l) |]

    makeErrorMessage = fromFormatAst
        (\l r -> [t| $l ':<>: $r |])
        (\l r -> [t| $l ':$$: $r |])
        $ \case
            Literal   a -> [t| 'Text     $(symbol a)    |]
            AntiQuote a -> [t| 'ShowType $(parseType a) |]
