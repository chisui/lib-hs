{-# LANGUAGE TemplateHaskell #-}
module Std.TypeError
    ( TypeError, ErrorMessage(..)
    , typeError
    ) where

import "base" GHC.TypeLits ( TypeError, ErrorMessage(..) )

import "this" Std.Quote
import "this" Std.Cat.Class ( (.) )


typeError :: QuasiQuoter
typeError = (failingQuoter "typeError") { quoteType = makeTypeError . parseFormatString "" }
  where
    makeTypeError l = [t| TypeError $(makeErrorMessage l) |]

    makeErrorMessage = fromFormatAst
        (\l r -> [t| $l ':<>: $r |])
        (\l r -> [t| $l ':$$: $r |])
        (\a -> [t| 'Text     $(symbol a)    |])
        (\a -> [t| 'ShowType $(parseType a) |])
