{-# LANGUAGE TemplateHaskell #-}
module Std.Text
    ( fmt
    ) where

import "base" Data.List qualified as Base
import "template-haskell" Language.Haskell.TH.Lib ( appE )
import "this" Std.Quote
import "this" Std.Cat ( (.) )


fmt :: QuasiQuoter
fmt = (failingQuoter "fmt") { quoteExp = makeString . parseFormatString "" }
  where
    makeString = fromFormatAst
        (appE . appE [e| (Base.++) |])
        (\l r -> [e| $l Base.++ ('\n' : $r) |])
        string
        (appE [e| show |] . parseExp)
