module Std.Compose
    ( module Exp
    , type (.)
    ) where

import "base" Data.Functor.Compose as Exp

type (.) = Compose
