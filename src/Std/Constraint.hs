module Std.Constraint
    ( type Constraint
    , Unconstraint
    ) where

import "base" Data.Kind

class Unconstraint (a :: k)
instance Unconstraint a
