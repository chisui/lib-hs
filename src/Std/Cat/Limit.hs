module Std.Cat.Limit where

import "base" Data.Void

import "this" Std.Type
import "this" Std.Cat.Class


class Category cat => CatTerminal (cat :: k -> k -> Type) where
    type Terminal cat :: k
    terminate :: a `cat` Terminal cat

class Category cat => CatInitial (cat :: k -> k -> Type) where
    type Initial cat :: k
    initiate :: Initial cat `cat` a


instance CatTerminal HASK where
    type Terminal HASK = ()
    terminate _ = ()
instance CatInitial HASK where
    type Initial HASK = Void
    initiate = absurd
