module Std.Cat.Limit where

import "base" Data.Void

import "this" Std.Cat.Class


class Category cat => CatTerminal cat where
    type Terminal cat
    terminate :: a `cat` Terminal cat

class Category cat => CatInitial cat where
    type Initial cat
    initiate :: Initial cat `cat` a


instance CatTerminal HASK where
    type Terminal HASK = ()
    terminate _ = ()
instance CatInitial HASK where
    type Initial HASK = Void
    initiate = absurd
