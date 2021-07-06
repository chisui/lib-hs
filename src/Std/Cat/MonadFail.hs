module Std.Cat.MonadFail where

import "base" Data.String

import "this" Std.Cat.Class
import "this" Std.Cat.Monad


class CatMonad cat f => CatMonadFail cat f where
    fail :: String `cat` f a
type MonadFail = CatMonadFail HASK
