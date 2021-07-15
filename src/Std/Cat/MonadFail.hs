module Std.Cat.MonadFail where

import "base" Data.String
import "base" Prelude qualified as Base

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Monad
import "this" Std.Cat.Iso


class CatMonad cat f => CatMonadFail cat f where
    fail :: String `cat` f a
type MonadFail = CatMonadFail HASK

instance Base.MonadFail m => CatMonadFail HASK (Basic1 m) where
    fail :: forall a. String -> Basic1 m a
    fail = to coerce (Base.fail :: String -> m a)
