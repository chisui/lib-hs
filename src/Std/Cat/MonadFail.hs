module Std.Cat.MonadFail where

import "base" Data.String
import "base" Prelude qualified as Base

import "this" Std.Type
import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Monad
import "this" Std.Cat.Iso


class CatMonad' c cat f => CatMonadFail' c (cat :: k -> k -> Type) f | f -> c where
    type FailMsg cat f :: k
    fail :: FailMsg cat f `cat` f a
type MonadFail' c = CatMonadFail' c HASK
type CatMonadFail = CatMonadFail' Unconstrained
type MonadFail    = CatMonadFail HASK

instance Base.MonadFail m => CatMonadFail' Unconstrained HASK (Basic1 m) where
    type FailMsg HASK (Basic1 m) = String
    fail :: forall a. String -> Basic1 m a
    fail = to coerce (Base.fail :: String -> m a)
