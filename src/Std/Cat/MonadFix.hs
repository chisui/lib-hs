module Std.Cat.MonadFix where

import "this" Std.Cat.Class
import "this" Std.Cat.Monad
import "this" Std.Cat.Closed


class (Closed cat, CatMonad cat f) => CatMonadFix cat f where
    mfix :: Exp cat a (f a) `cat` f a
type MonadFix = CatMonadFix HASK
