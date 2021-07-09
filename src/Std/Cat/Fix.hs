module Std.Cat.Fix where

import "this" Std.Cat.Class
import "this" Std.Cat.Monad
import "this" Std.Cat.Distributive
import "this" Std.Cat.Closed
import "this" Std.Cat.Kleisli


class Closed cat => CatFix cat where
    fix :: Exp cat a a `cat` a

instance CatFix HASK where fix f = let x = f x in x


class (Closed cat, CatMonad cat f) => CatMonadFix cat f where
    mfix :: Exp cat a (f a) `cat` f a

type MonadFix = CatMonadFix HASK

instance (MonadFix m, Distributive m) => CatFix (Kleisli m) where
    fix = Kleisli (mfix . unKleisli)
