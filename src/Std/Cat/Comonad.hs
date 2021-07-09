module Std.Cat.Comonad where

import "base" Data.Functor.Identity
import "base" Data.Coerce

import "this" Std.Cat.Class
import "this" Std.Cat.Applicative
import "this" Std.Cat.Monad
import "this" Std.Cat.Op
import "this" Std.Cat.Functor


class EndoFunctor cat f => CatExtract cat f where
    extract :: f a `cat` a
type Extract = CatExtract HASK

instance CatPure cat f => CatExtract (Op cat) f where
    extract = Op catPure

class CatExtend cat m where
    (<<=) :: m a `cat` b -> m a `cat` m b
type Extend = CatExtend HASK

instance CatBind cat m => CatExtend (Op cat) m where
    (<<=) (Op f) = Op ((=<<) f)

class EndoFunctor cat f => CatDuplicate cat f where
    duplicate :: f a `cat` f (f a)
type Duplicate = CatDuplicate HASK

instance CatJoin cat f => CatDuplicate (Op cat) f where
    duplicate = Op join

class (Category cat, CatExtract cat f, CatExtend cat f, CatDuplicate cat f) => CatComonad cat f
type Comonad = CatComonad HASK

instance CatMonad cat m => CatComonad (Op cat) m

class (CatAp cat f, CatComonad cat f) => ComonadApply cat f

instance CatExtract   HASK Identity where extract = coerce
instance CatExtend    HASK Identity where (<<=) = coerce
instance CatDuplicate HASK Identity where duplicate = coerce
instance CatComonad   HASK Identity
