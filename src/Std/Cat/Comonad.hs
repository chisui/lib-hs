{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Comonad where

import "base" Data.Functor.Identity
import "base" Data.List.NonEmpty qualified as Base
import "base" Data.List.NonEmpty ( NonEmpty(..) )
import "base" Data.Coerce

import "this" Std.Type
import "this" Std.Cat.Class
import "this" Std.Cat.Closed
import "this" Std.Cat.Applicative
import "this" Std.Cat.Monad
import "this" Std.Cat.Op
import "this" Std.Cat.Functor


class EndoFunctor' c cat f => CatExtract' c cat f | f -> c where
    extract :: c a => f a `cat` a
type CatExtract = CatExtract' Unconstrained
type Extract' c = CatExtract' c HASK
type Extract    = CatExtract HASK

class Closed cat => CatExtend' c cat m | m -> c where
    (<<=) :: (c a, c b) => Exp cat (m a) b `cat` Exp cat (m a) (m b)
type CatExtend = CatExtend' Unconstrained
type Extend' c = CatExtend' c HASK
type Extend    = CatExtend HASK

(=>>) :: forall m a b c. (Extend' c m, c a, c b) => m a -> (m a -> b) -> m b
(=>>) = flip (<<=)


class EndoFunctor' c cat f => CatDuplicate' c cat f | f -> c where
    duplicate :: (c a, c (f a)) => f a `cat` f (f a)
type CatDuplicate = CatDuplicate' Unconstrained
type Duplicate' c = CatDuplicate' c HASK
type Duplicate    = CatDuplicate HASK

class (Category cat, CatExtract' c cat f, CatExtend' c cat f, CatDuplicate' c cat f) => CatComonad' c cat f | f -> c
type CatComonad = CatComonad' Unconstrained
type Comonad' c = CatComonad' c HASK
type Comonad    = CatComonad HASK

class (CatAp' c cat f, CatComonad' c cat f) => CatComonadApply' c cat f | f -> c
type CatComonadApply = CatComonadApply' Unconstrained
type ComonadApply' c = CatComonadApply' c HASK
type ComonadApply    = CatComonadApply HASK

class Closed cat => CatUnlift2' (c :: k -> Constraint) (cat :: k -> k -> Type) (f :: k -> k) | f -> c where
    unlift2 :: (c a, c b, c r)
            => f (Exp cat (Exp cat a r) (Exp cat (Exp cat b r) r))
            `cat` Exp cat (Exp cat (f a) r) (Exp cat (Exp cat (f b) r) r)
type CatUnlift2 = CatUnlift2' Unconstrained
type Unlift2' c = CatUnlift2' c HASK
type Unlift2    = CatUnlift2 HASK

class (CatExtract' c cat f, CatUnlift2' c cat f) => CatCoapplicative' c cat f | f -> c
type CatCoapplicative = CatCoapplicative' Unconstrained
type Coapplicative' c = CatCoapplicative' c HASK
type Coapplicative    = CatCoapplicative HASK

instance CatPure'  c cat f => CatExtract'   c (Op cat) f where extract = Op catPure
instance CatJoin'  c cat f => CatDuplicate' c (Op cat) f where duplicate = Op join

instance CatExtract'       Unconstrained HASK Identity where extract   = coerce
instance CatExtend'        Unconstrained HASK Identity where (<<=)     = coerce
instance CatDuplicate'     Unconstrained HASK Identity where duplicate = coerce
instance CatComonad'       Unconstrained HASK Identity
instance CatComonadApply'  Unconstrained HASK Identity
instance CatUnlift2'       Unconstrained HASK Identity where unlift2 = coerce
instance CatCoapplicative' Unconstrained HASK Identity

instance CatExtract'   Unconstrained HASK NonEmpty where extract (a :| _) = a
instance CatDuplicate' Unconstrained HASK NonEmpty where duplicate = (<<=) id
instance CatExtend'    Unconstrained HASK NonEmpty where
    f <<= w@(~(_ :| aas)) =
        f w :| case aas of
            []     -> []
            (a:as) -> Base.toList (f <<= (a :| as))
instance CatComonad' Unconstrained HASK NonEmpty
instance CatUnlift2' Unconstrained HASK NonEmpty where
    unlift2 (f :| _) a b = f (a . pure) (b . pure)
