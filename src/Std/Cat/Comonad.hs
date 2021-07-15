module Std.Cat.Comonad where

import "base" Data.Functor.Identity
import "base" Data.List.NonEmpty qualified as Base
import "base" Data.List.NonEmpty ( NonEmpty(..) )
import "base" Data.Coerce

import "this" Std.Cat.Class
import "this" Std.Cat.Closed
import "this" Std.Cat.Applicative
import "this" Std.Cat.Monad
import "this" Std.Cat.Op
import "this" Std.Cat.Functor


class EndoFunctor cat f => CatExtract cat f where
    extract :: f a `cat` a
type Extract = CatExtract HASK

class CatExtend cat m where
    (<<=) :: m a `cat` b -> m a `cat` m b
type Extend = CatExtend HASK

(=>>) :: Extend m => m a -> (m a -> b) -> m b
(=>>) = flip (<<=)


class EndoFunctor cat f => CatDuplicate cat f where
    duplicate :: f a `cat` f (f a)
type Duplicate = CatDuplicate HASK

class (Category cat, CatExtract cat f, CatExtend cat f, CatDuplicate cat f) => CatComonad cat f
type Comonad = CatComonad HASK

class (CatAp cat f, CatComonad cat f) => CatComonadApply cat f

class Closed cat => CatUnlift2 cat f where
    unlift2 :: f ((Exp cat a r) `cat` Exp cat (Exp cat b r) r)
            `cat` Exp cat (Exp cat (f a) r) (Exp cat (Exp cat (f b) r) r)
type Unlift2 = CatUnlift2 HASK

class (CatExtract cat f, CatUnlift2 cat f) => CatCoapplicative cat f
type Coapplicative = CatCoapplicative HASK

instance CatPure  cat f => CatExtract   (Op cat) f where extract = Op catPure
instance CatJoin  cat f => CatDuplicate (Op cat) f where duplicate = Op join
instance CatBind  cat f => CatExtend    (Op cat) f where (<<=) (Op f) = Op ((=<<) f)
instance CatMonad cat f => CatComonad   (Op cat) f

instance CatExtract       HASK Identity where extract   = coerce
instance CatExtend        HASK Identity where (<<=)     = coerce
instance CatDuplicate     HASK Identity where duplicate = coerce
instance CatComonad       HASK Identity
instance CatComonadApply  HASK Identity
instance CatUnlift2       HASK Identity where unlift2 = coerce
instance CatCoapplicative HASK Identity

instance CatExtract   HASK NonEmpty where extract (a :| _) = a
instance CatDuplicate HASK NonEmpty where duplicate = (<<=) id
instance CatExtend    HASK NonEmpty where
    f <<= w@(~(_ :| aas)) =
        f w :| case aas of
            []     -> []
            (a:as) -> Base.toList (f <<= (a :| as))
instance CatComonad HASK NonEmpty
instance CatUnlift2 HASK NonEmpty where
    unlift2 (f :| _) a b = f (a . pure) (b . pure)
