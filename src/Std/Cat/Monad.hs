module Std.Cat.Monad where

import "base" Data.Bool ( Bool )
import "base" Data.Coerce
import "base" Control.Monad qualified as Base
import "base" Data.Functor.Identity qualified as Base

import "this" Std.Type
import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Closed
import "this" Std.Cat.Applicative


class EndoFunctor' c cat f => CatJoin' c cat f | f -> c where
    join :: (c (f a), c a) => f (f a) `cat` f a
type CatJoin = CatJoin' Unconstrained
type Join' c = CatJoin' c HASK
type Join    = CatJoin HASK

class CatBind' c cat m | m -> c where
    (=<<) :: (c a, c b) => a `cat` m b -> m a `cat` m b
infixr 1 =<<
type CatBind = CatBind' Unconstrained
type Bind' c = CatBind' c HASK
type Bind    = CatBind HASK

class (Category cat, EndoFunctor' c cat f, CatPure' c cat f, CatBind' c cat f, CatJoin' c cat f) => CatMonad' c (cat :: k -> k -> Type) f | f -> c
type CatMonad = CatMonad' Unconstrained
type Monad' c = CatMonad' c HASK
type Monad    = CatMonad HASK

(>>=) :: (Bind' c m, c a, c b) => m a -> (a -> m b) -> m b
m >>= f = f =<< m
infixl 1 >>=

(<=<) :: (CatMonad' c cat m, c a, c b, c d) => b `cat` m d -> a `cat` m b -> a `cat` m d
f <=< g = (=<<) f . (=<<) g . catPure
infixr 1 <=<

filter :: (Alternative f, Monad f) => (a -> Bool) -> f a -> f a
filter p = (=<<) (\a -> guard (p a) $> a)

instance Base.Monad f => CatBind' Unconstrained HASK (Basic1 f) where
    (=<<) :: forall a b. (a -> Basic1 f b) -> Basic1 f a -> Basic1 f b
    (=<<) = coerce ((Base.=<<) :: (a -> f b) -> f a -> f b)
instance Base.Monad f => CatJoin' Unconstrained HASK (Basic1 f) where
    join :: forall a. Basic1 f (Basic1 f a) -> Basic1 f a
    join (Basic1 f) = Basic1 (getBasic1 Base.=<< f)
instance Base.Monad f => CatMonad' Unconstrained HASK (Basic1 f)

deriving via (Basic1 Base.Identity) instance CatBind'  Unconstrained HASK Base.Identity
deriving via (Basic1 Base.Identity) instance CatJoin'  Unconstrained HASK Base.Identity
deriving via (Basic1 Base.Identity) instance CatMonad' Unconstrained HASK Base.Identity

deriving via (Basic1 ((->) a)) instance CatBind'  Unconstrained HASK ((->) a)
deriving via (Basic1 ((->) a)) instance CatJoin'  Unconstrained HASK ((->) a)
deriving via (Basic1 ((->) a)) instance CatMonad' Unconstrained HASK ((->) a)
