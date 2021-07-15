module Std.Cat.Monad where

import "base" Data.Bool ( Bool )
import "base" Data.Kind
import "base" Data.Coerce
import "base" Control.Monad qualified as Base
import "base" Data.Functor.Identity qualified as Base

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Closed
import "this" Std.Cat.Applicative


class EndoFunctor cat f => CatJoin cat f where
    join :: f (f a) `cat` f a
type Join = CatJoin HASK

class CatBind cat m where
    (=<<) :: a `cat` m b -> m a `cat` m b
infixr 1 =<<
type Bind = CatBind HASK

class (Category cat, EndoFunctor cat f, CatPure cat f, CatBind cat f, CatJoin cat f) => CatMonad (cat :: k -> k -> Type) f
type Monad = CatMonad HASK

(>>=) :: Bind m => m a -> (a -> m b) -> m b
m >>= f = f =<< m
infixl 1 >>=

(<=<) :: CatMonad cat m => b `cat` m c -> a `cat` m b -> a `cat` m c
f <=< g = (=<<) f . (=<<) g . catPure
infixr 1 <=<

filter :: (Alternative f, Monad f) => (a -> Bool) -> f a -> f a
filter p = (=<<) (\a -> guard (p a) $> a)

instance Base.Monad f => CatBind  HASK (Basic1 f) where
    (=<<) :: forall a b. (a -> Basic1 f b) -> Basic1 f a -> Basic1 f b
    (=<<) = coerce ((Base.=<<) :: (a -> f b) -> f a -> f b)
instance Base.Monad f => CatJoin  HASK (Basic1 f) where
    join :: forall a. Basic1 f (Basic1 f a) -> Basic1 f a
    join (Basic1 f) = Basic1 (getBasic1 Base.=<< f)
instance Base.Monad f => CatMonad HASK (Basic1 f)

deriving via (Basic1 Base.Identity) instance CatBind  HASK Base.Identity
deriving via (Basic1 Base.Identity) instance CatJoin  HASK Base.Identity
deriving via (Basic1 Base.Identity) instance CatMonad HASK Base.Identity

deriving via (Basic1 ((->) a)) instance CatBind  HASK ((->) a)
deriving via (Basic1 ((->) a)) instance CatJoin  HASK ((->) a)
deriving via (Basic1 ((->) a)) instance CatMonad HASK ((->) a)
