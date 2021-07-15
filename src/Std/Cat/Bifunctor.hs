{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Bifunctor where

import "base" Data.Coerce
import "base" Prelude qualified as Base
import "base" Data.Bifunctor qualified as Base
import "base" Data.Functor.Const qualified as Base

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Type


class (Category r, Category t) => CatLeftFunctor' c0 c1 r t p | r t p -> c0 c1 where
    left' :: (c0 a, c0 b, c1 c) => r a b -> t (p a c) (p b c)
type CatLeftFunctor = CatLeftFunctor' Unconstrained Unconstrained
type EndoLeftFunctor' c0 c1 cat = CatLeftFunctor' c0 c1 cat cat
type EndoLeftFunctor cat = EndoLeftFunctor' Unconstrained Unconstrained cat
type LeftFunctor' c0 c1 = EndoLeftFunctor' c0 c1 HASK
type LeftFunctor = LeftFunctor' Unconstrained Unconstrained

left :: EndoLeftFunctor cat p => a `cat` b -> p a c `cat` p b c
left = left'
leftEndo' :: (EndoLeftFunctor' c0 c1 cat p, c0 a, c0 b, c1 c) => a `cat` b -> p a c `cat` p b c
leftEndo' = left'
leftEndo :: EndoLeftFunctor cat p => a `cat` b -> p a c `cat` p b c
leftEndo = left

class (Category s, Category t) => CatRightFunctor' c0 c1 s t q | s t q -> c0 c1 where
    right' :: (c1 a, c1 b, c0 c) => s a b -> t (q c a) (q c b)
type CatRightFunctor = CatRightFunctor' Unconstrained Unconstrained
type EndoRightFunctor' c0 c1 cat = CatRightFunctor' c0 c1 cat cat
type EndoRightFunctor cat = EndoRightFunctor' Unconstrained Unconstrained cat
type RightFunctor' c0 c1 = EndoRightFunctor' c0 c1 HASK
type RightFunctor = RightFunctor' Unconstrained Unconstrained

right :: CatRightFunctor s t q => s a b -> t (q c a) (q c b)
right = right'
rightEndo' :: (EndoRightFunctor' c0 c1 cat p, c1 a, c1 b, c0 c) => a `cat` b -> p c a `cat` p c b
rightEndo' = right'
rightEndo :: EndoRightFunctor cat p => a `cat` b -> p c a `cat` p c b
rightEndo = right


class (CatLeftFunctor' c0 c1 r t p, CatRightFunctor' c0 c1 s t p) => CatBifunctor' c0 c1 r s t p | r t p -> c0 c1, s t p -> c0 c1 where
    catBimap', (***) :: (c0 a, c0 b, c1 c, c1 d) => a `r` b -> c `s` d -> (a `p` c) `t` (b `p` d)
    catBimap' f g = left' f . right' g
    (***) = catBimap'
infixr 3 ***
type CatBifunctor = CatBifunctor' Unconstrained Unconstrained
type EndoBifunctor' c0 c1 cat = CatBifunctor' c0 c1 cat cat cat
type EndoBifunctor cat = EndoBifunctor' Unconstrained Unconstrained cat
type Bifunctor' c0 c1 = EndoBifunctor' c0 c1 HASK
type Bifunctor = Bifunctor' Unconstrained Unconstrained

catBimap :: CatBifunctor r s t p => a `r` b -> c `s` d -> (a `p` c) `t` (b `p` d)
catBimap = catBimap'
bimap' :: (Bifunctor' c0 c1 p, c0 a, c1 a', c0 b, c1 b') => (a -> b) -> (a' -> b') -> p a a' -> p b b'
bimap' = catBimap'
bimap :: Bifunctor p => (a -> b) -> (a' -> b') -> p a a' -> p b b'
bimap = catBimap


instance Base.Bifunctor f => CatLeftFunctor' Unconstrained Unconstrained HASK HASK (Basic2 f) where
    left' :: forall a b c. (a -> b) -> Basic2 f a c -> Basic2 f b c
    left' = coerce (Base.first :: (a -> b) -> f a c -> f b c)
instance Base.Bifunctor f => CatRightFunctor' Unconstrained Unconstrained HASK HASK (Basic2 f) where
    right' :: forall a b c. (a -> b) -> Basic2 f c a -> Basic2 f c b
    right' = coerce (Base.second :: (a -> b) -> f c a -> f c b)
instance Base.Bifunctor f => CatBifunctor' Unconstrained Unconstrained HASK HASK HASK (Basic2 f) where
    catBimap' :: forall a b c d. (a -> b) -> (c -> d) -> Basic2 f a c -> Basic2 f b d
    catBimap' = coerce (Base.bimap :: (a -> b) -> (c -> d) -> f a c -> f b d)

deriving via (Basic2 (,)) instance CatLeftFunctor'   Unconstrained Unconstrained HASK HASK (,)
deriving via (Basic2 (,)) instance CatRightFunctor'  Unconstrained Unconstrained HASK HASK (,)
deriving via (Basic2 (,)) instance CatBifunctor' Unconstrained Unconstrained HASK HASK HASK (,)

deriving via (Basic2 ((,,) a)) instance CatLeftFunctor'   Unconstrained Unconstrained HASK HASK ((,,) a)
deriving via (Basic2 ((,,) a)) instance CatRightFunctor'  Unconstrained Unconstrained HASK HASK ((,,) a)
deriving via (Basic2 ((,,) a)) instance CatBifunctor' Unconstrained Unconstrained HASK HASK HASK ((,,) a)

deriving via (Basic2 ((,,,) a b)) instance CatLeftFunctor'   Unconstrained Unconstrained HASK HASK ((,,,) a b)
deriving via (Basic2 ((,,,) a b)) instance CatRightFunctor'  Unconstrained Unconstrained HASK HASK ((,,,) a b)
deriving via (Basic2 ((,,,) a b)) instance CatBifunctor' Unconstrained Unconstrained HASK HASK HASK ((,,,) a b)

deriving via (Basic2 ((,,,,) a b c)) instance CatLeftFunctor'   Unconstrained Unconstrained HASK HASK ((,,,,) a b c)
deriving via (Basic2 ((,,,,) a b c)) instance CatRightFunctor'  Unconstrained Unconstrained HASK HASK ((,,,,) a b c)
deriving via (Basic2 ((,,,,) a b c)) instance CatBifunctor' Unconstrained Unconstrained HASK HASK HASK ((,,,,) a b c)

deriving via (Basic2 ((,,,,,) a b c d)) instance CatLeftFunctor'   Unconstrained Unconstrained HASK HASK ((,,,,,) a b c d)
deriving via (Basic2 ((,,,,,) a b c d)) instance CatRightFunctor'  Unconstrained Unconstrained HASK HASK ((,,,,,) a b c d)
deriving via (Basic2 ((,,,,,) a b c d)) instance CatBifunctor' Unconstrained Unconstrained HASK HASK HASK ((,,,,,) a b c d)

deriving via (Basic2 ((,,,,,,) a b c d e)) instance CatLeftFunctor'  Unconstrained Unconstrained HASK HASK ((,,,,,,) a b c d e)
deriving via (Basic2 ((,,,,,,) a b c d e)) instance CatRightFunctor' Unconstrained Unconstrained HASK HASK ((,,,,,,) a b c d e)
deriving via (Basic2 ((,,,,,,) a b c d e)) instance CatBifunctor' Unconstrained Unconstrained HASK HASK HASK ((,,,,,,) a b c d e)

deriving via (Basic2 Base.Either) instance CatLeftFunctor'    Unconstrained Unconstrained HASK HASK Base.Either
deriving via (Basic2 Base.Either) instance CatRightFunctor'   Unconstrained Unconstrained HASK HASK Base.Either
deriving via (Basic2 Base.Either) instance CatBifunctor' Unconstrained Unconstrained HASK HASK HASK Base.Either

instance CatLeftFunctor' Unconstrained Unconstrained HASK HASK Base.Const where
    left' f (Base.Const a) = Base.Const (f a)
instance Category cat => CatRightFunctor' Unconstrained Unconstrained cat HASK Base.Const where
    right' _ (Base.Const a) = Base.Const a
instance Category cat => CatBifunctor' Unconstrained Unconstrained HASK cat HASK Base.Const




newtype Left p x a = MkLeft
    { unLeft :: p a x
    }
instance LeftFunctor' c Unconstrained p => CatFunctor' c HASK HASK (Left p x) where
    catMap f (MkLeft a) = MkLeft (left' f a)

newtype Right p x a = MkRight
    { unRight :: p x a
    }
instance RightFunctor' Unconstrained c p => CatFunctor' c HASK HASK (Right p x) where
    catMap f (MkRight a) = MkRight (right' f a)
