module Std.Cat.Bifunctor where

import "base" Data.Coerce
import "base" Prelude qualified as Base
import "base" Data.Bifunctor qualified as Base
import "base" Data.Functor.Const qualified as Base

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Type


class (Category r, Category t) => CatLeftFunctor' c0 r t p | r t p -> c0 where
    left' :: (c0 a, c0 b, c0 c) => r a b -> t (p a c) (p b c)
type CatLeftFunctor = CatLeftFunctor' Unconstrained
type EndoLeftFunctor' c0 cat = CatLeftFunctor' c0 cat cat
type EndoLeftFunctor cat = EndoLeftFunctor' Unconstrained cat
type LeftFunctor' c = EndoLeftFunctor' c HASK
type LeftFunctor = LeftFunctor' Unconstrained

left :: EndoLeftFunctor cat p => a `cat` b -> p a c `cat` p b c
left = left'
leftEndo' :: (EndoLeftFunctor' c0 cat p, c0 a, c0 b, c0 c) => a `cat` b -> p a c `cat` p b c
leftEndo' = left'
leftEndo :: EndoLeftFunctor cat p => a `cat` b -> p a c `cat` p b c
leftEndo = left

class (Category s, Category t) => CatRightFunctor' c0 s t q | s t q -> c0 where
    right' :: (c0 a, c0 b, c0 c) => s a b -> t (q c a) (q c b)
type CatRightFunctor = CatRightFunctor' Unconstrained
type EndoRightFunctor' c0 cat = CatRightFunctor' c0 cat cat
type EndoRightFunctor cat = EndoRightFunctor' Unconstrained cat
type RightFunctor' c = EndoRightFunctor' c HASK
type RightFunctor = RightFunctor' Unconstrained

right :: CatRightFunctor s t q => s a b -> t (q c a) (q c b)
right = right'
rightEndo' :: (EndoRightFunctor' c0 cat p, c0 a, c0 b, c0 c) => a `cat` b -> p c a `cat` p c b
rightEndo' = right'
rightEndo :: EndoRightFunctor cat p => a `cat` b -> p c a `cat` p c b
rightEndo = right


class (CatLeftFunctor' c0 r t p, CatRightFunctor' c0 s t p) => CatBifunctor' c0 r s t p | r s t p -> c0 where
    catBimap', (***) :: (c0 a, c0 b, c0 c, c0 d) => a `r` b -> c `s` d -> (a `p` c) `t` (b `p` d)
    catBimap' f g = left' f . right' g
    (***) = catBimap'
infixr 3 ***
type CatBifunctor = CatBifunctor' Unconstrained
type EndoBifunctor' c0 cat = CatBifunctor' c0 cat cat cat
type EndoBifunctor cat = EndoBifunctor' Unconstrained cat
type Bifunctor' c = EndoBifunctor' c HASK
type Bifunctor = Bifunctor' Unconstrained

catBimap :: CatBifunctor r s t p => a `r` b -> c `s` d -> (a `p` c) `t` (b `p` d)
catBimap = catBimap'
bimap' :: (Bifunctor' c0 p, c0 a, c0 a', c0 b, c0 b') => (a -> b) -> (a' -> b') -> p a a' -> p b b'
bimap' = catBimap'
bimap :: Bifunctor p => (a -> b) -> (a' -> b') -> p a a' -> p b b'
bimap = catBimap


instance Base.Bifunctor f => CatLeftFunctor' Unconstrained HASK HASK (Basic2 f) where
    left' :: forall a b c. (a -> b) -> Basic2 f a c -> Basic2 f b c
    left' = coerce (Base.first :: (a -> b) -> f a c -> f b c)
instance Base.Bifunctor f => CatRightFunctor' Unconstrained HASK HASK (Basic2 f) where
    right' :: forall a b c. (a -> b) -> Basic2 f c a -> Basic2 f c b
    right' = coerce (Base.second :: (a -> b) -> f c a -> f c b)
instance Base.Bifunctor f => CatBifunctor' Unconstrained HASK HASK HASK (Basic2 f) where
    catBimap' :: forall a b c d. (a -> b) -> (c -> d) -> Basic2 f a c -> Basic2 f b d
    catBimap' = coerce (Base.bimap :: (a -> b) -> (c -> d) -> f a c -> f b d)

deriving via (Basic2 (,)) instance CatLeftFunctor'    Unconstrained HASK HASK (,)
deriving via (Basic2 (,)) instance CatRightFunctor'   Unconstrained HASK HASK (,)
deriving via (Basic2 (,)) instance CatBifunctor' Unconstrained HASK HASK HASK (,)

deriving via (Basic2 ((,,) a)) instance CatLeftFunctor'    Unconstrained HASK HASK ((,,) a)
deriving via (Basic2 ((,,) a)) instance CatRightFunctor'   Unconstrained HASK HASK ((,,) a)
deriving via (Basic2 ((,,) a)) instance CatBifunctor' Unconstrained HASK HASK HASK ((,,) a)

deriving via (Basic2 ((,,,) a b)) instance CatLeftFunctor'    Unconstrained HASK HASK ((,,,) a b)
deriving via (Basic2 ((,,,) a b)) instance CatRightFunctor'   Unconstrained HASK HASK ((,,,) a b)
deriving via (Basic2 ((,,,) a b)) instance CatBifunctor' Unconstrained HASK HASK HASK ((,,,) a b)

deriving via (Basic2 ((,,,,) a b c)) instance CatLeftFunctor'    Unconstrained HASK HASK ((,,,,) a b c)
deriving via (Basic2 ((,,,,) a b c)) instance CatRightFunctor'   Unconstrained HASK HASK ((,,,,) a b c)
deriving via (Basic2 ((,,,,) a b c)) instance CatBifunctor' Unconstrained HASK HASK HASK ((,,,,) a b c)

deriving via (Basic2 ((,,,,,) a b c d)) instance CatLeftFunctor'    Unconstrained HASK HASK ((,,,,,) a b c d)
deriving via (Basic2 ((,,,,,) a b c d)) instance CatRightFunctor'   Unconstrained HASK HASK ((,,,,,) a b c d)
deriving via (Basic2 ((,,,,,) a b c d)) instance CatBifunctor' Unconstrained HASK HASK HASK ((,,,,,) a b c d)

deriving via (Basic2 ((,,,,,,) a b c d e)) instance CatLeftFunctor'    Unconstrained HASK HASK ((,,,,,,) a b c d e)
deriving via (Basic2 ((,,,,,,) a b c d e)) instance CatRightFunctor'   Unconstrained HASK HASK ((,,,,,,) a b c d e)
deriving via (Basic2 ((,,,,,,) a b c d e)) instance CatBifunctor' Unconstrained HASK HASK HASK ((,,,,,,) a b c d e)

deriving via (Basic2 Base.Either) instance CatLeftFunctor'    Unconstrained HASK HASK Base.Either
deriving via (Basic2 Base.Either) instance CatRightFunctor'   Unconstrained HASK HASK Base.Either
deriving via (Basic2 Base.Either) instance CatBifunctor' Unconstrained HASK HASK HASK Base.Either

deriving via (Basic2 Base.Const) instance CatLeftFunctor'    Unconstrained HASK HASK Base.Const
deriving via (Basic2 Base.Const) instance CatRightFunctor'   Unconstrained HASK HASK Base.Const
deriving via (Basic2 Base.Const) instance CatBifunctor' Unconstrained HASK HASK HASK Base.Const




newtype Left p x a = MkLeft
    { unLeft :: p a x
    }
instance LeftFunctor p => CatFunctor HASK HASK (Left p x) where
    catMap f (MkLeft a) = MkLeft (left f a)

newtype Right p x a = MkRight
    { unRight :: p x a
    }
instance RightFunctor p => CatFunctor HASK HASK (Right p x) where
    catMap f (MkRight a) = MkRight (right f a)
