module Std.Cat.Bifunctor where

import "base" Data.Coerce
import "base" Prelude qualified as Base
import "base" Data.Bifunctor qualified as Base

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Constraint


class (Category r, Category t) => CatLeftFunctor c0 r t p | r t p -> c0 where
    left :: (c0 a, c0 b, c0 c) => r a b -> t (p a c) (p b c)
type EndoLeftFunctor c0 cat = CatLeftFunctor c0 cat cat
type LeftFunctor' c = EndoLeftFunctor c HASK
type LeftFunctor = LeftFunctor' Unconstraint

leftEndo :: CatLeftFunctor c0 cat cat p => (c0 a, c0 b, c0 c) => a `cat` b -> p a c `cat` p b c
leftEndo = left

class (Category s, Category t) => CatRightFunctor c0 s t q | s t q -> c0 where
    right :: (c0 a, c0 b, c0 c) => s a b -> t (q c a) (q c b)
type EndoRightFunctor c0 cat = CatRightFunctor c0 cat cat
type RightFunctor' c = EndoRightFunctor c HASK
type RightFunctor = RightFunctor' Unconstraint

rightEndo :: CatRightFunctor c0 cat cat p => (c0 a, c0 b, c0 c) => a `cat` b -> p c a `cat` p c b
rightEndo = right

class (CatLeftFunctor c0 r t p, CatRightFunctor c0 s t p) => CatBifunctor c0 r s t p | r s t p -> c0 where
    catBimap, (***) :: (c0 a, c0 b, c0 c, c0 d) => a `r` b -> c `s` d -> (a `p` c) `t` (b `p` d)
    catBimap f g = left f . right g
    (***) = catBimap
type EndoBifunctor c0 cat = CatBifunctor c0 cat cat cat
type Bifunctor' c = EndoBifunctor c HASK
type Bifunctor = Bifunctor' Unconstraint
bimap :: Bifunctor p => (a -> b) -> (a' -> b') -> p a a' -> p b b'
bimap = catBimap


newtype Left p x a = MkLeft
    { unLeft :: p a x
    }
instance LeftFunctor p => CatFunctor HASK HASK (Left p x) where
    map f (MkLeft a) = MkLeft (left f a)

newtype Right p x a = MkRight
    { unRight :: p x a
    }
instance RightFunctor p => CatFunctor HASK HASK (Right p x) where
    map f (MkRight a) = MkRight (right f a)



newtype Basic2 f a b = Basic2 (f a b)

instance Base.Bifunctor f => CatLeftFunctor Unconstraint HASK HASK (Basic2 f) where
    left :: forall a b c. (a -> b) -> Basic2 f a c -> Basic2 f b c
    left = coerce (Base.first :: (a -> b) -> f a c -> f b c)
instance Base.Bifunctor f => CatRightFunctor Unconstraint HASK HASK (Basic2 f) where
    right :: forall a b c. (a -> b) -> Basic2 f c a -> Basic2 f c b
    right = coerce (Base.second :: (a -> b) -> f c a -> f c b)
instance Base.Bifunctor f => CatBifunctor Unconstraint HASK HASK HASK (Basic2 f) where
    catBimap :: forall a b c d. (a -> b) -> (c -> d) -> Basic2 f a c -> Basic2 f b d
    catBimap = coerce (Base.bimap :: (a -> b) -> (c -> d) -> f a c -> f b d)

deriving via (Basic2 (,)) instance CatLeftFunctor    Unconstraint HASK HASK (,)
deriving via (Basic2 (,)) instance CatRightFunctor   Unconstraint HASK HASK (,)
deriving via (Basic2 (,)) instance CatBifunctor Unconstraint HASK HASK HASK (,)

deriving via (Basic2 Base.Either) instance CatLeftFunctor    Unconstraint HASK HASK Base.Either
deriving via (Basic2 Base.Either) instance CatRightFunctor   Unconstraint HASK HASK Base.Either
deriving via (Basic2 Base.Either) instance CatBifunctor Unconstraint HASK HASK HASK Base.Either
