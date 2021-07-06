module Std.Cat.Functor where

import "base" Prelude qualified as Base
import "base" Data.List.NonEmpty qualified as Base
import "base" Data.Proxy qualified as Base
import "base" Data.Coerce
import "base" Data.Functor.Identity qualified as Base
import "base" Data.Functor.Const qualified as Base
import "base" Data.Kind

import "this" Std.Cat.Class


class CatFunctor (c0 :: k0 -> k0 -> Type) (c1 :: k1 -> k1 -> Type) (f :: k0 -> k1) where
    map, (<$>) :: a `c0` b -> f a `c1` f b
    map = (<$>)
    (<$>) = map
    {-# MINIMAL map | (<$>) #-}

type EndoFunctor c = CatFunctor c c
type Functor = EndoFunctor HASK

mapEndo :: EndoFunctor cat f => a `cat` b -> f a `cat` f b
mapEndo = map


newtype Basic1 f a = Basic1
    { unBasic1 :: f a
    }

instance Base.Functor f => CatFunctor HASK HASK (Basic1 f) where
    map :: forall a b. (a -> b) -> Basic1 f a -> Basic1 f b
    map = coerce (Base.fmap :: (a -> b) -> f a -> f b)

deriving via (Basic1 ((->) r)) instance CatFunctor HASK HASK ((->) r)
deriving via (Basic1 []) instance CatFunctor HASK HASK []
deriving via (Basic1 ((,) a)) instance CatFunctor HASK HASK ((,) a)
deriving via (Basic1 ((,,) a b)) instance CatFunctor HASK HASK ((,,) a b)
deriving via (Basic1 ((,,,) a b c)) instance CatFunctor HASK HASK ((,,,) a b c)

deriving via (Basic1 Base.Identity) instance CatFunctor HASK HASK Base.Identity
deriving via (Basic1 Base.Maybe) instance CatFunctor HASK HASK Base.Maybe
deriving via (Basic1 Base.IO) instance CatFunctor HASK HASK Base.IO
deriving via (Basic1 Base.NonEmpty) instance CatFunctor HASK HASK Base.NonEmpty
deriving via (Basic1 (Base.Either a)) instance CatFunctor HASK HASK (Base.Either a)
deriving via (Basic1 Base.Proxy) instance CatFunctor HASK HASK Base.Proxy
deriving via (Basic1 (Base.Const m)) instance CatFunctor HASK HASK (Base.Const m)
