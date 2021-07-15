module Std.Cat.Functor
    ( CatFunctor(..), EndoFunctor, Functor
    , mapEndo, map, (<$>), Basic1(..)
    ) where

import "base" Prelude qualified as Base
import "base" Data.List.NonEmpty qualified as Base
import "base" Data.Proxy qualified as Base
import "base" Data.Coerce
import "base" Data.Functor.Identity qualified as Base
import "base" Data.Functor.Const qualified as Base
import "base" Data.Semigroup qualified as Base ( Min(..), Max(..) )
import "base" Data.Monoid qualified as Base ( First(..), Last(..) )
import "base" Data.Kind

import "this" Std.Cat.Class


class (Category c0, Category c1) => CatFunctor (c0 :: k0 -> k0 -> Type) (c1 :: k1 -> k1 -> Type) (f :: k0 -> k1) where
    catMap, (<$$>) :: a `c0` b -> f a `c1` f b
    catMap = (<$$>)
    (<$$>) = catMap
    {-# MINIMAL catMap | (<$$>) #-}
infixl 4 <$$>

type EndoFunctor c = CatFunctor c c
type Functor = EndoFunctor HASK

mapEndo :: EndoFunctor cat f => a `cat` b -> f a `cat` f b
mapEndo = catMap

map :: Functor f => (a -> b) -> f a -> f b
map = catMap

(<$>) :: Functor f => (a -> b) -> f a ->  f b
(<$>) = map
infixl 4 <$>

newtype Basic1 f a = Basic1
    { getBasic1 :: f a
    }

instance Base.Functor f => CatFunctor HASK HASK (Basic1 f) where
    catMap :: forall a b. (a -> b) -> Basic1 f a -> Basic1 f b
    catMap = coerce (Base.fmap :: (a -> b) -> f a -> f b)

deriving via (Basic1 ((->) r))      instance CatFunctor HASK HASK ((->) r)
deriving via (Basic1 [])            instance CatFunctor HASK HASK []
deriving via (Basic1 ((,) a))       instance CatFunctor HASK HASK ((,) a)
deriving via (Basic1 ((,,) a b))    instance CatFunctor HASK HASK ((,,) a b)
deriving via (Basic1 ((,,,) a b c)) instance CatFunctor HASK HASK ((,,,) a b c)

deriving via (Basic1  Base.Identity)  instance CatFunctor HASK HASK  Base.Identity
deriving via (Basic1  Base.Maybe)     instance CatFunctor HASK HASK  Base.Maybe
deriving via (Basic1  Base.Min)       instance CatFunctor HASK HASK  Base.Min
deriving via (Basic1  Base.Max)       instance CatFunctor HASK HASK  Base.Max
deriving via (Basic1  Base.First)     instance CatFunctor HASK HASK  Base.First
deriving via (Basic1  Base.Last)      instance CatFunctor HASK HASK  Base.Last
deriving via (Basic1  Base.NonEmpty)  instance CatFunctor HASK HASK  Base.NonEmpty
deriving via (Basic1 (Base.Either a)) instance CatFunctor HASK HASK (Base.Either a)
deriving via (Basic1 (Base.Const m))  instance CatFunctor HASK HASK (Base.Const m)

instance Category cat => CatFunctor cat HASK Base.Proxy where catMap _ _ = Base.Proxy
instance CatFunctor (:~:) (:~:) f where catMap Refl = Refl
instance CatFunctor (:~:) HASK  f where catMap Refl = id
