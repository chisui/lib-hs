module Std.Cat.Functor
    ( CatFunctor'(..), EndoFunctor', Functor'
    , CatFunctor, EndoFunctor, Functor
    , mapEndo, map, (<$>), Basic1(..)
    ) where

import "base" Prelude qualified as Base
import "base" Data.List.NonEmpty qualified as Base
import "base" Data.Proxy qualified as Base
import "base" Data.Coerce
import "base" Data.Functor.Identity qualified as Base
import "base" Data.Functor.Const qualified as Base
import "base" Data.Semigroup qualified as Base ( Min(..), Max(..) )

import "this" Std.Type
import "this" Std.Cat.Class


{- | A Functor is a mapping between categories. Objects are mapped using typeconstructor @f@,
morphisms are mapped using @catMap@. Every functor has to preserve the structure of the
underlying categories:

[Identity]    @'catMap' 'id' == 'id'@
[Composition] @'catMap' (f . g) == 'catMap' f . 'catMap' g@

Functors that map a Category onto itself are Endofunctors.
-}
class (Category cat0, Category cat1) => CatFunctor' (c :: k0 -> Constraint) (cat0 :: k0 -> k0 -> Type) (cat1 :: k1 -> k1 -> Type) (f :: k0 -> k1) | f cat0 -> c where
    catMap, (<$$>) :: (c a, c b) => a `cat0` b -> f a `cat1` f b
    catMap = (<$$>)
    (<$$>) = catMap
    {-# MINIMAL catMap | (<$$>) #-}
infixl 4 <$$>

type EndoFunctor' c cat = CatFunctor' c cat cat
type Functor' c = EndoFunctor' c HASK

type CatFunctor = CatFunctor' Unconstrained
type EndoFunctor c = CatFunctor c c
type Functor = EndoFunctor HASK

mapEndo :: (EndoFunctor' c cat f, c a, c b) =>  a `cat` b -> f a `cat` f b
mapEndo = catMap

map :: (Functor' c f, c a, c b) => (a -> b) -> f a -> f b
map = catMap

(<$>) :: (Functor' c f, c a, c b) => (a -> b) -> f a ->  f b
(<$>) = map
infixl 4 <$>

newtype Basic1 f a = Basic1
    { getBasic1 :: f a
    }

instance Base.Functor f => CatFunctor' Unconstrained HASK HASK (Basic1 f) where
    catMap :: forall a b. (a -> b) -> Basic1 f a -> Basic1 f b
    catMap = coerce (Base.fmap :: (a -> b) -> f a -> f b)

deriving via (Basic1 ((->) r))      instance CatFunctor' Unconstrained HASK HASK ((->) r)
deriving via (Basic1 [])            instance CatFunctor' Unconstrained HASK HASK []
deriving via (Basic1 ((,) a))       instance CatFunctor' Unconstrained HASK HASK ((,) a)
deriving via (Basic1 ((,,) a b))    instance CatFunctor' Unconstrained HASK HASK ((,,) a b)
deriving via (Basic1 ((,,,) a b c)) instance CatFunctor' Unconstrained HASK HASK ((,,,) a b c)

deriving via (Basic1  Base.Identity)  instance CatFunctor' Unconstrained HASK HASK  Base.Identity
deriving via (Basic1  Base.Min)       instance CatFunctor' Unconstrained HASK HASK  Base.Min
deriving via (Basic1  Base.Max)       instance CatFunctor' Unconstrained HASK HASK  Base.Max
deriving via (Basic1  Base.NonEmpty)  instance CatFunctor' Unconstrained HASK HASK  Base.NonEmpty
deriving via (Basic1 (Base.Either a)) instance CatFunctor' Unconstrained HASK HASK (Base.Either a)
deriving via (Basic1 (Base.Const m))  instance CatFunctor' Unconstrained HASK HASK (Base.Const m)

instance Category cat => CatFunctor' Unconstrained cat HASK Base.Proxy where catMap _ _ = Base.Proxy
instance CatFunctor' Unconstrained (:~:) (:~:) f where catMap Refl = Refl
instance CatFunctor' Unconstrained (:~:) HASK  f where catMap Refl = id
