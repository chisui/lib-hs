module Std.Compose
    ( type (.)(..)
    ) where

import "base" Data.Kind
import "base" Data.Coerce ( coerce )
import "this" Std.Cat


newtype (.) (f :: k0 -> Type) (g :: k -> k0) (a :: k) = Compose
    { runCompose :: f (g a)
    }

instance (Functor f, Functor g) => CatFunctor HASK HASK (f . g) where
    map :: forall a b. (a -> b) -> (f . g) a -> (f . g) b
    map a = coerce (mapEndo (mapEndo a) :: f (g a) -> f (g b))

instance (Pure f, Pure g) => CatPure HASK (f . g) where
    catPure :: forall a. a -> (f . g) a
    catPure = coerce ((catPure . catPure) :: a -> f (g a))

instance (Lift2 f, Ap g) => CatAp HASK (f . g) where
    Compose f <*> Compose x = Compose (lift2 (<*>) f x)

instance (Applicative f, Applicative g) => CatLift2 HASK (f . g) where
    lift2 f a b = f <$> a <*> b

instance (Applicative f, Applicative g) => CatApplicative HASK (f . g)
