module Std.Cat.Compose where

import "base" GHC.Generics ( Generic )
import "base" Data.Kind

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Applicative
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.NaturalTransformation
import "this" Std.Cat.Iso


newtype (.) (f :: k0 -> Type) (g :: k -> k0) (a :: k) = Compose
    { runCompose :: f (g a)
    }
  deriving Generic

instance CatIsomorphic HASK ((f . g) a) (f (g a)) where
    catIso = runCompose :<-> Compose

liftCompose :: (f (g a) -> f' (g' a')) -> (f . g) a -> (f' . g') a'
liftCompose f = Compose . f . runCompose

instance (Functor f, Functor g) => CatFunctor HASK HASK (f . g) where
    catMap :: forall a b. (a -> b) -> (f . g) a -> (f . g) b
    catMap = to coerce (mapEndo . mapEndo :: (a -> b) -> f (g a) -> f (g b))

instance (Pure f, Pure g) => CatPure HASK (f . g) where
    catPure :: forall a. a -> (f . g) a
    catPure = to coerce (catPure . catPure :: a -> f (g a))

instance (Lift2 f, Ap g) => CatAp HASK (f . g) where
    (<**>) :: forall a b. (f . g) (a -> b) -> (f . g) a -> (f . g) b
    (<**>) = to coerce (lift2 (<**>) :: f (g (a -> b))-> f (g a) -> f (g b))

instance (Applicative f, Applicative g) => CatLift2 HASK (f . g) where
    lift2 f a b = f <$> a <*> b

instance (Applicative f, Applicative g) => CatApplicative HASK (f . g)


instance CatLeftFunctor'  Functor Functor (~>) (~>) (.) where left'  f = NT (liftCompose         (eta f))
instance CatRightFunctor' Functor Functor (~>) (~>) (.) where right' f = NT (liftCompose (catMap (eta f)))
instance CatBifunctor'    Functor Functor (~>) (~>) (~>) (.)
