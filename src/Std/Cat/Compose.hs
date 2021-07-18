{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Compose where

import "base" GHC.Generics ( Generic )

import "this" Std.Type
import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Closed
import "this" Std.Cat.Applicative
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.Distributive
import "this" Std.Cat.Representable
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

instance (Functor f, Functor' c g) => CatFunctor' c HASK HASK (f . g) where
    catMap :: forall a b. (c a, c b) => (a -> b) -> (f . g) a -> (f . g) b
    catMap = to coerce (mapEndo . mapEndo :: (a -> b) -> f (g a) -> f (g b))

instance (Pure f, Pure' c g) => CatPure' c HASK (f . g) where
    catPure :: forall a. c a => a -> (f . g) a
    catPure = to coerce (catPure . catPure :: a -> f (g a))

instance (Lift2 f, Ap' c g) => CatAp' c HASK (f . g) where
    (<**>) :: forall a b. (c a, c b) => (f . g) (a -> b) -> (f . g) a -> (f . g) b
    (<**>) = to coerce (lift2 (<**>) :: f (g (a -> b))-> f (g a) -> f (g b))

instance (Applicative f, Applicative g) => CatLift2' Unconstrained HASK (f . g) where
    lift2 f a b = f <$> a <*> b

instance (Applicative f, Applicative g) => CatApplicative' Unconstrained HASK (f . g)


instance (Distributive f, Distributive g) => CatDistributive HASK (f . g) where
    distribute = Compose . map distribute . collect runCompose
    collect  f = Compose . map distribute . collect (to coerce f)

instance (Representable f, Representable g) => CatRepresentable HASK HASK (f . g) where
    type CatRep HASK HASK (f . g) = (CatRep HASK HASK f, CatRep HASK HASK g)
    catRep = tabulate :<-> index
      where
        index (Compose fg) (i,j) = from catRep (from catRep fg i) j
        tabulate = Compose . to catRep . map (to catRep) . curry

instance CatLeftFunctor'  Functor Functor (~>) (~>) (.) where left'  f = NT (liftCompose         (eta f))
instance CatRightFunctor' Functor Functor (~>) (~>) (.) where right' f = NT (liftCompose (catMap (eta f)))
instance CatBifunctor'    Functor Functor (~>) (~>) (~>) (.)




class    CatComposition cat  o   where comp :: Iso cat ((f `o` g) x) (f (g x))
instance CatComposition HASK (.) where comp = coerce
type Composition = CatComposition HASK
