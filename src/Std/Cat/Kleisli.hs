{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Kleisli where

import "base" Data.Coerce

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Applicative
import "this" Std.Cat.Monad
import "this" Std.Cat.MonadFail
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Closed
import "this" Std.Cat.Distributive
import "this" Std.Cat.Associative
import "this" Std.Constraint


newtype CatKleisli cat m a b = Kleisli
    { kleisli :: a `cat` m b
    }
type Kleisli = CatKleisli HASK


instance (CatMonad cat m, Semigroupoid cat) => Semigroupoid (CatKleisli cat m) where
    (.) :: forall a b c. CatKleisli cat m b c -> CatKleisli cat m a b -> CatKleisli cat m a c
    (.) = coerce ((<=<) :: b `cat` m c -> a `cat` m b -> a `cat` m c)
instance (CatPure cat m, CatId cat) => CatId (CatKleisli cat m) where
    id :: forall a. CatKleisli cat m a a
    id = coerce (catPure :: a `cat` m a)
instance (CatMonad cat m, Category cat) => Category (CatKleisli cat m)


instance ( CatMonad cat m
         , Cartesian cat
         , EndoBifunctor Unconstraint (CatKleisli cat m) (Product cat)
         ) => Cartesian (CatKleisli cat m) where
    type Product (CatKleisli cat m) = Product cat
    fst = Kleisli (catPure . fst)
    snd = Kleisli (catPure . snd)
    copy = Kleisli (catPure . copy)

instance ( CatMonad cat m
         , Cocartesian cat
         , EndoBifunctor Unconstraint (CatKleisli cat m) (Coproduct cat)
         ) => Cocartesian (CatKleisli cat m) where
    type Coproduct (CatKleisli cat m) = Coproduct cat
    lft = Kleisli (catPure . lft)
    rght = Kleisli (catPure . rght)
    fuse = Kleisli (catPure . fuse)

instance ( Monad m
         , Distributive m
         ) => Closed (Kleisli m) where
    type Exp (Kleisli m) = Kleisli m
    apply :: Kleisli m (Kleisli m a b, a) b
    apply = Kleisli (uncurry kleisli)
    curry (Kleisli f) = Kleisli (catPure . Kleisli . curry f)
    uncurry (Kleisli f) = Kleisli $ \(a, b) -> app ( (,b) <$> f a)  --(uncurry ((. flip kleisli) . (>>=) . f))
      where
        app :: m (Kleisli m a b, a) -> m b
        app m = do 
            (Kleisli g, b) <- m
            g b


instance ( CatAssociative HASK f
         , EndoLeftFunctor Unconstraint HASK f
         , CatDistributive HASK m
         , CatMonad HASK m
         , EndoFunctor HASK m
         ) => CatLeftFunctor Unconstraint (CatKleisli HASK m) (CatKleisli HASK m) f where
    left (Kleisli f) = Kleisli (map unLeft . distribute . map f . MkLeft)
instance ( CatAssociative HASK f
         , EndoRightFunctor Unconstraint HASK f
         , CatDistributive HASK m
         , CatMonad HASK m
         ) => CatRightFunctor Unconstraint (CatKleisli HASK m) (CatKleisli HASK m) f where
    right (Kleisli f) = Kleisli (map unRight . distribute . map f . MkRight)
instance ( CatAssociative HASK f
         , CatDistributive HASK m
         , EndoLeftFunctor Unconstraint HASK f
         , EndoRightFunctor Unconstraint HASK f
         , EndoBifunctor Unconstraint HASK f
         , CatMonad HASK m
         ) => CatBifunctor Unconstraint (CatKleisli HASK m) (CatKleisli HASK m) (CatKleisli HASK m) f