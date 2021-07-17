module Std.Cat.Yoneda where

import "this" Std.Cat
import "this" Std.Cat.Kleisli
import "this" Std.Cat.Phantom


class Yoneda cat where
    yoneda :: forall a f. EndoFunctor cat f => Iso cat (NT cat (cat a) f) (f a)

instance Yoneda HASK where
    yoneda = (\phi -> eta phi id) 
        :<-> (\f -> NT (<$> f))

instance Monad m => Yoneda (Kleisli m) where
    yoneda = Kleisli (\(NT phi) -> unKleisli phi id)
        :<-> arr (\f -> NT (Kleisli (\phi -> unKleisli (mapEndo phi) f)))

instance Yoneda Skeleton where
    yoneda :: Iso Skeleton (NT Skeleton (Skeleton a) f) (f a)
    yoneda = Phantom :<-> Phantom
