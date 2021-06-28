{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Basic
    ( Basic(..), Basic1(..)
    , Unsafe(..), Unsafe1(..)
    , Monoidal(..)
    , Numeric(..), PartialNumeric(..)
    , errorToPartial0
    , errorToPartial1
    , errorToPartial2
    , errorToPartial3
    , Coercible, coerce
    ) where

import "base" Data.Either ( Either(..) )
import "base" Data.Maybe ( Maybe(..) )
import "base" GHC.IO.Unsafe ( unsafePerformIO )
import "base" Data.Coerce ( coerce, Coercible )
import "base" Control.Exception ( SomeException, try, evaluate )

import "base" Prelude qualified as Base
import "base" Control.Applicative qualified as Base
import "base" Control.Monad qualified as Base

import "this" Std.Partial
import "this" Std.Cat


newtype Basic a = Basic a
newtype Basic1 f a = Basic1 (f a)
newtype Unsafe a = Unsafe a
newtype Unsafe1 a = Unsafe1 a
newtype Monoidal a = Monoidal a
newtype Numeric a = Numeric a
newtype PartialNumeric a = PartialNumeric a

errorToPartial0 :: forall a. a -> Res 'Partial a
errorToPartial0 x = coerce (toRes (unsafePerformIO (try (evaluate x))))
  where
    toRes :: Either SomeException a -> Maybe a
    toRes (Left _)  = Nothing
    toRes (Right r) = Just r

errorToPartial1 :: forall f a b. Coercible f (a -> b) => f -> a -> Res 'Partial b
errorToPartial1 f a = errorToPartial0 ((coerce f :: a -> b) a)

errorToPartial2 :: forall f a b c. Coercible f (a -> b -> c) => f -> a -> b -> Res 'Partial c
errorToPartial2 f a b = errorToPartial0 ((coerce f :: a -> b -> c) a b)

errorToPartial3 :: forall f a b c d. Coercible f (a -> b -> c -> d) => f -> a -> b -> c -> Res 'Partial d
errorToPartial3 f a b c = errorToPartial0 ((coerce f :: a -> b -> c -> d) a b c)

-- instances

instance Base.Functor f => CatFunctor HASK HASK (Basic1 f) where
    map :: forall a b. (a -> b) -> Basic1 f a -> Basic1 f b
    map = coerce (Base.fmap :: (a -> b) -> f a -> f b)

instance Base.Applicative f => CatPure HASK (Basic1 f) where
    pure :: forall a. a -> Basic1 f a
    pure = coerce (Base.pure :: a -> f a)
instance Base.Applicative f => CatAp HASK (Basic1 f) where
    (<*>) :: forall a b. Basic1 f (a -> b) -> Basic1 f a -> Basic1 f b
    (<*>) = coerce ((Base.<*>) :: f (a -> b) -> f a -> f b)
instance Base.Applicative f => CatLift2 HASK (Basic1 f) where
    lift2 :: forall a b c. (a -> b -> c) -> Basic1 f a -> Basic1 f b -> Basic1 f c
    lift2 = coerce (Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c)
instance Base.Applicative f => CatApplicative HASK (Basic1 f)

instance Base.Monad f => CatBind HASK (Basic1 f) where
    (=<<) :: forall a b. (a -> Basic1 f b) -> Basic1 f a -> Basic1 f b
    (=<<) = coerce ((Base.=<<) :: (a -> f b) -> f a -> f b)
instance Base.Monad f => CatJoin HASK (Basic1 f) where
    join :: forall a. Basic1 f (Basic1 f a) -> Basic1 f a
    join = coerce (Base.join :: f (f a) -> f a) . map (coerce :: Basic1 f a -> f a)
instance Base.Monad f => CatMonad HASK (Basic1 f)


deriving via (Basic1 []) instance CatFunctor HASK HASK []
deriving via (Basic1 []) instance CatPure HASK []
deriving via (Basic1 []) instance CatLift2 HASK []
deriving via (Basic1 []) instance CatAp HASK []
deriving via (Basic1 []) instance CatApplicative HASK []
deriving via (Basic1 []) instance CatBind HASK []
deriving via (Basic1 []) instance CatJoin HASK []
deriving via (Basic1 []) instance CatMonad HASK []

deriving via (Basic1 (Either e)) instance CatFunctor HASK HASK (Either e)
deriving via (Basic1 (Either e)) instance CatPure HASK (Either e)
deriving via (Basic1 (Either e)) instance CatLift2 HASK (Either e)
deriving via (Basic1 (Either e)) instance CatAp HASK (Either e)
deriving via (Basic1 (Either e)) instance CatApplicative HASK (Either e)
deriving via (Basic1 (Either e)) instance CatBind HASK (Either e)
deriving via (Basic1 (Either e)) instance CatJoin HASK (Either e)
deriving via (Basic1 (Either e)) instance CatMonad HASK (Either e)

deriving via (Basic1 Base.IO) instance CatFunctor HASK HASK Base.IO
deriving via (Basic1 Base.IO) instance CatPure HASK Base.IO
deriving via (Basic1 Base.IO) instance CatLift2 HASK Base.IO
deriving via (Basic1 Base.IO) instance CatAp HASK Base.IO
deriving via (Basic1 Base.IO) instance CatApplicative HASK Base.IO
deriving via (Basic1 Base.IO) instance CatBind HASK Base.IO
deriving via (Basic1 Base.IO) instance CatJoin HASK Base.IO
deriving via (Basic1 Base.IO) instance CatMonad HASK Base.IO
