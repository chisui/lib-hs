{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Std.Singleton where

import "base" Data.Proxy ( Proxy(..) )
import "base" Prelude qualified as Base
import "base" Data.Coerce
import "base" GHC.TypeLits

import "ghc-prim" GHC.Prim ( Proxy#, proxy# )

import "this" Std.Partial
import "this" Std.Cat


liftProxy :: Proxy# a -> Proxy a
liftProxy _ = Proxy

unliftProxy :: Proxy a -> Proxy# a
unliftProxy !_ = proxy#

class Singleton (k :: k0) where
    type S k
class Singleton k => Known (t :: k) where
    val :: proxy t -> S k
    val# :: Proxy# t -> S k

val' :: forall t. Known t => S' t
val' = val# (proxy# @t)

class Singleton k => Promote (t :: Totallity) (k :: k0) | k -> t where
    promote :: proxy k -> S k -> Res t (SomeSingleton k)

type S' (t :: k) = S k

instance Singleton () where
    type S () = ()
instance Known '() where
    val _ = ()
    val# _ = ()
instance Promote 'Total () where
    promote _ _ = coerce (SomeSingleton (proxy# @'()))

instance Singleton Symbol where
    type S Symbol = Base.String
instance KnownSymbol s => Known s where
    val = symbolVal
    val# = symbolVal'
instance Promote 'Total Symbol where
    promote _ s = coerce (toSing (someSymbolVal s))
      where toSing (SomeSymbol p) = SomeSingleton (unliftProxy p)

instance Singleton Nat where
    type S Nat = Base.Integer
instance KnownNat n => Known n where
    val = natVal
    val# = natVal' 
instance Promote 'Partial Nat where
    promote _ i = coerce (toSing <$> someNatVal i)
      where toSing (SomeNat n) = SomeSingleton (unliftProxy n)

instance Singleton k => Singleton [k] where
    type S [k] = [S k]
instance (Known a, Known as) => Known (a ': as) where
    val _ = val' @a : val' @as
    val# _ = val' @a : val' @as

instance (Singleton a, Singleton b) => Singleton (a, b) where
    type S (a, b) = (S a, S b)
instance (Known a, Known b) => Known '(a, b) where
    val _ = (val' @a, val' @b)
    val# _ = (val' @a, val' @b)
instance (Promote t0 a, Promote t1 b, ZipRes t0 t1, Min t0 t1 ~ t2) => Promote t2 (a, b) where
    promote _ (a, b) = zipRes merge (promote (Proxy @a) a) (promote (Proxy @b) b)
      where
        merge :: SomeSingleton a -> SomeSingleton b -> SomeSingleton (a, b)
        merge (SomeSingleton (_ :: Proxy# k0)) (SomeSingleton (_ :: Proxy# k1))
            = SomeSingleton (proxy# @'(k0, k1))


instance (Singleton a, Singleton b, Singleton c) => Singleton (a, b, c) where
    type S (a, b, c) = (S a, S b, S c)
instance (Known a, Known b, Known c) => Known '(a, b, c) where
    val _ = (val' @a, val' @b, val' @c)
    val# _ = (val' @a, val' @b, val' @c)
instance (Promote t0 a, Promote t1 b, Promote t2 c, ZipRes t0 t1, ZipRes (t0 `Min` t1) t2, (t0 `Min` t1 `Min` t2) ~ t3) => Promote t3 (a, b, c) where
    promote _ (a, b, c) = zipRes3 merge (promote (Proxy @a) a) (promote (Proxy @b) b) (promote (Proxy @c) c)
      where
        merge :: SomeSingleton a -> SomeSingleton b -> SomeSingleton c -> SomeSingleton (a, b, c)
        merge (SomeSingleton (_ :: Proxy# k0)) (SomeSingleton (_ :: Proxy# k1)) (SomeSingleton (_ :: Proxy# k2))
            = SomeSingleton (proxy# @'(k0, k1, k2))


instance (Singleton a, Singleton b, Singleton c, Singleton d) => Singleton (a, b, c, d) where
    type S (a, b, c, d) = (S a, S b, S c, S d)
instance (Known a, Known b, Known c, Known d) => Known '(a, b, c, d) where
    val _ = (val' @a, val' @b, val' @c, val' @d)
    val# _ = (val' @a, val' @b, val' @c, val' @d)
instance (Promote t0 a, Promote t1 b, Promote t2 c, Promote t3 d, ZipRes t0 t1, ZipRes (t0 `Min` t1) t2, ZipRes (t0 `Min` t1 `Min` t2) t3, (t0 `Min` t1 `Min` t2 `Min` t3) ~ t4) => Promote t4 (a, b, c, d) where
    promote _ (a, b, c, d) = zipRes4 merge (promote (Proxy @a) a) (promote (Proxy @b) b) (promote (Proxy @c) c) (promote (Proxy @d) d)
      where
        merge :: SomeSingleton a -> SomeSingleton b -> SomeSingleton c -> SomeSingleton d -> SomeSingleton (a, b, c, d)
        merge (SomeSingleton (_ :: Proxy# k0)) (SomeSingleton (_ :: Proxy# k1)) (SomeSingleton (_ :: Proxy# k2)) (SomeSingleton (_ :: Proxy# k3))
            = SomeSingleton (proxy# @'(k0, k1, k2, k3))

data SomeSingleton (k :: k0) where
    SomeSingleton :: forall k (t :: k). Known t => !(Proxy# t) -> SomeSingleton k

unwrapSomeSingleton :: SomeSingleton k -> S k
unwrapSomeSingleton (SomeSingleton p) = val# p
