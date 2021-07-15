{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Std.Vec where

import "base" Prelude ( Integer )
import "base" Unsafe.Coerce

import "this" Std.HList
import "this" Std.Cat.Foldable
import "this" Std.Type
import "this" Std.Singleton
import "this" Std.Debug
import "this" Std.BinOp
import "this" Std.Group
import "this" Std.Ord
import "this" Std.Literal
import "this" Std.Cat


data Vec n a where
    VNil :: Vec 0 a
    VCons :: a -> Vec (n - 1) a -> Vec n a

instance CatIsomorphic HASK (Vec 0 a) (HList '[]) where
    catIso = const HNil :<-> const VNil
instance CatIsomorphic HASK (Vec (n - 1) a) (HList as) => CatIsomorphic HASK (Vec n a) (HList (a ': as)) where
    catIso = t' :<-> f'
      where
        t' (VCons a as) = a ::: t'' as
        t' _ = error "can't happen"
        f' (a ::: as) = VCons a (f'' as)
        (t'' :<-> f'') = catIso

instance CatFunctor HASK HASK (Vec n) where
    catMap _ VNil = VNil
    catMap f (VCons a as) = VCons (f a) (catMap f as)

instance Known n => CatPure HASK (Vec n) where
    catPure :: forall a. a -> Vec n a
    catPure a = v (val' @n)
      where
        v :: Integer -> Vec m a
        v 0 = unsafeCoerce VNil
        v n = unsafeCoerce (VCons a (v (n - 1)))

instance CatAp HASK (Vec n) where
    VNil         <**> VNil         = VNil
    (VCons f fs) <**> (VCons a as) = VCons (f a) (fs <*> as)
    _            <**> _            = error "can't happen"

instance CatLift2 HASK (Vec n) where
    lift2 _ VNil VNil = VNil
    lift2 f (VCons a as) (VCons b bs) = VCons (f a b) (lift2 f as bs)
    lift2 _ _ _ = error "can't happen"

instance Pure (Vec n) => CatApplicative HASK (Vec n)

instance Foldable (Vec n) where
    foldMap _ VNil = mempty
    foldMap f (VCons a as) = f a ++ (foldMap f as)

instance (Known n, Magma op a) => BinOp op (Vec n a) where
    op# p = lift2 (op# p)
instance (Known n, Magma op a, IdentityOp op a) => IdentityOp op (Vec n a) where
    identity# p = identity# p
instance (Known n, AssociativeOp op a) => AssociativeOp op (Vec n a)
instance (Known n, Idempotent op a) => Idempotent op (Vec n a)
instance (Known n, Commutative op a) => Commutative op (Vec n a)
instance (Known n, Upper op a) => Upper op (Vec n a) where
    top# p = pure (top# p)
instance (Known n, Lower op a) => Lower op (Vec n a) where
    bot# p = pure (bot# p)
