{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Std.Vec where

import "base" Prelude ( Integer )
import "base" Prelude qualified as Base
import "base" Unsafe.Coerce

import "this" Std.HList
import "this" Std.Bool
import "this" Std.Type
import "this" Std.Singleton
import "this" Std.Debug
import "this" Std.BinOp
import "this" Std.Group
import "this" Std.Ord
import "this" Std.Literal
import "this" Std.Cat
import "this" Std.Cat.Foldable
import "this" Std.Cat.Traversable


data Vec n a where
    VNil  :: Vec 0 a
    VCons :: a -> Vec (n - 1) a -> Vec n a

instance HasItems (Vec n a) where type Item (Vec n a) = a
instance ToList 'Total (Vec n a) where
    toList VNil = []
    toList (VCons a as) = a : toList as
instance Known n => FromList (Vec n a) where
    fromList l
        | (Base.length l)  /= fromInteger (val' @n) = error $ "expected exactly " ++ show (val' @n) ++ " elements but got " ++ show (Base.length l)
        | otherwise = unsafeFromList l
      where
        unsafeFromList :: [a] -> Vec m a
        unsafeFromList []     = unsafeCoerce VNil
        unsafeFromList (a:as) = unsafeCoerce (VCons a (unsafeFromList as))

instance (Known n, FromInteger a) => FromInteger (Vec n a) where
    fromInteger = pure . fromInteger

instance CatIsomorphic HASK (Vec 0 a) (HList '[]) where
    catIso = const HNil :<-> const VNil
instance CatIsomorphic HASK (Vec (n - 1) a) (HList as) => CatIsomorphic HASK (Vec n a) (HList (a ': as)) where
    catIso = t' :<-> f'
      where
        t' (VCons a as) = a ::: t'' as
        t' _ = error "can't happen"
        f' (a ::: as) = VCons a (f'' as)
        (t'' :<-> f'') = catIso

instance CatFunctor' Unconstrained HASK HASK (Vec n) where
    catMap _ VNil = VNil
    catMap f (VCons a as) = VCons (f a) (catMap f as)

instance Known n => CatPure' Unconstrained HASK (Vec n) where
    catPure :: forall a. a -> Vec n a
    catPure a = v (val' @n)
      where
        v :: Integer -> Vec m a
        v 0 = unsafeCoerce VNil
        v n = unsafeCoerce (VCons a (v (n - 1)))

instance CatAp' Unconstrained HASK (Vec n) where
    VNil         <**> VNil         = VNil
    (VCons f fs) <**> (VCons a as) = VCons (f a) (fs <*> as)
    _            <**> _            = error "can't happen"

instance CatLift2' Unconstrained HASK (Vec n) where
    lift2 _ VNil VNil = VNil
    lift2 f (VCons a as) (VCons b bs) = VCons (f a b) (lift2 f as bs)
    lift2 _ _ _ = error "can't happen"

instance Pure (Vec n) => CatApplicative' Unconstrained HASK (Vec n)

instance Foldable' Unconstrained (Vec n) where
    foldMap _ VNil = mempty
    foldMap f (VCons a as) = f a ++ (foldMap f as)

instance Known n => Traversable (Vec n) where
    sequence = map fromList . sequence . toList

unfoldV :: forall n a b. Known n => (a -> (a, b)) -> a -> Vec n b
unfoldV f = v (val' @n)
  where
    v :: Integer -> a -> Vec m b
    v 0 _ = unsafeCoerce VNil
    v n a = let (a', b) = f a
        in unsafeCoerce (VCons b (v (n - 1) a'))

iterateV :: forall n a. Known n => (a -> a) -> a -> Vec n a
iterateV f = unfoldV (diagonal . f)

iteratedV :: forall i n. (Known n, Iterable i) => Vec n i
iteratedV = iterateV succ zero

dot :: forall a n. (Known n, Field a) => Vec n a -> Vec n a -> a
dot a b = sum (a * b)

instance (Known n, Magma       op a) => BinOp         op (Vec n a) where op# p = lift2 (op# p)
instance (Known n, UnitalMagma op a) => IdentityOp    op (Vec n a) where identity# p = identity# p
instance (Known n, Quasigroup  op a) => InverseOp     op (Vec n a) where
    type InvOp op (Vec n a) = InvOp op a
    inv# p = map (inv# p)
instance (Known n, AssociativeOp op a) => AssociativeOp op (Vec n a)
instance (Known n, Idempotent    op a) => Idempotent    op (Vec n a)
instance (Known n, CommutativeOp op a) => CommutativeOp op (Vec n a)
instance (Known n, Upper         op a) => Upper         op (Vec n a) where top# p = pure (top# p)
instance (Known n, Lower         op a) => Lower         op (Vec n a) where bot# p = pure (bot# p)
