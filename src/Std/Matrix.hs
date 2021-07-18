{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MagicHash #-}
module Std.Matrix where

import "base" Prelude ( Int )
import "base" Data.List qualified as List

import "this" Std.Ord
import "this" Std.IfThenElse
import "this" Std.BinOp
import "this" Std.Type
import "this" Std.Group
import "this" Std.Literal
import "this" Std.Vec
import "this" Std.Cat
import "this" Std.Cat.Endo
import "this" Std.Cat.Traversable
import "this" Std.Singleton


type m >< n = Vec m . Vec n
newtype Matrix a m n = Matrix
    { getRowsM :: (m >< n) a
    }
type Square a = EndoMorph (Matrix a)

mkDiagonal :: (Known m, Known n) => (Int -> a) -> a -> (m >< n) a
mkDiagonal f a = Compose $ (<$> iteratedV) . mkCell <$> iteratedV
  where mkCell i j = if i == j then f i else a

toDiagonalMatrix :: (Known m, Known n) => Vec (MinT m n) a -> a -> (m >< n) a
toDiagonalMatrix d = mkDiagonal (toList d List.!!)

toScaleMatrix :: (Known m, Known n) => a -> a -> (m >< n) a
toScaleMatrix z a = mkDiagonal (const a) z

instance (FromInteger a, Field a, Known m) => FromInteger (Matrix a m m) where
    fromInteger = Matrix . toScaleMatrix zero . fromInteger

instance Known n => CatIsomorphic HASK (Vec n a) (Matrix a 1 n) where
    catIso = Matrix . Compose . pure :<-> (\(Matrix (Compose (VCons v _))) -> v)

instance Known n => CatIsomorphic HASK (Vec n a) (Matrix a n 1) where
    catIso = isoThrough @(Matrix a 1 n)

instance (Known m, Known n) => CatIsomorphic HASK (Matrix a m n) (Matrix a n m) where
    catIso = transposeM :<-> transposeM


transposeM :: forall a m n. (Known m, Known n) => Matrix a m n -> Matrix a n m
transposeM = to coerce (sequence :: Vec m (Vec n a) -> Vec n (Vec m a))

scaleM :: forall a m n. Magma 'Mult a => a -> Matrix a m n -> Matrix a m n
scaleM a = to coerce (map (* a) :: (m >< n) a -> (m >< n) a)

instance (Field a, Known m, Known n) => BinOp 'Add (Matrix a m n) where
    op# p = to coerce (lift2 (op# p) :: (m >< n) a -> (m >< n) a -> (m >< n) a)

instance (Field a, Known m, Known n) => IdentityOp 'Add (Matrix a m n) where
    identity# _ = Matrix $ toScaleMatrix zero zero

instance Field a => Semigroupoid' Known (Matrix a) where
    (.) :: forall m n l. (Known m, Known n, Known l) => Matrix a m l -> Matrix a n m -> Matrix a n l
    (.) = to coerce ((\ a b -> (<$> sequence a) . dot <$> b) :: Vec m (Vec l a) -> Vec n (Vec m a) -> Vec n (Vec l a))

instance Field a => CatId' Known (Matrix a) where
    id = Matrix $ toScaleMatrix zero one

instance Field a => Category' Known (Matrix a)
