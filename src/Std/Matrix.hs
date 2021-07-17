{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MagicHash #-}
module Std.Matrix where

import "base" Prelude ( Int )
import "base" Data.List qualified as List

import "this" Std.Ord
import "this" Std.BinOp
import "this" Std.Type
import "this" Std.Bool
import "this" Std.Group
import "this" Std.Literal
import "this" Std.Vec
import "this" Std.Cat
import "this" Std.Cat.Endo
import "this" Std.Cat.Traversable
import "this" Std.Singleton


newtype Matrix a m n = Matrix
    { getRowsM :: (Vec m . Vec n) a
    }
type Square a = EndoMorph (Matrix a)

mkScale :: forall m n a. (Known m, Known n) => a -> a -> Matrix a m n
mkScale z a = Matrix . Compose . fromList . map fromList . mkRows $ 0
  where
    mkRows i
        | i == (val' @m) = []
        | otherwise      = mkRow i 0 : mkRows (i + 1)
    mkRow i j
        | j == (val' @n) = []
        | j == i         = a : mkRow i (j + 1)
        | otherwise      = z : mkRow i (j + 1)

mkDiagonal :: forall m n a. (Known m, Known n) => a -> Vec (MinT m n) a -> Matrix a m n
mkDiagonal z d = Matrix . Compose $ (\i -> mkCell i <$> iteratedV) <$> iteratedV
  where
    mkCell :: Int -> Int -> a
    mkCell i j
        | i == j = toList d List.!! i
        | otherwise = z

instance (FromInteger a, Field a, Known m) => FromInteger (Matrix a m m) where fromInteger = mkScale zero . fromInteger

instance Known n => CatIsomorphic HASK (Vec n a) (Matrix a 1 n) where
    catIso = Matrix . Compose . pure :<-> (\(Matrix (Compose (VCons v _))) -> v)

instance Known n => CatIsomorphic HASK (Vec n a) (Matrix a n 1) where
    catIso = isoThrough @(Matrix a 1 n)

instance (Known m, Known n) => CatIsomorphic HASK (Matrix a m n) (Matrix a n m) where
    catIso = transposeM :<-> transposeM


transposeM :: forall a m n. (Known m, Known n) => Matrix a m n -> Matrix a n m
transposeM = to coerce (sequence :: Vec m (Vec n a) -> Vec n (Vec m a))

scaleM :: forall a m n. Magma 'Mult a => a -> Matrix a m n -> Matrix a m n
scaleM a = to coerce (map (* a) :: (Vec m . Vec n) a -> (Vec m . Vec n) a)

instance (Field a, Known m, Known n) => BinOp 'Add (Matrix a m n) where
    op# p m n = getME $ lift2 (op# p) (ME m) (ME n)

instance (Field a, Known m, Known n) => IdentityOp 'Add (Matrix a m n) where
    identity# p = mkScale zero (identity# p)

instance Field a => Semigroupoid' Known (Matrix a) where
    (.) :: forall m n l. (Known m, Known n, Known l) => Matrix a m l -> Matrix a n m -> Matrix a n l
    (.) = to coerce ((\ a b -> (<$> sequence a) . dot <$> b) :: Vec m (Vec l a) -> Vec n (Vec m a) -> Vec n (Vec l a))

instance Field a => CatId' Known (Matrix a) where
    id = mkScale zero one

instance Field a => Category' Known (Matrix a)


newtype MatrixElems m n a = ME
    { getME :: Matrix a m n
    }

instance CatFunctor' Unconstrained HASK HASK (MatrixElems m n) where
    catMap :: forall a b. (a -> b) -> MatrixElems m n a -> MatrixElems m n b
    catMap = to coerce (map :: (a -> b) -> (Vec m . Vec n) a -> (Vec m . Vec n) b)

instance (Known n, Known m) => CatPure' Unconstrained HASK (MatrixElems m n) where
    catPure = ME . Matrix . pure

instance (Known n, Known m) => CatAp' Unconstrained HASK (MatrixElems m n) where
    (<**>) = lift2 id

instance (Known n, Known m) => CatLift2' Unconstrained HASK (MatrixElems m n) where
    lift2 :: forall a b c. (a -> b -> c) -> MatrixElems m n a -> MatrixElems m n b -> MatrixElems m n c
    lift2 f = to coerce (lift2 (lift2 f) :: Vec m (Vec n a) -> Vec m (Vec n b) -> Vec m (Vec n c))
