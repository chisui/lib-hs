{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MagicHash #-}
module Std.Matrix where

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


data Matrix a m n where
    MkScale  :: a -> Matrix a m n
    MkMatrix :: (Known m, Known n) => Vec m (Vec n a) -> Matrix a m n

pattern Matrix :: (Known m, Known n, Field a) => Vec m (Vec n a) -> Matrix a m n
pattern Matrix m <- (toRows -> m)
  where Matrix m = MkMatrix m
{-# COMPLETE Matrix #-}

toRows :: forall m n a. (Known m, Known n, Field a) => Matrix a m n -> Vec m (Vec n a)
toRows (MkMatrix m) = m
toRows (MkScale  a) = fromList . map fromList . mkRows $ 0
  where
    mkRows i
        | i == (val' @m) = []
        | otherwise      = mkRow i 0 : mkRows (i + 1)
    mkRow i j
        | j == (val' @n) = []
        | j == i         = a    : mkRow i (j + 1)
        | otherwise      = zero : mkRow i (j + 1)


type Square a = EndoMorph (Matrix a)

instance FromInteger a => FromInteger (Matrix a m m) where fromInteger = MkScale . fromInteger

instance Known n => CatIsomorphic HASK (Vec n a) (Matrix a 1 n) where
    catIso = MkMatrix . pure :<-> f
      where
        f :: Matrix a 1 n -> Vec n a
        f (MkMatrix (VCons v _)) = v
        f (MkScale a)            = pure a

instance Known n => CatIsomorphic HASK (Vec n a) (Matrix a n 1) where
    catIso = isoThrough @(Matrix a 1 n)

instance (Known m, Known n) => CatIsomorphic HASK (Matrix a m n) (Matrix a n m) where
    catIso = transposeM :<-> transposeM


transposeM :: Matrix a m n -> Matrix a n m
transposeM (MkScale a)  = MkScale a
transposeM (MkMatrix v) = MkMatrix (sequence v)

scaleM :: Magma 'Mult a => a -> Matrix a m n -> Matrix a m n
scaleM a (MkScale b) = MkScale (a * b)
scaleM a (MkMatrix m) = MkMatrix (map (* a) <$> m)

instance Magma 'Add a => BinOp 'Add (Matrix a m n) where
    op# p (MkScale  a) (MkScale  b) = MkScale (op# p a b)
    op# p (MkMatrix m) (MkScale  a) = MkMatrix (map (flip (op# p) a) <$> m)
    op# p (MkScale  a) (MkMatrix m) = MkMatrix (map (op# p a) <$> m)
    op# p (MkMatrix m) (MkMatrix n) = MkMatrix (lift2 (lift2 (op# p)) m n)

instance UnitalMagma 'Add a => IdentityOp 'Add (Matrix a m m) where
    identity# p = MkScale (identity# p)

instance Field a => Semigroupoid' Known (Matrix a) where
    Matrix a . Matrix b = Matrix ((<$> sequence a) . dot <$> b)

instance Field a => CatId' Known (Matrix a) where
    id = MkScale one

instance Field a => Category' Known (Matrix a)


newtype MatrixElems m n a = ME (Matrix a m n)

instance CatFunctor' Unconstrained HASK HASK (MatrixElems m n) where
    catMap f (ME (MkScale  a)) = ME (MkScale (f a))
    catMap f (ME (MkMatrix m)) = ME (MkMatrix (map f <$> m))

instance (Known n, Known m) => CatPure' Unconstrained HASK (MatrixElems m n) where
    catPure = ME . MkScale
    
