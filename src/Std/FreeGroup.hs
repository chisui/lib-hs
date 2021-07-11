{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.FreeGroup where

import "base" Data.List ( reverse, foldr )
import "base" Text.Show ( Show )
import "base" GHC.Int ( Int )
import "base" Data.Char ( Char )

import "ghc-prim" GHC.Prim ( proxy# )

import "this" Std.Literal
import "this" Std.BinOp
import "this" Std.Group
import "this" Std.Ord
import "this" Std.Cat


newtype FreeGroup a = FreeGroup
    { unFreeGroup :: [(a, Int)]
    }
  deriving (Show, Eq)
instance FromString (FreeGroup Char) where
    fromString = fromList
instance HasItems (FreeGroup a) where
    type Item (FreeGroup a) = a
instance Eq a => FromList (FreeGroup a) where
    fromList = normalizeFreeGroup . FreeGroup . map (,1)

instance Eq a => BinOp 'Add (FreeGroup a) where
    op# _ (FreeGroup a0) (FreeGroup b0) = FreeGroup (op' (reverse a0) b0)
      where
        op' ((a, na) : as) ((b, nb) : bs) 
            | a == b = op' ((a, na + nb) : as) bs
        op' ((_, 0) : as) bs = op' as bs
        op' as ((_, 0) : bs) = op' as bs
        op' v w = reverse v <|> w
instance Eq a => IdentityOp 'Add (FreeGroup a) where
    identity# _ = FreeGroup []
instance Eq a => AssociativeOp 'Add (FreeGroup a)
instance Eq a => BinOp 'Sub (FreeGroup a) where
    op# p a b = a + inv# p b
instance Eq a => InverseOp 'Add (FreeGroup a) where
    type InvOp 'Add (FreeGroup a) = 'Sub
    inv# p = FreeGroup . map (right (inv# p)) . unFreeGroup
instance Eq a => InverseOp 'Sub (FreeGroup a) where
    type InvOp 'Sub (FreeGroup a) = 'Add
    inv# _ = inv# (proxy# @'Sub)

instance CatFunctor HASK HASK FreeGroup where
    catMap :: forall a b. (a -> b) -> FreeGroup a -> FreeGroup b
    catMap = to coerce ((map . left) :: (a -> b) -> [(a, Int)] -> [(b, Int)])

normalizeFreeGroup :: forall a. Eq a => FreeGroup a -> FreeGroup a
normalizeFreeGroup = to coerce (foldr go [] :: [(a, Int)] -> [(a, Int)])
  where
    go a [] = [a]
    go (b, nb) ((a, na) : as) | a == b = (a, na + nb) : as
    go a as = a : as
