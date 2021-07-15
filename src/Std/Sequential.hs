{-# LANGUAGE DefaultSignatures #-}
module Std.Sequential where

import "this" Std.IfThenElse
import "this" Std.Bool
import "this" Std.Maybe
import "this" Std.Cat
import "this" Std.Ord
import "this" Std.BinOp
import "this" Std.Literal
import "this" Std.Cat.Foldable


class Indexable i f | f -> i where

    index :: (Empty t, Pure t) => i -> f a -> t a
    default index :: (Empty t, Pure t, Iterable i, Foldable f) => i -> f a -> t a
    index i = snd . foldr go (zero, empty)
      where
        go a (j, r) = (succ j, if j == i then pure a else r)

    (!!) :: (Empty t, Pure t) => f a -> i -> t a
    (!!) = flip index

class Indexable i f => Sequential i f | f -> i where

    uncons :: Alternative t => f a -> t (a, f a)

    splitAt :: i -> f a -> (f a, f a)
    default splitAt :: (Alternative f, Iterable i) => i -> f a -> (f a, f a)
    splitAt i f
        | i == zero = (empty, f)
        | otherwise = fromMaybe (empty, empty) $ do
            (a, as) <- uncons f
            let (l, r) = splitAt (pred i) as
            pure (cons a l, r)

    take :: i -> f a -> f a
    take i = fst . splitAt i

    drop :: i -> f a -> f a
    drop i = snd . splitAt i

    nub :: Eq a => f a -> f a

    nubOrd :: Ord a => f a -> f a

