module Std.Cat.Traversable where

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Applicative
import "this" Std.Cat.Foldable


class (Functor t, Foldable t) => Traversable t where
    {-# MINIMAL traverse | sequence #-}
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f = sequence . map f
    sequence :: Applicative f => t (f a) -> f (t a)
    sequence = traverse id

instance Traversable [] where
    traverse f = foldr cons_f (pure [])
      where cons_f x ys = lift2 (:) (f x) ys

