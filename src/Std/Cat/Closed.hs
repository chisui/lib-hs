module Std.Cat.Closed where

import "this" Std.Cat.Class
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Associative
import "this" Std.Cat.Bifunctor


class Cartesian cat => Closed cat where
    type Exp cat :: k -> k -> k
    apply :: Product cat (Exp cat a b) a `cat` b
    curry :: (Product cat a b `cat` c) -> a `cat` Exp cat b c
    uncurry :: (a `cat` Exp cat b c) -> Product cat a b `cat` c

const :: Closed cat => a `cat` Exp cat b a
const = curry fst

flip :: (Closed cat, CatAssociative cat (Product cat)) => b `cat` Exp cat a c -> a `cat` Exp cat b c
flip f = curry (uncurry f . assoc)

on' :: Closed cat => a `cat` Exp cat b c -> a' `cat` a -> b' `cat` b -> a' `cat` Exp cat b' c
on' f g h = curry (uncurry f . (g *** h))

on :: Closed cat => a `cat` Exp cat a c -> b `cat` a -> b `cat` Exp cat b c
on f g = on' f g g
infixl 0 `on`

instance Closed HASK where
    type Exp HASK = HASK
    apply (f, a) = f a
    curry f a b = f (a, b)
    uncurry f (a, b) = f a b
