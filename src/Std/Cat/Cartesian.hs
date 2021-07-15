module Std.Cat.Cartesian where

import "base" Data.Kind

import "this" Std.Cat.Class
import "this" Std.Cat.Bifunctor


class (Category cat, EndoBifunctor cat (Product cat)) => Cartesian (cat :: k -> k -> Type) where
    type Product cat :: k -> k -> k
    fst :: Product cat a b `cat` a
    snd :: Product cat a b `cat` b

    diagonal :: a `cat` Product cat a a
    diagonal = id &&& id

    (&&&) :: b `cat` c -> b `cat` c' -> b `cat` Product cat c c'
    infixr 3 &&&
    f &&& g = catBimap f g . diagonal
    {-# MINIMAL fst, snd, (diagonal | (&&&)) #-}

type family Fst (a :: p l r) :: l where Fst (p a _) = a
type family Snd (a :: p l r) :: r where Snd (p _ b) = b

instance Cartesian HASK where
    type Product HASK = (,)
    fst (a, _) = a
    snd (_, b) = b
    (&&&) f g a = (f a, g a)
