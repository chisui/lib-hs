module Std.Cat.Cartesian where

import "base" Data.Kind

import "this" Std.Cat.Class
import "this" Std.Cat.Bifunctor
import "this" Std.Constraint


class EndoBifunctor Unconstraint cat (Product cat) => Cartesian (cat :: k -> k -> Type) where
    type Product cat :: k -> k -> k
    fst :: Product cat a b `cat` a
    snd :: Product cat a b `cat` b

    copy :: a `cat` Product cat a a
    copy = id &&& id

    (&&&) :: b `cat` c -> b `cat` c' -> b `cat` Product cat c c'
    infixr 3 &&&
    f &&& g = catBimap f g . copy
    {-# MINIMAL fst, snd, (copy | (&&&)) #-}

instance Cartesian HASK where
    type Product HASK = (,)
    fst (a, _) = a
    snd (_, b) = b
    (&&&) f g a = (f a, g a)
