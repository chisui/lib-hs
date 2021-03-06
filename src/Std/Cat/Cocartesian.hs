module Std.Cat.Cocartesian where

import "base" Data.Kind
import "base" Data.Either

import "this" Std.Cat.Class
import "this" Std.Cat.Bifunctor


class EndoBifunctor cat (Coproduct cat) => Cocartesian (cat :: k -> k -> Type) where
    type Coproduct cat :: k -> k -> k
    lft  :: a `cat` Coproduct cat a b
    rght :: b `cat` Coproduct cat a b

    codiagonal :: Coproduct cat a a `cat` a
    codiagonal = id ||| id

    (|||) :: b `cat` d -> c `cat` d -> Coproduct cat b c `cat` d
    infixr 2 |||
    f ||| g = codiagonal . catBimap f g
    {-# MINIMAL lft, rght, (codiagonal | (|||)) #-}

instance Cocartesian HASK where
    type Coproduct HASK = Either
    lft = Left
    rght = Right
    (|||) = either
