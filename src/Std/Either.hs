{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Either
    ( Either(..)
    , either
    , isLeft,     isRight
    , fromLeft,   fromRight
    , unpackLeft, unpackRight
    ) where

import "base" Data.Either
import "base" Data.Void

import "this" Std.Cat


instance CatIsomorphic HASK (Either Void a) a where
    catIso = (\(Right a) -> a) :<-> Right

instance CatIsomorphic HASK (Either a Void) a where
    catIso = (\(Left a) -> a) :<-> Left

unpackLeft :: Alternative f => Either a b -> f a
unpackLeft = pure ||| const empty

unpackRight :: Alternative f => Either a b -> f b
unpackRight = const empty ||| pure
