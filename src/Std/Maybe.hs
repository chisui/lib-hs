{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Maybe
    ( Maybe(..)
    , maybe
    , isJust, isNothing
    , fromMaybe
    , unpackMaybe
    ) where

import "base" Data.Maybe
import "base" Data.Either
import "base" Data.Void

import "this" Std.Cat
import "this" Std.Partial


instance CatIsomorphic HASK (Maybe Void) () where
    catIso = const () :<-> const Nothing
instance CatIsomorphic HASK (Maybe a) (Either () a) where
    catIso = maybe (Left ()) Right :<-> either (const Nothing) Just
instance CatIsomorphic HASK (Maybe a) (Res 'Partial a) where
    catIso = unpackMaybe :<-> unpackRes

unpackMaybe :: forall f a. Alternative f => Maybe a -> f a
unpackMaybe = maybe empty pure
