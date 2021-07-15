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
import "base" Data.Monoid ( First(..), Last(..) )

import "this" Std.Cat
import "this" Std.Type
import "this" Std.Partial


instance CatIsomorphic HASK (Maybe Void) () where
    catIso = const () :<-> const Nothing
instance CatIsomorphic HASK (Maybe a) (Either () a) where
    catIso = maybe (Left ()) Right :<-> either (const Nothing) Just
instance CatIsomorphic HASK (Maybe a) (Res 'Partial a) where
    catIso = unpackMaybe :<-> unpackRes
instance CatIsomorphic HASK (Res 'Partial a) (Maybe a) where
    catIso = etaIso
instance CatIsomorphic (~>) (Res 'Partial) Maybe where
    catIso = NT unpackRes :<-> NT toRes

unpackMaybe :: forall f a. Alternative f => Maybe a -> f a
unpackMaybe = maybe empty pure

deriving via (Basic1 Maybe) instance CatFunctor'     Unconstrained HASK HASK Maybe
deriving via (Basic1 Maybe) instance CatPure'        Unconstrained HASK Maybe
deriving via (Basic1 Maybe) instance CatAp'          Unconstrained HASK Maybe
deriving via (Basic1 Maybe) instance CatLift2'       Unconstrained HASK Maybe
deriving via (Basic1 Maybe) instance CatApplicative' Unconstrained HASK Maybe
deriving via (Basic1 Maybe) instance CatEmpty'       Unconstrained HASK Maybe
deriving via (Basic1 Maybe) instance CatCombine'     Unconstrained HASK Maybe
deriving via (Basic1 Maybe) instance CatAlternative' Unconstrained HASK Maybe
deriving via (Basic1 Maybe) instance CatBind'        Unconstrained HASK Maybe
deriving via (Basic1 Maybe) instance CatJoin'        Unconstrained HASK Maybe
deriving via (Basic1 Maybe) instance CatMonad'       Unconstrained HASK Maybe


deriving via Maybe instance CatFunctor'     Unconstrained HASK HASK First
deriving via Maybe instance CatPure'        Unconstrained HASK First
deriving via Maybe instance CatAp'          Unconstrained HASK First
deriving via Maybe instance CatLift2'       Unconstrained HASK First
deriving via Maybe instance CatApplicative' Unconstrained HASK First
deriving via Maybe instance CatEmpty'       Unconstrained HASK First
instance CatCombine' Unconstrained HASK First where combine = fst
deriving via Maybe instance CatAlternative' Unconstrained HASK First
deriving via Maybe instance CatBind'        Unconstrained HASK First
deriving via Maybe instance CatJoin'        Unconstrained HASK First
deriving via Maybe instance CatMonad'       Unconstrained HASK First

deriving via Maybe instance CatFunctor'     Unconstrained HASK HASK Last
deriving via Maybe instance CatPure'        Unconstrained HASK Last
deriving via Maybe instance CatAp'          Unconstrained HASK Last
deriving via Maybe instance CatLift2'       Unconstrained HASK Last
deriving via Maybe instance CatApplicative' Unconstrained HASK Last
deriving via Maybe instance CatEmpty'       Unconstrained HASK Last
instance CatCombine' Unconstrained HASK Last where combine = snd
deriving via Maybe instance CatAlternative' Unconstrained HASK Last
deriving via Maybe instance CatBind'        Unconstrained HASK Last
deriving via Maybe instance CatJoin'        Unconstrained HASK Last
deriving via Maybe instance CatMonad'       Unconstrained HASK Last
