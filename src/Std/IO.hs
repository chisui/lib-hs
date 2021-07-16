{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.IO
    ( Base.IO
    ) where

import "base" GHC.IO qualified as Base

import "this" Std.Type
import "this" Std.Cat


deriving via (Basic1 Base.IO) instance CatFunctor'     Unconstrained HASK HASK Base.IO
deriving via (Basic1 Base.IO) instance CatPure'        Unconstrained HASK Base.IO
deriving via (Basic1 Base.IO) instance CatAp'          Unconstrained HASK Base.IO
deriving via (Basic1 Base.IO) instance CatLift2'       Unconstrained HASK Base.IO
deriving via (Basic1 Base.IO) instance CatApplicative' Unconstrained HASK Base.IO
deriving via (Basic1 Base.IO) instance CatEmpty'       Unconstrained HASK Base.IO
deriving via (Basic1 Base.IO) instance CatCombine'     Unconstrained HASK Base.IO
deriving via (Basic1 Base.IO) instance CatAlternative' Unconstrained HASK Base.IO
deriving via (Basic1 Base.IO) instance CatBind'        Unconstrained HASK Base.IO
deriving via (Basic1 Base.IO) instance CatJoin'        Unconstrained HASK Base.IO
deriving via (Basic1 Base.IO) instance CatMonad'       Unconstrained HASK Base.IO
deriving via (Basic1 Base.IO) instance CatMonadFail'   Unconstrained HASK Base.IO
