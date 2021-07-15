{-# OPTIONS_GHC -Wno-orphans #-}
module Std.IO
    ( Base.IO
    ) where

import "base" GHC.IO qualified as Base

import "this" Std.Cat


deriving via (Basic1 Base.IO) instance CatFunctor HASK HASK Base.IO
deriving via (Basic1 Base.IO) instance CatPure         HASK Base.IO
deriving via (Basic1 Base.IO) instance CatAp           HASK Base.IO
deriving via (Basic1 Base.IO) instance CatLift2        HASK Base.IO
deriving via (Basic1 Base.IO) instance CatApplicative  HASK Base.IO
deriving via (Basic1 Base.IO) instance CatEmpty        HASK Base.IO
deriving via (Basic1 Base.IO) instance CatCombine      HASK Base.IO
deriving via (Basic1 Base.IO) instance CatAlternative  HASK Base.IO
deriving via (Basic1 Base.IO) instance CatBind         HASK Base.IO
deriving via (Basic1 Base.IO) instance CatJoin         HASK Base.IO
deriving via (Basic1 Base.IO) instance CatMonad        HASK Base.IO
deriving via (Basic1 Base.IO) instance CatMonadFail    HASK Base.IO
