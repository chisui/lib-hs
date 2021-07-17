{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Cat.Discrete
    ( type (:~:)(..), Discrete, type (<:~:>)
    ) where

import "this" Std.Type
import "this" Std.Cat


type Discrete = (:~:)
type (<:~:>) = Iso Discrete

instance Semigroupoid' Unconstrained (:~:) where Refl . Refl = Refl
instance CatId'        Unconstrained (:~:) where id = Refl
instance Category'     Unconstrained (:~:)
instance Groupoid'     Unconstrained (:~:) where catInv Refl = Refl
instance CatCommutative HASK (:~:) where commute Refl = Refl
instance CatId cat => CatArrow HASK (:~:) cat where catArr Refl = id

instance CatFunctor' Unconstrained (:~:) (:~:) f where catMap Refl = Refl
instance CatFunctor' Unconstrained (:~:) HASK  f where catMap Refl = id

instance Category cat => CatLeftFunctor'  Unconstrained Unconstrained (:~:)       cat f where left'     Refl      = id
instance Category cat => CatRightFunctor' Unconstrained Unconstrained       (:~:) cat f where right'         Refl = id
instance Category cat => CatBifunctor'    Unconstrained Unconstrained (:~:) (:~:) cat f where catBimap' Refl Refl = id

instance CatIsomorphic HASK (f :~: g) (NT (:~:) f g) where
    catIso = (\Refl -> NT Refl) :<-> (\(NT Refl) -> Refl)
