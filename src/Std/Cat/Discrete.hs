{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Cat.Discrete
    ( type (:~:)(..), Discrete
    ) where

import "this" Std.Type
import "this" Std.Cat


type Discrete = (:~:)

instance Semigroupoid (:~:) where Refl . Refl = Refl
instance CatId        (:~:) where id = Refl
instance Category     (:~:)
instance Groupoid     (:~:) where catInv Refl = Refl
instance CatCommutative HASK (:~:) where commute Refl = Refl
instance CatId cat => CatArrow HASK (:~:) cat where catArr Refl = id

instance CatFunctor' Unconstrained (:~:) (:~:) f where catMap Refl = Refl
instance CatFunctor' Unconstrained (:~:) HASK  f where catMap Refl = id

instance CatLeftFunctor'  Unconstrained Unconstrained (:~:)       HASK f where left'     Refl      = id
instance CatRightFunctor' Unconstrained Unconstrained       (:~:) HASK f where right'         Refl = id
instance CatBifunctor'    Unconstrained Unconstrained (:~:) (:~:) HASK f where catBimap' Refl Refl = id
