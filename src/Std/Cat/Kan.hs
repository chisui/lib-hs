module Std.Cat.Kan where

import "this" Std.Type
import "this" Std.Cat


data Ran
        (mC :: oC -> oC -> Type)
        (mD :: oD -> oD -> Type)
        (mC' :: oC' -> oC' -> Type)
        (g :: oC -> oC')
        (h :: oC -> oD)
        (a :: oC')
    = forall (z :: oC' -> oD). CatFunctor mC' mD z 
        => Ran
            (forall x. z (g x) `mD` h x)
            (Terminal mD `mD` z a)


--instance CatFunctor' Unconstrained HASK HASK (Ran g h) where
--    catMap f m = Ran (\k -> runRan m (k . f))
