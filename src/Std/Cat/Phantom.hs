module Std.Cat.Phantom where

import "base" Data.Ord qualified as Base
import "base" Data.Either
import "base" Data.Void

import "this" Std.Ord
import "this" Std.Type
import "this" Std.Cat

-- | A Category that contains every Object in k and unique morphisms between every pair of objects
data Phantom k (a :: k) (b :: k) = Phantom
  deriving (Eq, Base.Ord)

-- | The Category that only contains a single object and its identity
type Trivial = Phantom ()
-- | The empty Category
type Empty = Phantom Void

instance CatIsomorphic (Phantom k) a b where
    catIso = Phantom :<-> Phantom

instance CatIsomorphic HASK () (Phantom k a b) where
    catIso = const Phantom :<-> const ()

instance Semigroupoid (Phantom k) where _ . _ = Phantom
instance CatId        (Phantom k) where id = Phantom
instance Category     (Phantom k)
instance Groupoid     (Phantom k) where invCat _ = Phantom

instance CatFunctor' Unconstrained (Phantom k) (Phantom k) f where catMap _ = Phantom

instance Cartesian (Phantom Type) where
    type Product (Phantom Type) = (,)
    fst      = Phantom
    snd      = Phantom
    diagonal = Phantom
    _ &&& _  = Phantom
instance Cocartesian (Phantom Type) where
    type Coproduct (Phantom Type) = Either
    lft        = Phantom
    rght       = Phantom
    codiagonal = Phantom
    _ ||| _    = Phantom
instance Closed (Phantom Type) where
    type Exp (Phantom Type) = Phantom Type
    apply     = Phantom
    curry   _ = Phantom
    uncurry _ = Phantom
instance CatInitial (Phantom Type) where
    type Initial (Phantom Type) = ()
    initiate = Phantom
instance CatTerminal (Phantom Type) where
    type Terminal (Phantom Type) = Void
    terminate = Phantom
instance CatDistributive (Phantom Type) f where distribute = Phantom
instance CatFix      (Phantom Type) where fix = Phantom
instance CatMonoidal (Phantom Type) f where
    type Id (Phantom Type) f = ()
    idl = Phantom :<-> Phantom
    idr = Phantom :<-> Phantom
instance CatArrow HASK        f (Phantom k) where catArr _ = Phantom
instance CatArrow (Phantom k) f g           where catArr   = Phantom
instance CatAssociative (Phantom k)    f where assoc = Phantom :<-> Phantom
instance CatCommutative (Phantom k)    f where commute = Phantom
instance CatPure'        Unconstrained (Phantom Type) f where catPure = Phantom
instance CatAp'          Unconstrained (Phantom Type) f where (<**>) _ = Phantom
instance CatLift2'       Unconstrained (Phantom Type) f where lift2 _ = Phantom
instance CatApplicative' Unconstrained (Phantom Type) f
instance CatEmpty'       Unconstrained (Phantom Type) f where catEmpty = Phantom
instance CatCombine'     Unconstrained (Phantom Type) f where combine = Phantom
instance CatAlternative' Unconstrained (Phantom Type) f
instance CatBind'        Unconstrained (Phantom Type) f where (=<<) _ = Phantom
instance CatJoin'        Unconstrained (Phantom Type) f where join = Phantom
instance CatMonad'       Unconstrained (Phantom Type) f
instance CatExtract'     Unconstrained (Phantom Type) f where extract = Phantom
instance CatExtend'      Unconstrained (Phantom Type) f where (<<=) _ = Phantom
instance CatDuplicate'   Unconstrained (Phantom Type) f where duplicate = Phantom
instance CatComonad'     Unconstrained (Phantom Type) f

instance Category c0 => CatLeftFunctor'  Unconstrained Unconstrained c0 (Phantom k) f where left'  _ = Phantom
instance Category c0 => CatRightFunctor' Unconstrained Unconstrained c0 (Phantom k) f where right' _ = Phantom
instance (Category c0, Category c1) => CatBifunctor' Unconstrained Unconstrained c0 c1 (Phantom k) f where

instance Category c0 => CatLeftFunctor'  Unconstrained Unconstrained c0 HASK (Phantom k) where left'  _ _ = Phantom
instance Category c0 => CatRightFunctor' Unconstrained Unconstrained c0 HASK (Phantom k) where right' _ _ = Phantom
instance (Category c0, Category c1) => CatBifunctor' Unconstrained Unconstrained c0 c1 HASK (Phantom k) where
