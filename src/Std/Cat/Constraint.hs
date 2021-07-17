{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Std.Cat.Constraint where

import "base" Data.Type.Coercion ( Coercion(..) )

import "this" Std.Type
import "this" Std.Cat


data Dict c where
    Dict :: c => Dict c

newtype a :- b = Sub (a => Dict b)
  deriving newtype Typeable
type (⊢) = (:-)
infixr 9 :-
infixr 9 ⊢
type (<:-:>) = Iso (:-)
infixr 1 <:-:>


class         HasDict c               e | e -> c     where evidence :: e -> Dict c
instance      HasDict a               (Dict a)       where evidence = id
instance a => HasDict b               (a :- b)       where evidence (Sub x) = x
instance      HasDict (Coercible a b) (Coercion a b) where evidence Coercion = Dict
instance      HasDict (a ~ b)         (a :~: b)      where evidence Refl = Dict

infixl 1 \\
(\\) :: HasDict c e => (c => r) -> e -> r
r \\ d = case evidence d of Dict -> r


instance Semigroupoid' Unconstrained (:-) where f . g = Sub (Dict \\ f \\ g)
instance CatId'        Unconstrained (:-) where id    = Sub Dict
instance Category'     Unconstrained (:-)

deriving via (Hom (:-))   instance CatLeftFunctor'  Unconstrained Unconstrained (Op (:-)) HASK (:-)
deriving via (Hom (:-))   instance CatRightFunctor' Unconstrained Unconstrained (:-) HASK (:-)
deriving via (Hom (:-))   instance CatBifunctor'    Unconstrained Unconstrained (Op (:-)) (:-) HASK (:-)
deriving via (Hom (:-) a) instance CatFunctor'      Unconstrained (:-) HASK ((:-) a)

instance CatFunctor' Unconstrained (:-) HASK Dict where
    catMap p Dict = Dict \\ p

instance CatFullyFaithful' Unconstrained (:-) HASK Dict where
    catUnmap f = Sub (f Dict)

class    (a, b) => a :&: b
instance (a, b) => a :&: b
infixr 8 :&:

instance CatLeftFunctor'  Unconstrained Unconstrained (:-) (:-)      (:&:) where left'  f = Sub (Dict \\ f)
instance CatRightFunctor' Unconstrained Unconstrained (:-) (:-)      (:&:) where right' g = Sub (Dict \\ g)
instance CatBifunctor'    Unconstrained Unconstrained (:-) (:-) (:-) (:&:)
instance CatAssociative (:-) (:&:) where assoc   = assocProd
instance CatCommutative (:-) (:&:) where commute = commuteProd
instance a => CatFunctor'   Unconstrained (:-) (:-) ((:&:) a) where catMap f = Sub (Dict \\ f)
instance a => CatPure'      Unconstrained (:-) ((:&:) a) where catPure   = Sub Dict
instance a => CatCombine'   Unconstrained (:-) ((:&:) a) where combine   = Sub Dict
instance a => CatJoin'      Unconstrained (:-) ((:&:) a) where join      = Sub Dict
instance a => CatDuplicate' Unconstrained (:-) ((:&:) a) where duplicate = Sub Dict

instance Cartesian (:-) where
    type Product (:-) = (:&:)
    fst = Sub Dict
    snd = Sub Dict
    f &&& g = Sub (Dict \\ f \\ g)

instance CatTerminal (:-) where
    type Terminal (:-) = ()
    terminate = Sub Dict

class Any => Bottom where no :: a

instance CatInitial (:-) where
    type Initial (:-) = Bottom
    initiate = Sub no

instance CatMonoidal (:-) (:&:) where
    type Id (:-) (:&:) = ()
    idl = snd :<-> catUnmap (\Dict -> Dict)
