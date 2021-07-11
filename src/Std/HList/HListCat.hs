module Std.HList.HListCat where

import "this" Std.Cat
import "this" Std.HList
import "this" Std.Type


applyHList :: HList a -> HListCat HASK a b -> HList b
applyHList a HLCId = a
applyHList HNil (HLCVal BiHNil) = HNil
applyHList (a ::: as) (HLCVal (BiHCons f fs)) = f a ::: applyHList as (HLCVal fs)

data BiHListT f as bs where
    BiHNil  :: BiHListT f '[] '[]
    BiHCons :: (a `f` b) -> BiHListT f as bs -> BiHListT f (a ': as) (b ':bs)
data HListCat f as bs where
    HLCId  :: HListCat f as as
    HLCVal :: BiHListT f as bs -> HListCat f as bs

instance Semigroupoid cat => Semigroupoid (BiHListT cat) where
    BiHNil . BiHNil = BiHNil
    BiHCons f fs . BiHCons g gs = BiHCons (f . g) (fs . gs)
instance Semigroupoid cat => Semigroupoid (HListCat cat) where
    HLCId . f = f
    f . HLCId = f
    HLCVal a . HLCVal b = HLCVal (a . b)
instance CatId cat => CatId (HListCat cat) where
    id = HLCId 
instance Category cat => Category (HListCat cat)

instance LeftFunctor f => CatLeftFunctor' Unconstrained Unconstrained (HListCat HASK) HASK (BiHListT f) where
    left' HLCId l = l
    left' (HLCVal BiHNil) BiHNil = BiHNil
    left' (HLCVal (BiHCons f fs)) (BiHCons a as) = BiHCons (left' f a) (left' (HLCVal fs) as)
instance RightFunctor f => CatRightFunctor' Unconstrained Unconstrained (HListCat HASK) HASK (BiHListT f) where
    right' HLCId l = l
    right' (HLCVal BiHNil) BiHNil = BiHNil
    right' (HLCVal (BiHCons f fs)) (BiHCons a as) = BiHCons (right' f a) (right' (HLCVal fs) as)
instance Bifunctor f => CatBifunctor' Unconstrained Unconstrained (HListCat HASK) (HListCat HASK) HASK (BiHListT f) where
    catBimap' HLCId HLCId l = l
    catBimap' f HLCId l = left' f l
    catBimap' HLCId f l = right' f l
    catBimap' (HLCVal BiHNil) (HLCVal BiHNil) BiHNil = BiHNil
    catBimap' (HLCVal (BiHCons f fs)) (HLCVal (BiHCons g gs)) (BiHCons a as) = BiHCons (catBimap' f g a) (catBimap' (HLCVal fs) (HLCVal gs) as)
