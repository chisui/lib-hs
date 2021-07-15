{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MagicHash #-}
module Std.Generic.Simple where

import "base" Data.Either

import "this" Std.Generic
import "this" Std.Union
import "this" Std.HList
import "this" Std.Type
import "this" Std.Cat
import "this" Std.Ord
import "this" Std.Tuple ()
import "this" Std.Singleton


type family (|:) a b where
    Union a |: Union b = Union (Concat a b)
    a |: Union b = Union (a ': b)
    Union a |: b = Union (Concat a '[b])
    a |: b = Union '[a, b]
infixr 5 |:

type family (&:) a b where
    HList a &: HList b = HList (Concat a b)
    a &: HList b = HList (a ': b)
    HList a &: b = HList (Concat a '[b])
    a &: b = HList '[a, b]
infixr 6 &:

class SimplifyRep rep where
    type SimplifyedRep rep
    simple :: rep x <-> SimplifyedRep rep

instance SimplifyRep f => SimplifyRep (M1 a b f) where
    type SimplifyedRep (M1 a b f) = SimplifyedRep f
    simple = simple . etaIso @f
instance SimplifyRep (K1 a b) where
    type SimplifyedRep (K1 a b) = b
    simple = iso @b

instance SimplifySumRep 'True (f :+: g) => SimplifyRep (f :+: g) where
    type SimplifyedRep (f :+: g) = Union (SimplifyedSumRep 'True (f :+: g))
    simple = simpleSum (proxy# @'True)

instance SimplifyPrdRep 'True (f :*: g) => SimplifyRep (f :*: g) where
    type SimplifyedRep (f :*: g) = HList (SimplifyedPrdRep 'True (f :*: g))
    simple = simplifyPrd (proxy# @'True)

class SimplifySumRep (b :: Bool) rep where
    type SimplifyedSumRep b rep :: [Type]
    simpleSum :: Proxy# b -> rep x <-> Union (SimplifyedSumRep (IsSum rep) rep)

type family IsSum l :: Bool where
    IsSum (_ :+: _) = 'True
    IsSum _ = 'False

instance ( SimplifySumRep (IsSum l) l, SimplifySumRep (IsSum r) r
         , Known (Length (SimplifyedSumRep (IsSum l) l))
         , (SimplifyedSumRep (IsSum l) l :<: Concat (SimplifyedSumRep (IsSum l) l) (SimplifyedSumRep (IsSum r) r))
         , (SimplifyedSumRep (IsSum r) r :<: Concat (SimplifyedSumRep (IsSum l) l) (SimplifyedSumRep (IsSum r) r))
         ) => SimplifySumRep 'True (l :+: r) where
    type SimplifyedSumRep 'True (l :+: r) = Concat (SimplifyedSumRep (IsSum l) l) (SimplifyedSumRep (IsSum r) r)
    simpleSum :: forall x. Proxy# 'True -> (l :+: r) x <-> Union (Concat (SimplifyedSumRep (IsSum l) l) (SimplifyedSumRep (IsSum r) r))
    simpleSum _ = invCat splitUAtIso . (l *** r) . (iso :: (l :+: r) x <-> Either (l x) (r x))
      where
        l = simpleSum (proxy# @(IsSum l))
        r = simpleSum (proxy# @(IsSum r))

instance (SimplifyRep rep, IsSum rep ~ 'False) => SimplifySumRep 'False rep where
    type SimplifyedSumRep 'False rep = '[SimplifyedRep rep]
    simpleSum _ = invCat iso . simple

class SimplifyPrdRep (b :: Bool) rep where
    type SimplifyedPrdRep b rep :: [Type]
    simplifyPrd :: Proxy# b -> rep x <-> HList (SimplifyedPrdRep b rep)

type family IsPrd l :: Bool where
    IsPrd (_ :*: _) = 'True
    IsPrd _ = 'False

instance ( SimplifyPrdRep (IsPrd l) l
         , SimplifyPrdRep (IsPrd r) r
         , SplittableAt (SimplifyedPrdRep (IsPrd l) l) (SimplifyedPrdRep (IsPrd r) r)
         ) => SimplifyPrdRep 'True (l :*: r) where
    type SimplifyedPrdRep 'True (l :*: r) = Concat (SimplifyedPrdRep (IsPrd l) l) (SimplifyedPrdRep (IsPrd r) r)
    simplifyPrd :: forall x. Proxy# 'True -> (l :*: r) x <-> HList (Concat (SimplifyedPrdRep (IsPrd l) l) (SimplifyedPrdRep (IsPrd r) r))
    simplifyPrd _ = invCat splitHAtIso . (l *** r) . iso @(l x, r x)
      where
        l = simplifyPrd (proxy# @(IsPrd l))
        r = simplifyPrd (proxy# @(IsPrd r))

instance SimplifyRep rep => SimplifyPrdRep 'False rep where
    type SimplifyedPrdRep 'False rep = '[SimplifyedRep rep]
    simplifyPrd _ = invCat (isoThrough @(Identity (SimplifyedRep rep))) . simple

type SimpleRep a = SimplifyedRep (Rep a)
class    (Generic a, SimplifyRep (Rep a)) => GenericSimple a where simpleRep :: a <-> SimpleRep a
instance (Generic a, SimplifyRep (Rep a)) => GenericSimple a where simpleRep = simple . rep

type GThroughSimple c a = (GenericSimple a, c (SimpleRep a))
