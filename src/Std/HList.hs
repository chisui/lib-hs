{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MagicHash #-}
module Std.HList where

import "base" Data.Coerce

import "this" Std.Bool
import "this" Std.Type
import "this" Std.Cat
import "this" Std.Ord
import "this" Std.Partial


data HList l where
    HNil  :: HList '[]
    (:::) :: a -> HList as -> HList (a ': as)
infixr 5 :::

newtype HListT (l :: [k]) (f :: k -> Type) = HListT
    { unHListT :: HList (Map f l)
    }

singletonH :: a <-> HList '[a]
singletonH = invCat tuple . coerceIso

mapToList :: forall c l b proxy. AllImplement l c => proxy c -> (forall a. c a => a -> b) -> HList l -> [b]
mapToList _ = mapToList# (proxy# @c)

mapToList# :: AllImplement l c => Proxy# c -> (forall a. c a => a -> b) -> HList l -> [b]
mapToList# _ _ HNil = []
mapToList# p f (a ::: as) = f a : mapToList# p f as

toList :: forall a l. AllImplement l ((==) a) => HList l -> [a]
toList = mapToList# (proxy# @((==) a)) (from (same @a))

concatH :: forall a b. HList a -> HList b -> HList (Concat a b)
concatH HNil l = l
concatH (a ::: as) l = a ::: concatH as l


(!!) :: forall n l proxy. HElem (n === 0) n l => HList l -> proxy n -> l ! n
l !! _ = getAt' (proxy# @(n === 0)) (proxy# @n) l

class HElem (b :: Bool) (n :: Nat) (l :: [Type]) where
    getAt' :: Proxy# b -> Proxy# n -> HList l -> l ! n

instance HElem 'True 0 (a ': as) where
    getAt' _ _ (a ::: _) = a
instance ( HElem ((n - 1) === 0) (n - 1) as
         , (as ! (n - 1)) ~ ((a ': as) ! n)
         ) => HElem 'False n (a ': as) where
    getAt' _ _ (_ ::: as) = getAt' (proxy# @((n - 1) === 0)) (proxy# @(n - 1)) as


newtype Pair a = Pair (a, a)
zipH :: HList l -> HList l -> HListT l Pair
zipH = zipHWith (curry Pair)

zipHWith :: forall f l. (forall a. a -> a -> f a) -> HList l -> HList l -> HListT l f
zipHWith f x y = HListT (zipHWith' x y)
  where
    zipHWith' :: HList k -> HList k -> HList (Map f k)
    zipHWith' HNil HNil = HNil
    zipHWith' (a ::: as) (b ::: bs) = f a b ::: zipHWith' as bs


class Splittable a b where
    splitH :: HList (Concat a b) -> (HList a, HList b)
instance Splittable '[] b where
    splitH = (HNil,)
instance Splittable as b => Splittable (a ': as) b where
    splitH (a ::: as) = let (l', r') = splitH as in (a ::: l', r')

splitHIso :: Splittable a b => HList (Concat a b) <-> (HList a, HList b)
splitHIso = splitH :<-> uncurry concatH

instance CatFunctor (~>) (->) (HListT '[]) where
    map :: forall f g. f ~> g -> HListT '[] f -> HListT '[] g
    map _ = coerce :: HListT '[] f -> HListT '[] g

instance CatFunctor (~>) (->) (HListT as) => CatFunctor (~>) (->) (HListT (a ': as)) where
    map :: forall f g. f ~> g -> HListT (a ': as) f -> HListT (a ': as) g
    map f = coerce map' :: HListT (a ': as) f -> HListT (a ': as) g
      where
        map' :: HList (Map f (a ': as)) -> HList (Map g (a ': as))
        map' (a ::: as) = eta f a ::: mapNext as
        mapNext = coerce @(HListT as f -> HListT as g) @(HList (Map f as) -> HList (Map g as)) (map f)

instance Eq 'Total (HList '[]) where
    _ ==? _ = pure True
    _ /=? _ = pure False
instance (Eq u a, Eq v (HList as), t ~ Min u v) => Eq t (HList (a ': as)) where
    (a ::: as) ==? (b ::: bs) = zipRes (&&) (a ==? b) (as ==? bs)

instance Ord 'Total (HList '[]) where
    compare' _ _ = pure EQ
instance (Ord u a, Ord v (HList as), t ~ Min u v) => Ord t (HList (a ': as)) where
    compare' (a ::: as) (b ::: bs) = zipRes (&&) (compare' a b) (compare' as bs)

class AsTuple (l :: [Type]) (t :: Type) | l -> t, t -> l where
    tuple :: HList l <-> t

instance AsTuple '[] () where
    tuple = const () :<-> const HNil

instance AsTuple '[a] (Identity a) where
    tuple = (\(a ::: HNil) -> Identity a)
       :<-> (\(Identity a) -> a ::: HNil)

instance AsTuple '[a, b] (a, b) where
    tuple = (\(a ::: b ::: HNil) -> (a, b))
       :<-> (\(a, b) -> a ::: b ::: HNil)

instance AsTuple '[a, b, c] (a, b, c) where
    tuple = (\(a ::: b ::: c ::: HNil) -> (a, b, c))
       :<-> (\(a, b, c) -> a ::: b ::: c ::: HNil)

instance AsTuple '[a, b, c, d] (a, b, c, d) where
    tuple = (\(a ::: b ::: c ::: d ::: HNil) -> (a, b, c, d))
       :<-> (\(a, b, c, d) -> a ::: b ::: c ::: d ::: HNil)
