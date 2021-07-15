{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MagicHash #-}
module Std.HList where

import "base" GHC.Int

import "this" Std.Bool
import "this" Std.Type
import "this" Std.Cat
import "this" Std.BinOp
import "this" Std.Literal
import "this" Std.Ord
import "this" Std.Partial
import "this" Std.Union


data HList l where
    HNil  :: HList '[]
    (:::) :: a -> HList as -> HList (a ': as)
infixr 5 :::

newtype HListT (l :: [k]) (f :: k -> Type) = HListT
    { unHListT :: HList (Map f l)
    }

mapToList :: forall c l b proxy. c <$> l => proxy c -> (forall a. c a => a -> b) -> HList l -> [b]
mapToList _ = mapToList# (proxy# @c)

mapToList# :: c <$> l => Proxy# c -> (forall a. c a => a -> b) -> HList l -> [b]
mapToList# _ _ HNil = []
mapToList# p f (a ::: as) = f a : mapToList# p f as

toList :: forall a l. (==) a <$> l => HList l -> [a]
toList = mapToList# (proxy# @((==) a)) (from (same @a))

explodeH :: forall l. HList l -> [Union l]
explodeH = explodeH' 0
  where
    explodeH' :: Int -> HList x -> [Union l]
    explodeH' _ HNil = []
    explodeH' n (a ::: as) = UnsafeInternalUnion n a : explodeH' (n + 1) as

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


class SplittableAt a b where
    splitHAt :: HList (Concat a b) -> (HList a, HList b)
instance SplittableAt '[] b where
    splitHAt = (HNil,)
instance SplittableAt as b => SplittableAt (a ': as) b where
    splitHAt (a ::: as) = let (l', r') = splitHAt as in (a ::: l', r')

splitHAtIso :: SplittableAt a b => HList (Concat a b) <-> (HList a, HList b)
splitHAtIso = splitHAt :<-> uncurry concatH

instance CatFunctor' Unconstrained (~>) (->) (HListT '[]) where
    catMap :: forall f g. f ~> g -> HListT '[] f -> HListT '[] g
    catMap _ = to coerce :: HListT '[] f -> HListT '[] g

instance CatFunctor (~>) (->) (HListT as) => CatFunctor' Unconstrained (~>) (->) (HListT (a ': as)) where
    catMap :: forall f g. f ~> g -> HListT (a ': as) f -> HListT (a ': as) g
    catMap f = to coerce map' :: HListT (a ': as) f -> HListT (a ': as) g
      where
        map' :: HList (Map f (a ': as)) -> HList (Map g (a ': as))
        map' (a ::: as) = eta f a ::: mapNext as
        mapNext :: HList (Map f as) -> HList (Map g as)
        mapNext = to coerce (catMap f :: HListT as f -> HListT as g)

instance Eq (HList '[]) where
    _ == _ = True
    _ /= _ = False
instance (Eq a, Eq (HList as)) => Eq (HList (a ': as)) where
    (a ::: as) == (b ::: bs) = a == b && as == bs

instance Ord' 'Total (HList '[]) where
    compare' _ _ = pure EQ
instance (Ord' u a, Ord' v (HList as), t ~ MinTotallity u v) => Ord' t (HList (a ': as)) where
    compare' (a ::: as) (b ::: bs) = zipRes (++) (compare' a b) (compare' as bs)


instance BinOp 'Canonic (HList '[]) where
    op# _ HNil HNil = HNil
instance ( TotalBinOp 'Canonic a
         , TotalBinOp 'Canonic (HList as)) => BinOp 'Canonic (HList (a ': as)
         ) where
    op# p (a ::: as) (b ::: bs) = zip' (:::) (op# p a b) (op# p as bs)
      where
        zip' :: (a -> HList as -> HList (a ': as))
             -> OpRes 'Canonic a
             -> OpRes 'Canonic (HList as)
             -> OpRes 'Canonic (HList (a : as))
        zip' = zipDirectRes#
                (proxy# @(OpTotallity 'Canonic a))
                (proxy# @(OpTotallity 'Canonic (HList as)))

instance IdentityOp 'Canonic (HList '[]) where
    identity# _ = HNil
instance ( TotalBinOp 'Canonic a, IdentityOp 'Canonic a
         , TotalBinOp 'Canonic (HList as), IdentityOp 'Canonic (HList as)
         ) => IdentityOp 'Canonic (HList (a ': as)) where
    identity# p = zip' (:::) (identity# p) (identity# p)
      where
        zip' :: (a -> HList as -> HList (a ': as))
             -> OpRes 'Canonic a
             -> OpRes 'Canonic (HList as)
             -> OpRes 'Canonic (HList (a : as))
        zip' = zipDirectRes#
                (proxy# @(OpTotallity 'Canonic a))
                (proxy# @(OpTotallity 'Canonic (HList as)))
