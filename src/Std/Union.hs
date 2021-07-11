{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Union
    ( Union(..) , UnionT
    , inject, injectAt, injectAt#, project
    , decompose, decomposeLast
    , weaken, splitU, splitUIso
    , mapUnion
    , type (:<), Element
    , type (:<:), Elements
    , type (:$), ApplyU(..)
    , toSome, applyU2
    ) where

import "base" Prelude ( Int, fromIntegral )
import "base" Data.Either ( Either(..), either )
import "base" Data.Maybe ( Maybe(..), fromJust )
import "base" Data.List ( elemIndex )
import "base" Unsafe.Coerce ( unsafeCoerce )

import "deepseq" Control.DeepSeq ( NFData(..) )
import "hashable" Data.Hashable ( Hashable(..) )

import "this" Std.IfThenElse
import "this" Std.Bool
import "this" Std.Singleton
import "this" Std.Cat
import "this" Std.BinOp
import "this" Std.Ord
import "this" Std.Partial
import "this" Std.Literal
import "this" Std.Debug
import "this" Std.Text
import "this" Std.Some
import "this" Std.Type


type Elements ts l = Known (ElemIndices ts l)
type (ts :<: l) = Elements ts l
infixr 5 :<:

data Union (l :: [Type]) where
    UnsafeInternalUnion :: {-# UNPACK #-} !Int -> a -> Union l

newtype UnionT (l :: [k]) (f :: k -> Type) = UnionT (Union (Map f l))

natV :: forall (n :: Nat). Known n => Proxy# n -> Int
natV p = fromIntegral (val# p)

instance CatIsomorphic HASK (Union '[a]) a where
    catIso = decomposeLast:<-> inject

-- | Inject a functor into a type-aligned Union.
inject :: forall l a. (a :< l) => a -> Union l
inject = injectAt# (proxy# @(ElemIndex a l))
{-# INLINE inject #-}

injectAt :: forall n l proxy. Known n => proxy n -> l ! n -> Union l
injectAt _ = injectAt# (proxy# @n)
{-# INLINE injectAt #-}

injectAt# :: forall n l. Known n => Proxy# n -> l ! n -> Union l
injectAt# p = UnsafeInternalUnion (natV p)
{-# INLINE injectAt# #-}

-- | Maybe project a functor out of a type-aligned Union.
project :: forall a l m. (Alternative m, a :< l) => Union l -> m a
project (UnsafeInternalUnion n' x) = guard (natV (proxy# @(ElemIndex a l)) == n') $> unsafeCoerce x
{-# INLINE project #-}


-- | Attempts to extract the head type @e@ from a @Union@. Returns
-- @Right@ on success, and a @Union@ without @e@ otherwise. You can
-- repeatedly applyU this and applyU 'decomposeLast' when you have @Union
-- '[e]@ to get typesafe, exhaustive matching of an open Union. See
-- @examples/Errors.hs@ for a full example.
decompose :: Union (e ': es) -> Either (Union es) e
decompose = decomposeOffset 0
{-# INLINE decompose #-}

decomposeOffset :: Int -> Union (e ': es) -> Either (Union es) e
decomposeOffset !i (UnsafeInternalUnion n v)
    | i == n    = Right (unsafeCoerce v)
    | otherwise = Left (UnsafeInternalUnion (n + 1) v)
{-# INLINE decomposeOffset #-}

-- | Special case of 'decompose' which knows that there is only one
-- possible type remaining in the @Union@, @e@ thus it is guaranteed to
-- return @e@
decomposeLast :: HasCallStack => Union '[a] -> a
decomposeLast (UnsafeInternalUnion 0 v) = unsafeCoerce v
decomposeLast (UnsafeInternalUnion n _) = error [fmt| malformed union: unions of exactly one type have to have the index 0 but got $n |]
{-# INLINE decomposeLast #-}

weaken :: forall a b. (a :<: b) => Union a -> Union b
weaken (UnsafeInternalUnion n a) = UnsafeInternalUnion n' a
  where n' = fromJust . elemIndex n . map fromIntegral $ val' @(ElemIndices a b)

splitU :: forall a b proxy. Known (Length a) => proxy '(a, b) -> Union (Concat a b) -> Either (Union a) (Union b)
splitU _ (UnsafeInternalUnion n a)
    = let l = natV (proxy# @(Length a))
    in if n >= l
        then Left (UnsafeInternalUnion (n - l) a)
        else Right (UnsafeInternalUnion n a)

splitUIso :: forall a b. Known (Length a) => Union (Concat a b) <-> Either (Union a) (Union b)
splitUIso = splitU (Proxy @'(a, b)) :<-> from'
  where
    from' (Left u) = unsafeCoerce u -- left doesn't need offset
    from' (Right (UnsafeInternalUnion n a)) = UnsafeInternalUnion (n + natV (proxy# @(Length a))) a

mapUnion :: (forall a. a -> f a) -> Union l -> UnionT l f
mapUnion f (UnsafeInternalUnion n v) = UnionT (UnsafeInternalUnion n (f a))
  where a = unsafeCoerce v :: Any -- can be done since the function can not touch a anyways.

type (:$) = ApplyU

class ApplyU (c :: Type -> Constraint) (fs :: [Type]) where
    applyU :: proxy c -> (forall a. c a => a -> b) -> Union fs -> b
    applyU _ = applyU# (proxy# @c)
    applyU# :: Proxy# c -> (forall a. c a => a -> b) -> Union fs -> b

instance ApplyU' c 0 a as => ApplyU c (a ': as) where
    applyU# _ = applyU' (proxy# @'(c, 0, a, as))

class ApplyU' (c :: Type -> Constraint) (n :: Nat) (a :: Type) (as :: [Type]) where
    applyU' :: Proxy# '(c, n, a, as) -> (forall x. c x => x -> b) -> Union (a ': as) -> b

instance (Known n, c a, ApplyU' c (n + 1) a (b ': as)) => ApplyU' c n a (b ': as) where
    applyU' _ f u = either next f . decomposeOffset (natV (proxy# @n)) $ u
      where next _ = applyU' (proxy# @'(c, n + 1, a, b ': as)) f u
    {-# INLINE applyU' #-}
instance (Known n, c a) => ApplyU' c n a '[] where
    applyU' _ f = f . decomposeLast
    {-# INLINE applyU' #-}

toSome :: forall c l. c :$ l => Union l -> Some c
toSome = applyU# (proxy# @c) Some

applyU2# :: forall c l b. c :$ l => Proxy# c -> (forall a. c a => a -> a -> b) -> Union l -> Union l -> Maybe b
applyU2# _ f (UnsafeInternalUnion i a) u@(UnsafeInternalUnion j _)
    = guard (i == j) $> applyU# (proxy# @c) (f (unsafeCoerce a)) u
{-# INLINABLE applyU2# #-}

applyU2 :: forall c l b proxy. c :$ l => proxy c -> (forall a. c a => a -> a -> b) -> Union l -> Union l -> Maybe b
applyU2 _ = applyU2# (proxy# @c)
{-# INLINABLE applyU2 #-}


instance (Hashable :$ l) => Hashable (Union l) where
    hashWithSalt salt = applyU# (proxy# @Hashable) (hashWithSalt salt)
    {-# INLINABLE hashWithSalt #-}

instance (NFData :$ l) => NFData (Union l) where
    rnf = applyU# (proxy# @NFData) rnf
    {-# INLINABLE rnf #-}

instance (Show :$ l) => Show (Union l) where
    showsPrec i u
        = showParen (i >= 10)
        $ showString "Union "
        . applyU# (proxy# @Show) shows u

instance Eq a => Eq (Union '[a]) where
    a == b = decomposeLast a == decomposeLast b
instance (Eq (Union (a ': as)), Eq b) => Eq (Union (b ': a ': as)) where
    a == b = decompose a == decompose b

instance (Ord' t a) => Ord' t (Union '[a]) where
    a `compare'` b = decomposeLast a `compare'` decomposeLast b
instance (Ord' u (Union (a ': as)), Ord' v b, t ~ Min u v) => Ord' t (Union (b ': a ': as)) where
    a `compare'` b = decompose a `compare'` decompose b


instance CatFunctor (~>) (->) (UnionT l) where
    catMap (NT f) (UnionT (UnsafeInternalUnion n v)) = UnionT (UnsafeInternalUnion n (f a))
      where a = unsafeCoerce v :: f Any -- can be done since the function can not touch a anyways.

instance CatIsomorphic HASK (Union '[a, b]) (Either a b) where
    catIso = either (Right . decomposeLast) Left . decompose
        :<-> injectAt# (proxy# @0) `either` injectAt# (proxy# @1)
