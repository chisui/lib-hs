{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Union
    ( Union
    , UnionT
    , inject, project
    , decompose, decomposeLast
    , weaken
    , mapUnion
    , mapUnionT
    , type (:<), Element
    , type (:<:), Elements
    , type (:$), ApplyU(..)
    , applyU2
    ) where

import "base" Prelude ( Int, Either(..), either, Maybe(..), fromIntegral, otherwise )
import "base" Data.Kind ( Type, Constraint )
import "base" Data.Maybe ( fromJust )
import "base" Data.List ( elemIndex )
import "base" Data.Functor ( ($>) )
import "base" Control.Applicative ( Alternative )
import "base" Control.Monad( guard )
import "base" GHC.TypeLits ( Nat, type (+) )
import "base" GHC.Exts ( Any )
import "base" Unsafe.Coerce ( unsafeCoerce )

import "deepseq" Control.DeepSeq ( NFData(..) )
import "hashable" Data.Hashable ( Hashable(..) )

import "this" Std.TypeError
import "this" Std.Singleton
import "this" Std.Cat
import "this" Std.Group
import "this" Std.Ord
import "this" Std.Debug


type ElemIndex (t :: k) (l :: [k]) = ElemIndex' t l 0 l

type family ElemIndex' (t :: k) (orig :: [k]) (i :: Nat) (l :: [k]) :: Nat where
    ElemIndex' t orig i (t ': _)  = i
    ElemIndex' t orig i (_ ': ts) = ElemIndex' t orig (i + 1) ts
    ElemIndex' t orig _ '[] = [typeError|
        '$t\' is not a member of the type-level list
        $orig
    |]

type Element t l = Known (ElemIndex t l)
type (t :< l) = Element t l
infixr 5 :<

type family ElemIndices a b where
    ElemIndices '[] _ = '[]
    ElemIndices (a ': as) b = ElemIndex a b ': ElemIndices as b

type Elements ts l = Known (ElemIndices ts l)
type (ts :<: l) = Elements ts l
infixr 5 :<:


data Union (l :: [Type]) where
    InternalUnion :: {-# UNPACK #-} !Int -> a -> Union l

type family MapT (f :: k -> k1) (l :: [k]) :: [k1] where
    MapT _ '[] = '[]
    MapT f (a ': as) = f a ': MapT f as
type UnionT (f :: k -> Type) (l :: [k]) = Union (MapT f l)

natV :: forall (n :: Nat). Known n => Int
natV = fromIntegral (val' @n)

-- | Inject a functor into a type-aligned Union.
inject :: forall a l. (a :< l) => a -> Union l
inject = InternalUnion (natV @(ElemIndex a l))
{-# INLINE inject #-}

-- | Maybe project a functor out of a type-aligned Union.
project :: forall a l m. (Alternative m, a :< l) => Union l -> m a
project (InternalUnion n' x) = guard (natV @(ElemIndex a l) == n') $> unsafeCoerce x
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
decomposeOffset !i (InternalUnion n v)
    | i == n    = Right (unsafeCoerce v)
    | otherwise = Left (InternalUnion (n + 1) v)
{-# INLINE decomposeOffset #-}

-- | Special case of 'decompose' which knows that there is only one
-- possible type remaining in the @Union@, @e@ thus it is guaranteed to
-- return @e@
decomposeLast :: HasCallStack => Union '[a] -> a
decomposeLast (InternalUnion 0 v) = unsafeCoerce v
decomposeLast (InternalUnion n _) = [error| malformed union: unions of exactly one type have to have the index 0 but got $n |]
{-# INLINE decomposeLast #-}

weaken :: forall a b. (a :<: b) => Union a -> Union b
weaken (InternalUnion n a) = InternalUnion n' a
  where n' = fromJust . elemIndex n . map fromIntegral $ val' @(ElemIndices a b)

mapUnion :: (forall a. a -> f a) -> Union l -> UnionT f l
mapUnion f (InternalUnion n v) = InternalUnion n (f a)
  where a = unsafeCoerce v :: Any -- can be done since the function can not touch a anyways.

mapUnionT :: (forall a. f a -> g a) -> UnionT f l -> UnionT g l
mapUnionT f (InternalUnion n v) = InternalUnion n (f a)
  where a = unsafeCoerce v :: f Any -- can be done since the function can not touch a anyways.


type (:$) = ApplyU

class ApplyU (c :: Type -> Constraint) (fs :: [Type]) where
    applyU :: (forall a. c a => a -> b) -> Union fs -> b

instance ApplyU' c 0 a as => ApplyU c (a ': as) where
    applyU = applyU' @c @0 @a @as

class ApplyU' (c :: Type -> Constraint) (n :: Nat) (a :: Type) (as :: [Type]) where
    applyU' :: (forall x. c x => x -> b) -> Union (a ': as) -> b

instance (Known n, c a, ApplyU' c (n + 1) a (b ': as)) => ApplyU' c n a (b ': as) where
    applyU' f u = either next f . decomposeOffset (natV @n) $ u
      where next _ = applyU' @c @(n + 1) @a @(b ': as) f u
    {-# INLINE applyU' #-}
instance (Known n, c a) => ApplyU' c n a '[] where
    applyU' f = f . decomposeLast
    {-# INLINE applyU' #-}

applyU2 :: forall c l b. c :$ l => (forall a. c a => a -> a -> b) -> Union l -> Union l -> Maybe b
applyU2 f (InternalUnion i a) u@(InternalUnion j _) = guard (i == j) $> applyU @c (f (unsafeCoerce a)) u
{-# INLINABLE applyU2 #-}


instance (Hashable :$ l) => Hashable (Union l) where
    hashWithSalt salt = applyU @Hashable (hashWithSalt salt)
    {-# INLINABLE hashWithSalt #-}

instance (NFData :$ l) => NFData (Union l) where
    rnf = applyU @NFData rnf
    {-# INLINABLE rnf #-}


instance (Show :$ l) => Show (Union l) where
    showsPrec i u
        = showParen (i >= 10)
        $ showString "Union "
        . applyU @Show shows u
