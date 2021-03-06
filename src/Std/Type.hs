{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Type
    ( module Exp
    , Unconstrained
    , ElemIndex, ElemIndices
    , Replicate
    , Element, type (:<)
    , Map
    , type (!)
    , AllImplement, type (<$>)
    , type (==)(..)
    , type (===), type (==>)
    , Concat
    , Length
    , MinT, MaxT
    ) where

import "base" Prelude ( Bool(..), Ordering(..) )
import "base" Data.Type.Equality as Exp (type (:~:) (..))
import "base" Data.Typeable as Exp (Typeable)
import "base" Data.Type.Bool ( type (&&) )
import "base" Data.Kind as Exp ( Type, Constraint )
import "base" GHC.Exts as Exp ( Any )
import "base" GHC.TypeLits as Exp ( Symbol, KnownSymbol, Nat, KnownNat, CmpNat, CmpSymbol, type (+), type(-) )

import "ghc-prim" GHC.Prim as Exp ( proxy#, Proxy# )

import "this" Std.TypeError


class Unconstrained (a :: k)
instance Unconstrained a

type family Length (l :: [k]) :: Nat where
    Length '[]       = 0
    Length (a ': as) = Length as + 1

type family (!) (l :: [k]) (i :: Nat) :: k where
    (a ': _ ) ! 0 = a
    (_ ': as) ! n = as ! (n - 1)

type family Replicate (n :: Nat) (a :: k) :: [k] where
    Replicate 0 _ = '[]
    Replicate n a = Replicate (n - 1) a

class    (KnownNat (ElemIndex t l), l ! ElemIndex t l ~ t) => Element l t
instance (KnownNat (ElemIndex t l), l ! ElemIndex t l ~ t) => Element l t

type (t :< l) = Element l t
infixr 5 :<

type family ElemIndices a b where
    ElemIndices '[] _ = '[]
    ElemIndices (a ': as) b = ElemIndex a b ': ElemIndices as b


type ElemIndex (t :: k) (l :: [k]) = ElemIndex' t l 0 l

type family ElemIndex' (t :: k) (orig :: [k]) (i :: Nat) (l :: [k]) :: Nat where
    ElemIndex' t orig i (t ': _)  = i
    ElemIndex' t orig i (_ ': ts) = ElemIndex' t orig (i + 1) ts
    ElemIndex' t orig _ '[] = [typeError|
        '$t\' is not a member of the type-level list
        $orig
    |]

type family Map (f :: k -> k1) (l :: [k]) = (l' :: [k1]) where
    Map _ '[] = '[]
    Map f (a ': as) = f a ': Map f as

class AllImplement l c => (<$>) c l
instance AllImplement l c => (<$>) c l

type family AllImplement (l :: [k]) (c :: k -> Constraint) :: Constraint where
    AllImplement '[] _ = ()
    AllImplement (a ': as) c = (c a, AllImplement as c)


class    (forall x. c0 x => c1 x) => (c0 :: k -> Constraint) ==> (c1 :: k -> Constraint)
instance (forall x. c0 x => c1 x) => (c0 :: k -> Constraint) ==> (c1 :: k -> Constraint)


infix 4 ===
type family (===) (a :: k0) (b :: k1) :: Bool where
    (f a) === (g b) = (f === g) && (a === b)
    a === a = 'True
    _ === _ = 'False


class (==) a b where eq :: a :~: b
instance (a ~ b) => (==) a b where eq = Refl

type family Concat a b where
    Concat '[] l = l
    Concat (a ': as) l = a ': Concat as l

class OrdKind k where
    type Compare (a :: k) (b :: k) :: Ordering
instance OrdKind Nat where
    type Compare a b = CmpNat a b
instance OrdKind Symbol where
    type Compare a b = CmpSymbol a b

type MinT (a :: k) (b :: k) = MinFromOrdering (Compare a b) a b

type MaxT (a :: k) (b :: k) = MinFromOrdering (Compare a b) b a

type family MinFromOrdering (o :: Ordering) (a :: k) (b :: k) :: k where
    MinFromOrdering 'GT _ b = b
    MinFromOrdering _   a _ = a
