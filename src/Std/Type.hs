{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Type
    ( module Exp
    , Unconstrained
    , ElemIndex, ElemIndices
    , Element, type (:<)
    , Map
    , type (!)
    , AllImplement, type (<$>)
    , type (==)(..)
    , type (===)
    , Concat
    , Length
    ) where

import "base" Prelude ( Bool(..) )
import "base" Data.Type.Equality as Exp (type (:~:) (Refl))
import "base" Data.Type.Bool ( type (&&) )
import "base" Data.Kind as Exp ( Type, Constraint )
import "base" GHC.Exts as Exp ( Any )
import "base" GHC.TypeLits as Exp

import "ghc-prim" GHC.Prim as Exp ( proxy#, Proxy# )

import "this" Std.TypeError


class Unconstrained (a :: k)
instance Unconstrained a

type family Length (l :: [k]) :: Nat where
    Length '[] = 0
    Length (a ': as) = Length as + 1

type family (!) (l :: [k]) (i :: Nat) :: k where
    (a ': _) ! 0 = a
    (_ ': as) ! n = as ! (n - 1)


class (KnownNat (ElemIndex t l), l ! ElemIndex t l ~ t) => Element l t
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
