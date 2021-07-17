{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Class
    ( HASK
    , Semigroupoid'(..), Semigroupoid, (∘)
    , CatId'(..), CatId, ($)
    , Category', Category
    , Groupoid'(..), Groupoid
    , Basic2(..)
    ) where

import "base" Data.Coerce qualified as Base
import "base" Control.Category qualified as Base
import "base" Data.Functor.Const

import "this" Std.Type


-- | @HASK@ is the category of Haskell types and functions between them.
-- Since categories are represented by a typeconstructor for their
-- morphisms, this type is equivalent to @(->)@.
type HASK = (->)

-- | A @Semigroupoid is a category without identity.
-- The composition and associativity still has to hold:
--
-- [Composition]: ∀f,g∈C ∃(f . g)∈C, Or in other wirds @(.)@ has to be a total function.
-- [associativity]: @h '.' (g '.' f) = (h '.' g) '.' f@
--
class Semigroupoid' c0 cat | cat -> c0 where
    (.) :: (c0 a, c0 b, c0 c) => b `cat` c -> a `cat` b -> a `cat` c
infixr 9 .
type Semigroupoid = Semigroupoid' Unconstrained

(∘) :: (Semigroupoid' c0 cat, c0 a, c0 b, c0 c) => b `cat` c -> a `cat` b -> a `cat` c
(∘) = (.)
infixr 9 ∘

-- | A type class that only contains a categorical identity morphism @id@ for all objects
-- in that category.
class CatId' c0 cat | cat -> c0 where
    id :: c0 a => a `cat` a
type CatId = CatId' Unconstrained

($) :: CatId cat => a `cat` a
($) = id
infixr 0 $

-- | A class for categories. Instances should satisfy the laws
--
-- [Right identity] @f '.' 'id'  =  f@
-- [Left identity]  @'id' '.' f  =  f@
-- [Associativity]  @f '.' (g '.' h)  =  (f '.' g) '.' h@
--
-- The category is represented by a type constructor constructing its
-- morphism @cat :: k -> k -> Type@ where @k@ is the kind of objects
-- in the category.
--
class (Semigroupoid' c0 cat, CatId' c0 cat) => Category' c0 cat | cat -> c0
type Category = Category' Unconstrained

-- | A groupoid is a category where every morphism is invertible.
-- The inverse morphism has to adhere to the following:
--
-- [Right identity] @f '.' catInv f  =  id@
-- [Left identity]  @catInv f '.' f  =  id@
--
class Category' c0 cat => Groupoid' c0 cat | cat -> c0 where
    catInv :: (c0 a, c0 b) => a `cat` b -> b `cat` a
type Groupoid = Groupoid' Unconstrained

-- | A newtype to be used for deriving instances for data types with
-- two type arguments. Example:
--
-- @
-- deriving via (Basic2 HASK) instance Semigroupoid HASK
-- @
--
newtype Basic2 f a b = Basic2 (f a b)

instance Base.Category cat => Semigroupoid' Unconstrained (Basic2 cat) where 
    (.) ::forall a b c. Basic2 cat b c -> Basic2 cat a b -> Basic2 cat a c
    (.) = Base.coerce ((Base..) :: b `cat` c -> a `cat` b -> a `cat` c)
instance Base.Category cat => CatId' Unconstrained (Basic2 cat) where
    id :: forall a. Basic2 cat a a
    id = Base.coerce (Base.id :: cat a a)
instance Base.Category cat => Category' Unconstrained (Basic2 cat)

deriving via (Basic2 HASK) instance Semigroupoid' Unconstrained HASK
deriving via (Basic2 HASK) instance CatId'        Unconstrained HASK
deriving via (Basic2 HASK) instance Category'     Unconstrained HASK

instance Semigroupoid' Unconstrained Const where _ . Const a = Const a
instance Semigroupoid' Unconstrained (,) where (_,c) . (a,_) = (a, c)
