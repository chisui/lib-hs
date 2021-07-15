{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Class
    ( HASK
    , Semigroupoid(..), (∘)
    , CatId(..), ($)
    , Category
    , Groupoid(..)
    , Basic2(..)
    , type (:~:)(..)
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
class Semigroupoid cat where
    (.) :: b `cat` c -> a `cat` b -> a `cat` c
infixr 9 .

(∘) :: Semigroupoid cat => b `cat` c -> a `cat` b -> a `cat` c
(∘) = (.)
infixr 9 ∘

-- | A type class that only contains a categorical identity morphism @id@ for all objects
-- in that category.
class CatId cat where
    id :: a `cat` a

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
class (Semigroupoid cat, CatId cat) => Category cat

-- | A groupoid is a category where every morphism is invertible.
-- The inverse morphism has to adhere to the following:
--
-- [Right identity] @f '.' invCat f  =  id@
-- [Left identity]  @invCat f '.' f  =  id@
--
class Category cat => Groupoid cat where
    invCat :: a `cat` b -> b `cat` a

-- | A newtype to be used for deriving instances for data types with
-- two type arguments. Example:
--
-- @
-- deriving via (Basic2 HASK) instance Semigroupoid HASK
-- @
--
newtype Basic2 f a b = Basic2 (f a b)

instance Base.Category cat => Semigroupoid (Basic2 cat) where 
    (.) ::forall a b c. Basic2 cat b c -> Basic2 cat a b -> Basic2 cat a c
    (.) = Base.coerce ((Base..) :: b `cat` c -> a `cat` b -> a `cat` c)
instance Base.Category cat => CatId (Basic2 cat) where
    id :: forall a. Basic2 cat a a
    id = Base.coerce (Base.id :: cat a a)
instance Base.Category cat => Category (Basic2 cat)

deriving via (Basic2 HASK) instance Semigroupoid HASK
deriving via (Basic2 HASK) instance CatId HASK
deriving via (Basic2 HASK) instance Category HASK

instance Semigroupoid Const where _ . Const a = Const a
instance Semigroupoid (,) where (_,c) . (a,_) = (a, c)

instance Semigroupoid (:~:) where Refl . Refl = Refl
instance CatId (:~:) where id = Refl
instance Category (:~:)
