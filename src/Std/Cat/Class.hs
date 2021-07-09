{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Class where

import "base" Data.Coerce qualified as Base
import "base" Control.Category qualified as Base
import "base" Data.Functor.Const

import "this" Std.Type


type HASK = (->)

class Semigroupoid cat where
    (.) :: b `cat` c -> a `cat` b -> a `cat` c
infixr 9 .

class CatId cat where
    id :: cat a a

($) :: CatId cat => a `cat` a
($) = id
infixr 0 $

class (Semigroupoid cat, CatId cat) => Category cat


class Category cat => Groupoid cat where
    invCat :: a `cat` b -> b `cat` a


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
