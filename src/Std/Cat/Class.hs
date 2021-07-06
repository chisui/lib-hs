{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Class where

import "base" Data.Function qualified as Base


type HASK = (->)

class Semigroupoid cat where
    (.) :: b `cat` c -> a `cat` b -> a `cat` c


class CatId cat where
    id :: cat a a

($) :: CatId cat => a `cat` a
($) = id


class (Semigroupoid cat, CatId cat) => Category cat


class Category cat => Groupoid cat where
    invCat :: a `cat` b -> b `cat` a

instance Semigroupoid HASK where (.) = (Base..)
instance CatId        HASK where id = Base.id
instance Category     HASK
