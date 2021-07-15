{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
module Std.Cat.Endo
    ( EndoMorph(..), Endo
    ) where

import "this" Std.Type
import "this" Std.Ord
import "this" Std.Debug
import "this" Std.Cat
import "this" Std.BinOp
import "this" Std.Group


newtype EndoMorph cat a = Endo
    { appEndo :: a `cat` a
    }
type Endo = EndoMorph HASK

deriving stock   instance Show   (a `cat` a) => Show   (EndoMorph cat a)
deriving newtype instance Eq     (a `cat` a) => Eq     (EndoMorph cat a)
deriving newtype instance Ord' t (a `cat` a) => Ord' t (EndoMorph cat a)


instance Category cat => CatFunctor' Unconstrained (Iso cat) HASK (EndoMorph cat) where
    catMap = to coerce map'
      where
        map' :: forall a b. Iso cat a b -> a `cat` a -> b `cat` b
        map' f g = to f . g . from f

instance Semigroupoid cat => BinOp 'Canonic (EndoMorph cat a) where
    op# _ = to coerce ((.) :: a `cat` a -> a `cat` a -> a `cat` a)
instance Category cat => IdentityOp 'Canonic (EndoMorph cat a) where
    identity# _ = to coerce (id :: a `cat` a)
instance Semigroupoid cat => AssociativeOp 'Canonic (EndoMorph cat a)

instance Groupoid cat => BinOp 'InvCanonic (EndoMorph cat a) where
    op# _ = to coerce (op' :: a `cat` a -> a `cat` a -> a `cat` a)
      where
        op' a b = a . invCat b

instance Groupoid cat => InverseOp 'Canonic (EndoMorph cat a) where
    type InvOp 'Canonic (EndoMorph cat a) = 'InvCanonic
    inv# _ = to coerce (invCat :: a `cat` a -> a `cat` a)

instance Groupoid cat => InverseOp 'InvCanonic (EndoMorph cat a) where
    type InvOp 'InvCanonic (EndoMorph cat a) = 'Canonic
    inv# _ = to coerce (invCat :: a `cat` a -> a `cat` a)
