{-# LANGUAGE MagicHash #-}
module Std.Cat.Endo where

import "this" Std.Cat
import "this" Std.BinOp
import "this" Std.Group


newtype EndoT cat a = Endo
    { runEndo :: a `cat` a
    }
type Endo = EndoT HASK

instance Semigroupoid cat => CatFunctor (Iso cat) HASK (EndoT cat) where
    catMap = to coerce map'
      where
        map' :: forall a b. Iso cat a b -> a `cat` a -> b `cat` b
        map' f g = to f . g . from f

instance Semigroupoid cat => BinOp 'Add (EndoT cat a) where
    op# _ = to coerce ((.) :: a `cat` a -> a `cat` a -> a `cat` a)
instance Category cat => IdentityOp 'Add (EndoT cat a) where
    identity# _ = to coerce (id :: a `cat` a)
instance Semigroupoid cat => AssociativeOp 'Add (EndoT cat a)

instance Groupoid cat => BinOp 'Sub (EndoT cat a) where
    op# _ = to coerce (op' :: a `cat` a -> a `cat` a -> a `cat` a)
      where
        op' a b = a . invCat b

instance Groupoid cat => InverseOp 'Add (EndoT cat a) where
    type InvOp 'Add (EndoT cat a) = 'Sub
    inv# _ = to coerce (invCat :: a `cat` a -> a `cat` a)

instance Groupoid cat => InverseOp 'Sub (EndoT cat a) where
    type InvOp 'Sub (EndoT cat a) = 'Add
    inv# _ = to coerce (invCat :: a `cat` a -> a `cat` a)

