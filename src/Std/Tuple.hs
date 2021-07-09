{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Tuple where

import "this" Std.Type
import "this" Std.Cat
import "this" Std.HList


type family ListToTuple (l :: [Type]) = (t :: Type) | t -> l where
    ListToTuple '[] = ()
    ListToTuple '[a] = Identity a
    ListToTuple '[a, b] = (a, b)
    ListToTuple '[a, b, c] = (a, b, c)
    ListToTuple '[a, b, c, d] = (a, b, c, d)
    ListToTuple '[a, b, c, d, e] = (a, b, c, d, e)
    ListToTuple '[a, b, c, d, e, f] = (a, b, c, d, e, f)
    ListToTuple '[a, b, c, d, e, f, g] = (a, b, c, d, e, f, g)
    ListToTuple '[a, b, c, d, e, f, g, h] = (a, b, c, d, e, f, g, h)

type family TupleToList (t :: Type) = (l :: [Type]) | l -> t where
    TupleToList () = '[]
    TupleToList (Identity a) = '[a]
    TupleToList (a, b) = '[a, b]
    TupleToList (a, b, c) = '[a, b, c]
    TupleToList (a, b, c, d) = '[a, b, c, d]
    TupleToList (a, b, c, d, e) = '[a, b, c, d, e]
    TupleToList (a, b, c, d, e, f) = '[a, b, c, d, e, f]
    TupleToList (a, b, c, d, e, f, g) = '[a, b, c, d, e, f, g]
    TupleToList (a, b, c, d, e, f, g, h) = '[a, b, c, d, e, f, g, h]

instance IsTuple l t => CatIsomorphic HASK (HList l) t where
    catIso = tuple

class (l ~ TupleToList t, ListToTuple l ~ t) => IsTuple (l :: [Type]) (t :: Type) | l -> t, t -> l where
    tuple :: HList l <-> t

instance IsTuple '[] () where
    tuple = const () :<-> const HNil

instance IsTuple '[a] (Identity a) where
    tuple = (\(a ::: HNil) -> Identity a)
       :<-> (\(Identity a) -> a ::: HNil)

instance IsTuple '[a, b] (a, b) where
    tuple = (\(a ::: b ::: HNil) -> (a, b))
       :<-> (\(a, b) -> a ::: b ::: HNil)

instance IsTuple '[a, b, c] (a, b, c) where
    tuple = (\(a ::: b ::: c ::: HNil) -> (a, b, c))
       :<-> (\(a, b, c) -> a ::: b ::: c ::: HNil)

instance IsTuple '[a, b, c, d] (a, b, c, d) where
    tuple = (\(a ::: b ::: c ::: d ::: HNil) -> (a, b, c, d))
       :<-> (\(a, b, c, d) -> a ::: b ::: c ::: d ::: HNil)

instance IsTuple '[a, b, c, d, e] (a, b, c, d, e) where
    tuple = (\(a ::: b ::: c ::: d ::: e ::: HNil) -> (a, b, c, d, e))
       :<-> (\(a, b, c, d, e) -> a ::: b ::: c ::: d ::: e ::: HNil)

instance IsTuple '[a, b, c, d, e, f] (a, b, c, d, e, f) where
    tuple = (\(a ::: b ::: c ::: d ::: e ::: f ::: HNil) -> (a, b, c, d, e, f))
       :<-> (\(a, b, c, d, e, f) -> a ::: b ::: c ::: d ::: e ::: f ::: HNil)

instance IsTuple '[a, b, c, d, e, f, g] (a, b, c, d, e, f, g) where
    tuple = (\(a ::: b ::: c ::: d ::: e ::: f ::: g ::: HNil) -> (a, b, c, d, e, f, g))
       :<-> (\(a, b, c, d, e, f, g) -> a ::: b ::: c ::: d ::: e ::: f ::: g ::: HNil)
