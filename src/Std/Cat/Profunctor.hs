module Std.Cat.Profunctor where

import "this" Std.Cat.Class
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.Dual
import "this" Std.Constraint


type CatProfunctor c = CatBifunctor Unconstraint (Dual c)
type EndoProfunctor cat = CatProfunctor cat cat cat
type Profunctor = EndoProfunctor HASK


catDimap :: CatProfunctor r s t p => b `r` a -> c `s` d -> (a `p` c) `t` (b `p` d)
catDimap f = catBimap (Dual f)

dimap :: Profunctor p => (b -> a) -> (a' -> b') -> p a a' -> p b b'
dimap = catDimap
