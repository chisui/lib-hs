module Std.Cat.Profunctor where

import "this" Std.Cat.Class
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.Op
import "this" Std.Type


type CatProfunctor'  c0 c1 cat = CatBifunctor' c0 c1 (Op cat)
type CatProfunctor         cat = CatProfunctor' Unconstrained Unconstrained cat
type EndoProfunctor' c0 c1 cat = CatProfunctor' c0 c1 cat cat cat
type EndoProfunctor        cat = CatProfunctor cat cat cat
type Profunctor'     c0 c1     = EndoProfunctor' c0 c1 HASK
type Profunctor                = EndoProfunctor HASK


catDimap' :: (CatProfunctor' c0 c1 r s t p, c0 a, c0 b, c1 c, c1 d)
         => b `r` a -> c `s` d -> (a `p` c) `t` (b `p` d)
catDimap' = catBimap' . Op

dimap' :: (Profunctor' c0 c1 p, c0 a, c0 b, c1 a', c1 b')
       => (b -> a) -> (a' -> b') -> p a a' -> p b b'
dimap' = catDimap'

catDimap :: CatProfunctor r s t p => b `r` a -> c `s` d -> (a `p` c) `t` (b `p` d)
catDimap = catDimap'

dimap :: Profunctor p => (b -> a) -> (a' -> b') -> p a a' -> p b b'
dimap = catDimap'
