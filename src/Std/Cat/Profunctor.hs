module Std.Cat.Profunctor where

import "this" Std.Cat.Class
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.Op
import "this" Std.Type


type CatProfunctor'  c cat = CatBifunctor' c (Op cat)
type CatProfunctor     cat = CatProfunctor' Unconstrained cat
type EndoProfunctor' c cat = CatProfunctor' c cat cat cat
type EndoProfunctor    cat = CatProfunctor cat cat cat
type Profunctor'     c     = EndoProfunctor' c HASK
type Profunctor            = EndoProfunctor HASK


catDimap' :: (CatProfunctor' c0 r s t p, c0 a, c0 b, c0 c, c0 d)
         => b `r` a -> c `s` d -> (a `p` c) `t` (b `p` d)
catDimap' = catBimap' . Op

dimap' :: (Profunctor' c0 p, c0 a, c0 b, c0 a', c0 b')
       => (b -> a) -> (a' -> b') -> p a a' -> p b b'
dimap' = catDimap'

catDimap :: CatProfunctor r s t p => b `r` a -> c `s` d -> (a `p` c) `t` (b `p` d)
catDimap = catDimap'

dimap :: Profunctor p => (b -> a) -> (a' -> b') -> p a a' -> p b b'
dimap = catDimap'
