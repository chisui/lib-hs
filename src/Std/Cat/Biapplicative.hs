module Std.Cat.Biapplicative where

import "this" Std.Type
import "this" Std.Cat.Class
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Closed


class (Cartesian cat, CatBifunctor' c0 c1 cat cat cat p) => CatBipure' c0 c1 cat p | cat p -> c0 c1 where
    catBipure' :: (c0 a, c1 b) => Product cat a b `cat` p a b
type CatBipure     = CatBipure' Unconstrained Unconstrained
type Bipure' c0 c1 = CatBipure' c0 c1 HASK
type Bipure        = CatBipure HASK
catBipure :: CatBipure cat p => Product cat a b `cat` p a b
catBipure = catBipure'
bipure' :: (Bipure' c0 c1 p, c0 a, c1 b) => a -> b -> p a b
bipure' = curry catBipure'
bipure :: Bipure p => a -> b -> p a b
bipure = curry catBipure'

class (Cartesian cat, CatBifunctor' c0 c1 cat cat cat p) => CatBiap' c0 c1 cat p | cat p -> c0 c1 where
    catBiap' :: (c0 al, c0 bl, c1 ar, c1 br) => (al `cat` bl) `p` (ar `cat` br ) -> (al `p` ar) `cat` (bl `p` br)
type CatBiap     = CatBiap' Unconstrained Unconstrained
type Biap' c0 c1 = CatBiap' c0 c1 HASK
type Biap       = CatBiap HASK
catBiap :: CatBiap cat p => (al `cat` bl) `p` (ar `cat` br) -> (al `p` ar) `cat` (bl `p` br)
catBiap = catBiap'
(<<*>>) :: (Biap' c0 c1 p, c0 al, c0 bl, c1 ar, c1 br) => (al -> bl) `p` (ar -> br) -> al `p` ar -> bl `p` br
(<<*>>) = catBiap'
infixl 4 <<*>>
(*>>) :: Biap p => al `p` ar -> bl `p` br -> bl `p` br
a *>> b = bimap (const id) (const id) a <<*>> b
infixl 4 *>>
(<<*) :: Biap p => bl `p` br -> al `p` ar -> bl `p` br
a <<* b = bimap const const a <<*>> b
infixl 4 <<*


class (Closed cat, CatBifunctor' c0 c1 cat cat cat p) => CatBilift2' c0 c1 cat p | cat p -> c0 c1 where
    catBilift2' :: (c0 al, c0 bl, c0 cl, c1 ar, c1 br, c1 cr)
                => (al `cat` Exp cat bl cl) -> (ar `cat` Exp cat br cr)
                -> ((al `p` ar) `cat` Exp cat (bl `p` br) (cl `p` cr))
type CatBilift2     = CatBilift2' Unconstrained Unconstrained
type Bilift2' c0 c1 = CatBilift2' c0 c1 HASK
type Bilift2       = CatBilift2 HASK
catBilift2 :: CatBilift2 cat p 
           => (al `cat` Exp cat bl cl) -> (ar `cat` Exp cat br cr)
           -> ((al `p` ar) `cat` Exp cat (bl `p` br) (cl `p` cr))
catBilift2 = catBilift2'
bilift2' :: (Bilift2' c0 c1 p, c0 al, c0 bl, c0 cl, c1 ar, c1 br, c1 cr)
         => (al -> bl -> cl) -> (ar -> br -> cr)
         -> (al `p` ar) -> (bl `p` br) -> (cl `p` cr)
bilift2' = catBilift2'
bilift2 :: Bilift2 p
         => (al -> bl -> cl) -> (ar -> br -> cr)
         -> (al `p` ar) -> (bl `p` br) -> (cl `p` cr)
bilift2 = catBilift2'

class (CatBipure' c0 c1 cat p, CatBiap' c0 c1 cat p, CatBilift2' c0 c1 cat p) => CatBiapplicative'  c0 c1 cat p | cat p -> c0 c1
type CatBiapplicative     = CatBiapplicative' Unconstrained Unconstrained
type Biapplicative' c0 c1 = CatBiapplicative' c0 c1 HASK
type Biapplicative        = CatBiapplicative HASK


instance CatBipure' Unconstrained Unconstrained HASK (,) where
    catBipure' = id
instance CatBiap' Unconstrained Unconstrained HASK (,) where
    catBiap' (f, g) (a, b) = (f a, g b)
instance CatBilift2' Unconstrained Unconstrained HASK (,) where
    catBilift2' f g (af, ag) (bf, bg) = (f af bf, g ag bg)
