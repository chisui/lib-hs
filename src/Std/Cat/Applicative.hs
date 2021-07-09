module Std.Cat.Applicative where

import "base" Data.Bool ( Bool(..)) 
import "base" Data.Coerce
import "base" GHC.IO qualified as Base
import "base" Data.Maybe qualified as Base
import "base" Control.Applicative qualified as Base
import "base" Data.Functor.Identity qualified as Base

import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Closed


class EndoFunctor cat f => CatPure cat f where
    catPure :: a `cat` f a
type Pure = CatPure HASK
pure :: Pure f => a -> f a
pure = catPure

class EndoFunctor cat f => CatAp cat f where
    (<*>) :: f (a `cat` b) -> f a `cat` f b
infixl 4 <*>
type Ap = CatAp HASK

infixl 4 *>
(*>) :: Ap f => f a -> f b -> f b
a *> b = (id <$ a) <*> b

infixl 4 <*
(<*) :: Ap f => f a -> f b -> f a
a <* b = (id <$ b) <*> a

class (Closed cat, EndoFunctor cat f) => CatLift2 cat f where
    lift2 :: (a `cat` Exp cat b c) -> (f a `cat` Exp cat (f b) (f c))
type Lift2 = CatLift2 HASK

class (CatPure cat f, CatAp cat f, CatLift2 cat f) => CatApplicative cat f
type Applicative = CatApplicative HASK


class CatApplicative cat f => CatAlternative cat f where
    catEmpty :: x `cat` f a 
    (<|>) :: f a `cat` Exp cat (f a) (f a)
type Alternative = CatAlternative HASK

empty :: Alternative f => f a
empty = catEmpty ()

guard :: Alternative f => Bool -> f ()
guard True  = pure ()
guard False = empty


instance Base.Applicative f => CatPure HASK (Basic1 f) where
    catPure :: forall a. a -> Basic1 f a
    catPure = coerce (Base.pure :: a -> f a)
instance Base.Applicative f => CatAp HASK (Basic1 f) where
    (<*>) :: forall a b. Basic1 f (a -> b) -> Basic1 f a -> Basic1 f b
    (<*>) = coerce ((Base.<*>) :: f (a -> b) -> f a -> f b)
instance Base.Applicative f => CatLift2 HASK (Basic1 f) where
    lift2 :: forall a b c. (a -> b -> c) -> Basic1 f a -> Basic1 f b -> Basic1 f c
    lift2 = coerce (Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c)
instance Base.Applicative f => CatApplicative HASK (Basic1 f)
instance Base.Alternative f => CatAlternative HASK (Basic1 f) where
    catEmpty :: forall a x. x -> Basic1 f a
    catEmpty _ = coerce (Base.empty :: f a)
    (<|>) :: forall a. Basic1 f a -> Basic1 f a -> Basic1 f a
    (<|>) = coerce ((Base.<|>) :: f a -> f a -> f a)

deriving via (Basic1 Base.Maybe) instance CatPure        HASK Base.Maybe
deriving via (Basic1 Base.Maybe) instance CatAp          HASK Base.Maybe
deriving via (Basic1 Base.Maybe) instance CatLift2       HASK Base.Maybe
deriving via (Basic1 Base.Maybe) instance CatApplicative HASK Base.Maybe
deriving via (Basic1 Base.Maybe) instance CatAlternative HASK Base.Maybe

deriving via (Basic1 Base.Identity) instance CatPure        HASK Base.Identity
deriving via (Basic1 Base.Identity) instance CatAp          HASK Base.Identity
deriving via (Basic1 Base.Identity) instance CatLift2       HASK Base.Identity
deriving via (Basic1 Base.Identity) instance CatApplicative HASK Base.Identity

deriving via (Basic1 ((->) a)) instance CatPure        HASK ((->) a)
deriving via (Basic1 ((->) a)) instance CatAp          HASK ((->) a)
deriving via (Basic1 ((->) a)) instance CatLift2       HASK ((->) a)
deriving via (Basic1 ((->) a)) instance CatApplicative HASK ((->) a)

deriving via (Basic1 Base.IO) instance CatPure        HASK Base.IO
deriving via (Basic1 Base.IO) instance CatAp          HASK Base.IO
deriving via (Basic1 Base.IO) instance CatLift2       HASK Base.IO
deriving via (Basic1 Base.IO) instance CatApplicative HASK Base.IO
