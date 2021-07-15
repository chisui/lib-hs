module Std.Cat.Applicative where

import "base" Data.Bool ( Bool(..) ) 
import "base" Data.Maybe ( Maybe(..), maybe )
import "base" Data.Coerce
import "base" Data.Kind
import "base" Control.Applicative qualified as Base
import "base" Data.Functor.Identity qualified as Base
import "base" Data.Semigroup qualified as Base ( Min(..), Max(..), (<>) )
import "base" Data.List.NonEmpty ( NonEmpty(..) )

import "this" Std.Type
import "this" Std.Cat.Class
import "this" Std.Cat.Functor
import "this" Std.Cat.Bifunctor
import "this" Std.Cat.Cartesian
import "this" Std.Cat.Cocartesian
import "this" Std.Cat.Limit
import "this" Std.Cat.Closed


class EndoFunctor' c cat f => CatPure' c (cat :: k -> k -> Type) f | f -> c where
    catPure :: c a => a `cat` f a
type Pure' c = CatPure' c HASK
type CatPure = CatPure' Unconstrained
type Pure = CatPure HASK
pure :: forall f a c. (Pure' c f, c a) => a -> f a
pure = catPure

class EndoFunctor' c cat f => CatAp' c cat f | f -> c where
    (<**>) :: (c a, c b) => f (a `cat` b) -> f a `cat` f b
infixl 4 <**>
type Ap' c = CatAp' c HASK
type CatAp = CatAp' Unconstrained
type Ap    = CatAp HASK

(<*>) :: (Ap' c f, c a, c b) => f (a -> b) -> f a -> f b
(<*>) = (<**>)
infixl 4 <*>

infixl 4 *>
(*>) :: (Ap' c f, c a, c b, c (b -> b)) => f a -> f b -> f b
a *> b = (id <$ a) <*> b

infixl 4 <*
(<*) :: (Ap' c f, c a, c b, c (b -> a)) => f a -> f b -> f a
a <* b = (const <$> a) <*> b

class (Closed cat, EndoFunctor' c cat f) => CatLift2' c cat f | f -> c where
    lift2 :: (c a, c b, c d) => (a `cat` Exp cat b d) -> (f a `cat` Exp cat (f b) (f d))
type Lift2' c = CatLift2' c HASK
type CatLift2 = CatLift2' Unconstrained
type Lift2    = CatLift2 HASK

class (Category cat, EndoFunctor' c cat f, CatPure' c cat f, CatAp' c cat f, CatLift2' c cat f) => CatApplicative' c cat f | f -> c
type Applicative' c = CatApplicative' c HASK
type CatApplicative = CatApplicative' Unconstrained
type Applicative    = CatApplicative HASK

class CatEmpty' c cat f | f -> c where
    catEmpty :: c a => x `cat` f a
type Empty' c = CatEmpty' c HASK
type CatEmpty = CatEmpty' Unconstrained
type Empty    = CatEmpty HASK
class Cartesian cat => CatCombine' c cat f | f -> c where
    combine :: c a => Product cat (f a) (f a) `cat` (f a)
    infixl 3 `combine`
type Combine' c = CatCombine' c HASK
type CatCombine = CatCombine' Unconstrained
type Combine    = CatCombine HASK

(<|>) :: (Combine' c f, c a) => f a -> f a -> f a
(<|>) = curry combine
infixl 3 <|>

class (CatApplicative' c cat f, CatEmpty' c cat f, CatCombine' c cat f) => CatAlternative' c cat f | f -> c
type Alternative' c = CatAlternative' c HASK
type CatAlternative = CatAlternative' Unconstrained
type Alternative    = CatAlternative HASK

catCons :: forall cat f a c. (Closed cat, CatPure' c cat f, CatCombine' c cat f, c a) => a `cat` Exp cat (f a) (f a)
catCons = curry combine . catPure

cons :: (Pure' c f, Combine' c f, c a) => a -> f a -> f a
cons = catCons

empty :: (Empty' c f, c a) => f a
empty = catEmpty ()

guard :: (Alternative' c f, c ()) => Bool -> f ()
guard True  = pure ()
guard False = empty

catRepeat :: forall cat f a c. (CatPure' c cat f, CatCombine' c cat f, c a) => a `cat` f a
catRepeat = combine . (catPure &&& catRepeat)

repeat :: forall f a c. (Pure' c f, Combine' c f, c a) => a -> f a
repeat = catRepeat

catCycle :: forall cat f a c. (CatCombine' c cat f, c a) => f a `cat` f a
catCycle = combine . (id &&& catCycle)

cycle :: forall f a c. (Combine' c f, c a) => f a -> f a
cycle = catCycle

catUnfoldr :: forall cat f a b. (Cartesian cat, Cocartesian cat, CatAlternative cat f)
           => (b `cat` Coproduct cat (Initial cat) (Product cat a b)) 
           -> b `cat` f a
catUnfoldr f = (catEmpty ||| (combine . ((catPure :: a `cat` f a) *** catUnfoldr f))) . f

unfoldr :: Alternative f => (b -> Maybe (a, b)) -> b -> f a
unfoldr f = maybe empty (combine . (pure *** unfoldr f)) . f

instance Base.Applicative f => CatPure' Unconstrained HASK (Basic1 f) where
    catPure :: forall a. a -> Basic1 f a
    catPure = coerce (Base.pure :: a -> f a)
instance Base.Applicative f => CatAp' Unconstrained HASK (Basic1 f) where
    (<**>) :: forall a b. Basic1 f (a -> b) -> Basic1 f a -> Basic1 f b
    (<**>) = coerce ((Base.<*>) :: f (a -> b) -> f a -> f b)
instance Base.Applicative f => CatLift2' Unconstrained HASK (Basic1 f) where
    lift2 :: forall a b c. (a -> b -> c) -> Basic1 f a -> Basic1 f b -> Basic1 f c
    lift2 = coerce (Base.liftA2 :: (a -> b -> c) -> f a -> f b -> f c)
instance Base.Applicative f => CatApplicative' Unconstrained HASK (Basic1 f)
instance Base.Alternative f => CatEmpty' Unconstrained HASK (Basic1 f) where
    catEmpty :: forall a x. x -> Basic1 f a
    catEmpty _ = coerce (Base.empty :: f a)
instance Base.Alternative f => CatCombine' Unconstrained HASK (Basic1 f) where
    combine :: forall a. (Basic1 f a, Basic1 f a) -> Basic1 f a
    combine = coerce (uncurry (Base.<|>) :: (f a, f a) -> f a)
instance Base.Alternative f => CatAlternative' Unconstrained HASK (Basic1 f)

deriving via (Basic1 Base.Identity) instance CatPure'        Unconstrained HASK Base.Identity
deriving via (Basic1 Base.Identity) instance CatAp'          Unconstrained HASK Base.Identity
deriving via (Basic1 Base.Identity) instance CatLift2'       Unconstrained HASK Base.Identity
deriving via (Basic1 Base.Identity) instance CatApplicative' Unconstrained HASK Base.Identity

deriving via (Basic1 ((->) a)) instance CatPure'        Unconstrained HASK ((->) a)
deriving via (Basic1 ((->) a)) instance CatAp'          Unconstrained HASK ((->) a)
deriving via (Basic1 ((->) a)) instance CatLift2'       Unconstrained HASK ((->) a)
deriving via (Basic1 ((->) a)) instance CatApplicative' Unconstrained HASK ((->) a)

deriving via (Basic1 []) instance CatPure'        Unconstrained HASK []
deriving via (Basic1 []) instance CatAp'          Unconstrained HASK []
deriving via (Basic1 []) instance CatLift2'       Unconstrained HASK []
deriving via (Basic1 []) instance CatApplicative' Unconstrained HASK []
deriving via (Basic1 []) instance CatEmpty'       Unconstrained HASK []
deriving via (Basic1 []) instance CatCombine'     Unconstrained HASK []
deriving via (Basic1 []) instance CatAlternative' Unconstrained HASK []

deriving via (Basic1 NonEmpty) instance CatPure'        Unconstrained HASK NonEmpty
deriving via (Basic1 NonEmpty) instance CatAp'          Unconstrained HASK NonEmpty
deriving via (Basic1 NonEmpty) instance CatLift2'       Unconstrained HASK NonEmpty
deriving via (Basic1 NonEmpty) instance CatApplicative' Unconstrained HASK NonEmpty
instance CatCombine' Unconstrained HASK NonEmpty where combine = uncurry (Base.<>)

deriving via (Basic1 Base.Min) instance CatPure'        Unconstrained HASK Base.Min
deriving via (Basic1 Base.Min) instance CatAp'          Unconstrained HASK Base.Min
deriving via (Basic1 Base.Min) instance CatLift2'       Unconstrained HASK Base.Min
deriving via (Basic1 Base.Min) instance CatApplicative' Unconstrained HASK Base.Min

deriving via (Basic1 Base.Max) instance CatPure'        Unconstrained HASK Base.Max
deriving via (Basic1 Base.Max) instance CatAp'          Unconstrained HASK Base.Max
deriving via (Basic1 Base.Max) instance CatLift2'       Unconstrained HASK Base.Max
deriving via (Basic1 Base.Max) instance CatApplicative' Unconstrained HASK Base.Max
