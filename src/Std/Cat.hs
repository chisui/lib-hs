{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Cat
    ( HASK
    , CatFunctor(..), EndoFunctor, Functor
    , CatPure(..), Pure
    , CatAp(..), Ap
    , CatLift2(..), Lift2
    , CatApplicative, Applicative
    , CatBind(..), Bind, (>>=)
    , CatJoin(..), Join
    , CatMonad, Monad
    , Identity(..)
    , Semigroupoid(..)
    , CatId(..)
    , Category
    , Closed(..)
    , CatDistributive(..), Distributive
    , Iso(..), type (<->), type (<~>)
    , Dual(..), type (<--), type (<~)
    , NT(..), type (~>)
    , NT1(..), type (~~>)
    , CatKleisli(..), Kleisli
    , Cartesian(..), Cocartesian(..)
    , CatAssociative(..)
    , Prod1(..), Product1, Coproduct1
    , CatLeftFunctor(..), EndoLeftFunctor, LeftFunctor
    , CatRightFunctor(..), EndoRightFunctor, RightFunctor
    , CatBifunctor(..), EndoBifunctor, Bifunctor
    , CatExtract(..), Extract, CatExtend(..), Extend, CatDuplicate(..), Duplicate, CatComonad, Comonad
    , ($)
    ) where

import "base" Data.Either
import "base" Data.Void
import "base" Data.Kind ( Type )
import "base" Data.Coerce ( coerce )
import "base" Data.Maybe ( Maybe )
import "base" Data.Function qualified as Base
import "base" Data.Functor qualified as Base
import "base" Data.Functor.Identity ( Identity(..) )
import "base" Data.Functor.Const ( Const(..) )
import "base" Control.Applicative qualified as Base
import "base" Control.Monad qualified as Base


type HASK = (->)

class CatFunctor (c0 :: k0 -> k0 -> Type) (c1 :: k1 -> k1 -> Type) (f :: k0 -> k1) where
    map, (<$>) :: a `c0` b -> f a `c1` f b
    {-# MINIMAL map | (<$>) #-}
    map = (<$>)
    (<$>) = map

type EndoFunctor c = CatFunctor c c
type Functor = EndoFunctor HASK

class EndoFunctor cat f => CatPure cat f where
    pure :: a `cat` f a
type Pure = CatPure HASK

class EndoFunctor cat f => CatExtract cat f where
    extract :: f a `cat` a
type Extract = CatExtract HASK

class CatBind cat m where
    (=<<) :: a `cat` m b -> m a `cat` m b
type Bind = CatBind HASK

class CatExtend cat m where
    (<<=) :: m a `cat` b -> m a `cat` m b
type Extend = CatExtend HASK

(>>=) :: Bind m => m a -> (a -> m b) -> m b
m >>= f = f =<< m

(<=<) :: CatMonad cat m => b `cat` m c -> a `cat` m b -> a `cat` m c
f <=< g = (=<<) f . (=<<) g . pure

class EndoFunctor cat f => CatJoin cat f where
    join :: f (f a) `cat` f a
type Join = CatJoin HASK

class EndoFunctor cat f => CatDuplicate cat f where
    duplicate :: f a `cat` f (f a)
type Duplicate = CatDuplicate HASK

class CatDistributive cat g where
    distribute :: EndoFunctor cat f => f (g a) `cat` g (f a)
type Distributive = CatDistributive HASK

class CatAp cat f where
    (<*>) :: f (a `cat` b) -> f a `cat` f b
type Ap = CatAp HASK

class Closed cat => CatLift2 cat f where
    lift2 :: (a `cat` Exp cat b c) -> (f a `cat` Exp cat (f b) (f c))
type Lift2 = CatLift2 HASK

class (EndoFunctor cat f, CatPure cat f, CatAp cat f, CatLift2 cat f) => CatApplicative cat f
type Applicative = CatApplicative HASK
class (Category cat, EndoFunctor cat f, CatPure cat f, CatBind cat f, CatJoin cat f) => CatMonad (cat :: k -> k -> Type) f
type Monad = CatMonad HASK
class (Category cat, CatExtract cat f, CatExtend cat f, CatDuplicate cat f) => CatComonad cat f
type Comonad = CatComonad HASK


-- instances

instance CatFunctor      HASK HASK Identity where map = coerce
instance CatPure         HASK      Identity where pure = coerce
instance CatAp           HASK      Identity where (<*>) = coerce
instance CatLift2        HASK      Identity where lift2 = coerce
instance CatBind         HASK      Identity where (=<<) = coerce
instance CatJoin         HASK      Identity where join = coerce
instance CatApplicative  HASK      Identity
instance CatMonad        HASK      Identity
instance CatExtract      HASK      Identity where extract = coerce
instance CatExtend       HASK      Identity where (<<=) = coerce
instance CatDuplicate    HASK      Identity where duplicate = coerce
instance CatComonad      HASK      Identity
instance CatDistributive HASK ((->) a) where
    distribute = flip (map . flip id)

instance CatFunctor     HASK HASK Maybe where map = Base.fmap
instance CatPure        HASK      Maybe where pure = Base.pure
instance CatAp          HASK      Maybe where (<*>) = (Base.<*>)
instance CatLift2       HASK      Maybe where lift2 = Base.liftA2
instance CatBind        HASK      Maybe where (=<<) = (Base.=<<)
instance CatJoin        HASK      Maybe where join = Base.join
instance CatApplicative HASK      Maybe
instance CatMonad       HASK      Maybe
instance CatFunctor     HASK HASK ((->) a) where map = (.)

instance CatFunctor HASK HASK ((,) a) where map = right

($) :: CatId cat => a `cat` a
($) = id

class Semigroupoid cat where
    (.) :: b `cat` c -> a `cat` b -> a `cat` c
class CatId cat where
    id :: cat a a
class (Semigroupoid cat, CatId cat) => Category cat


instance Semigroupoid HASK where (.) = (Base..)
instance CatId        HASK where id = Base.id
instance Category     HASK

data Iso cat a b = (:<->)
    { to :: a `cat` b
    , from :: b `cat` a
    }
type (<->) = Iso HASK
type (<~>) = Iso (~>)

instance Semigroupoid cat => Semigroupoid (Iso cat) where
    (t0 :<-> f0) . (t1 :<-> f1) = (t0 . t1) :<-> (f1 . f0)
instance CatId cat => CatId (Iso cat) where
    id = id :<-> id
instance Category cat => Category (Iso cat)

newtype Dual cat a b = Dual
    { dual :: b `cat` a
    }
type (<--) = Dual HASK
type (<~) = Dual (~>)

instance Semigroupoid cat => Semigroupoid (Dual cat) where
    Dual g . Dual f = Dual (f . g)
instance CatId cat => CatId (Dual cat) where
    id = Dual id
instance Category cat => Category (Dual cat)

newtype NT cat f g = NT
    { nt :: forall a. f a `cat` g a
    }
type (~>) = NT HASK

newtype NT1 cat f g a = NT1
    { nt1 :: f a `cat` g a
    }
type (~~>) = NT1 HASK

instance Semigroupoid cat => Semigroupoid (NT cat) where
    NT f . NT g = NT (f . g)
instance CatId cat => CatId (NT cat) where
    id = NT id
instance Category cat => Category (NT cat)

newtype CatKleisli cat m a b = Kleisli
    { kleisli :: a `cat` m b
    }
type Kleisli = CatKleisli HASK


instance (CatMonad cat m, Semigroupoid cat) => Semigroupoid (CatKleisli cat m) where
    (.) :: forall a b c. CatKleisli cat m b c -> CatKleisli cat m a b -> CatKleisli cat m a c
    (.) = coerce ((<=<) :: b `cat` m c -> a `cat` m b -> a `cat` m c)
instance (CatPure cat m, CatId cat) => CatId (CatKleisli cat m) where
    id :: forall a. CatKleisli cat m a a
    id = coerce (pure :: a `cat` m a)
instance (CatMonad cat m, Category cat) => Category (CatKleisli cat m)


class Cartesian cat => Closed cat where
    type Exp cat :: k -> k -> k
    apply :: Product cat (Exp cat a b) a `cat` b
    curry :: (Product cat a b `cat` c) -> a `cat` Exp cat b c
    uncurry :: (a `cat` Exp cat b c) -> Product cat a b `cat` c

flip :: (Closed cat, CatAssociative cat (Product cat)) => b `cat` Exp cat a c -> a `cat` Exp cat b c
flip f = curry (uncurry f . assoc)

instance Closed HASK where
    type Exp HASK = HASK
    apply (f, a) = f a
    curry f a b = f (a, b)
    uncurry f (a, b) = f a b

instance Closed (~>) where
    type Exp (~>) = (~~>)
    apply = NT ((\(NT1 f, a) -> f a) . prod1)
    curry (NT f) = NT (\a -> NT1 (\b -> f (Prod1 (a, b))))
    uncurry (NT f) = NT (\(Prod1 (a, b)) -> nt1 (f a) b)

instance ( Monad m
         , Distributive m
         ) => Closed (Kleisli m) where
    type Exp (Kleisli m) = Kleisli m
    apply = Kleisli (uncurry kleisli)
    curry f = Kleisli (pure . Kleisli . (kleisli f .) . (,))
    uncurry f = Kleisli (uncurry ((. flip kleisli) . (>>=) . kleisli f))

class EndoBifunctor cat (Product cat) => Cartesian (cat :: k -> k -> Type) where
    type Product cat :: k -> k -> k
    fst :: Product cat a b `cat` a
    snd :: Product cat a b `cat` b

    copy :: a `cat` Product cat a a
    copy = id &&& id

    (&&&) :: b `cat` c -> b `cat` c' -> b `cat` Product cat c c'
    infixr 3 &&&
    f &&& g = bimap f g . copy
    {-# MINIMAL fst, snd, (copy | (&&&)) #-}

class EndoBifunctor cat (Coproduct cat) => Cocartesian (cat :: k -> k -> Type) where
    type Coproduct cat :: k -> k -> k
    lft  :: a `cat` Coproduct cat a b
    rght :: b `cat` Coproduct cat a b

    fuse :: Coproduct cat a a `cat` a
    fuse = id ||| id

    (|||) :: b `cat` d -> c `cat` d -> Coproduct cat b c `cat` d
    infixr 2 |||
    f ||| g = fuse . bimap f g
    {-# MINIMAL lft, rght, (fuse | (|||)) #-}

instance Cocartesian cat => Cartesian (Dual cat) where
    type Product (Dual cat) = Coproduct cat
    fst = Dual lft
    snd = Dual rght
    copy = Dual fuse
    Dual f &&& Dual g = Dual (f ||| g)

instance Cartesian cat => Cocartesian (Dual cat) where
    type Coproduct (Dual cat) = Product cat
    lft = Dual fst
    rght = Dual snd
    fuse = Dual copy
    Dual f ||| Dual g = Dual (f &&& g)

instance Cartesian HASK where
    type Product HASK = (,)
    fst (a, _) = a
    snd (_, b) = b
    (&&&) f g a = (f a, g a)

instance ( CatMonad cat m
         , Cartesian cat
         , EndoBifunctor (CatKleisli cat m) (Product cat)
         ) => Cartesian (CatKleisli cat m) where
    type Product (CatKleisli cat m) = Product cat
    fst = Kleisli (pure . fst)
    snd = Kleisli (pure . snd)
    copy = Kleisli (pure . copy)

instance ( CatMonad cat m
         , Cocartesian cat
         , EndoBifunctor (CatKleisli cat m) (Coproduct cat)
         ) => Cocartesian (CatKleisli cat m) where
    type Coproduct (CatKleisli cat m) = Coproduct cat
    lft = Kleisli (pure . lft)
    rght = Kleisli (pure . rght)
    fuse = Kleisli (pure . fuse)

newtype Prod1 p f g a = Prod1
    { prod1 :: f a `p` g a
    }
type Product1 = Prod1 (,)
type Coproduct1 = Prod1 Either

instance Cartesian (~>) where
    type Product (~>) = Product1
    NT f &&& NT g = NT (Prod1 . (f &&& g))
    fst = NT (fst . prod1)
    snd = NT (snd . prod1)

instance Cocartesian HASK where
    type Coproduct HASK = Either
    lft = Left
    rght = Right
    (|||) = either

instance Cocartesian (~>) where
    type Coproduct (~>) = Coproduct1
    lft = NT (Prod1 . lft)
    rght = NT (Prod1 . rght)
    NT f ||| NT g = NT ((f ||| g) . prod1)

class Category cat => CatAssociative (cat :: k1 -> k1 -> Type) (f :: k0 -> k0 -> k1) where
    assoc :: (a `f` b) `cat` (b `f` a)

instance CatAssociative cat f => CatAssociative (Dual cat) f where
    assoc = Dual assoc

instance CatAssociative cat f => CatAssociative (Iso cat) f where
    assoc = assoc :<-> assoc

instance CatAssociative HASK f => CatAssociative (~>) (Prod1 f) where
    assoc = NT (Prod1 . assoc . prod1)

assocProd :: Cartesian cat => Product cat a b `cat` Product cat b a
assocProd = snd &&& fst

assocCoprod :: Cocartesian cat => Coproduct cat a b `cat` Coproduct cat b a
assocCoprod = rght ||| lft

instance CatAssociative HASK (,) where
    assoc = assocProd
instance CatAssociative HASK Either where
    assoc = assocCoprod

instance Category cat => CatAssociative HASK (Iso cat) where
    assoc (f :<-> g) = g :<-> f

class CatAssociative cat f => Monoidal (cat :: k -> k -> Type) (f :: k -> k -> k) where
    type Id cat f :: k
    idl :: Iso cat (Id cat f `f` a) a
    idl = (to idr . assoc) :<-> (assoc . from idr)
    idr :: Iso cat (a `f` Id cat f) a
    idr = (to idl . assoc) :<-> (assoc . from idl)
    {-# MINIMAL idl | idr #-}

instance Monoidal cat f => Monoidal (Dual cat) f where
    type Id (Dual cat) f = Id cat f
    idl = Dual (from idl) :<-> Dual (to idl)

instance Monoidal cat f => Monoidal (Iso cat) f where
    type Id (Iso cat) f = Id cat f
    idl = idl :<-> assoc idl

instance (LeftFunctor f, Monoidal HASK f) => Monoidal (~>) (Prod1 f) where
    type Id (~>) (Prod1 f) = Const (Id HASK f)
    idl = NT (to idl . left getConst . prod1)
     :<-> NT (Prod1 . left Const . from idl)

instance Monoidal HASK (,) where
    type Id HASK (,) = ()
    idl = snd :<-> ((),)

instance Monoidal HASK Either where
    type Id HASK Either = Void
    idl = (absurd ||| id) :<-> Right

class (Category r, Category t) => CatLeftFunctor r t p | p r -> t, p t -> r where
    left :: r a b -> t (p a c) (p b c)
    default left :: CatBifunctor r s t p => r a b -> t (p a c) (p b c)
    left = (`bimap` id)
type EndoLeftFunctor cat = CatLeftFunctor cat cat
type LeftFunctor = EndoLeftFunctor HASK

class (Category s, Category t) => CatRightFunctor s t q | q s -> t, q t -> s where
    right :: s a b -> t (q c a) (q c b)
    default right :: CatBifunctor r s t q => s a b -> t (q c a) (q c b)
    right = bimap id
type EndoRightFunctor cat = CatRightFunctor cat cat
type RightFunctor = EndoRightFunctor HASK

class (CatLeftFunctor r t p, CatRightFunctor s t p) => CatBifunctor r s t p | p r -> s t, p s -> r t, p t -> r s where
    bimap :: a `r` b -> c `s` d -> (a `p` c) `t` (b `p` d)
type EndoBifunctor cat = CatBifunctor cat cat cat
type Bifunctor = EndoBifunctor HASK

instance CatLeftFunctor  HASK HASK (,)
instance CatRightFunctor  HASK HASK (,)
instance CatBifunctor HASK HASK HASK (,) where
    bimap f g (a,b)= (f a, g b)

instance CatLeftFunctor  HASK HASK Either
instance CatRightFunctor  HASK HASK Either
instance CatBifunctor HASK HASK HASK Either where
    bimap f _ (Left a) = Left (f a)
    bimap _ g (Right a) = Right (g a)


instance EndoLeftFunctor  HASK f => CatLeftFunctor  (~>) (~>) (Prod1 f) where
    left (NT f) = NT (Prod1 . left f . prod1)
instance EndoRightFunctor  HASK f => CatRightFunctor  (~>) (~>) (Prod1 f) where
    right (NT f) = NT (Prod1 . right f . prod1)
instance EndoBifunctor HASK f => CatBifunctor (~>) (~>) (~>) (Prod1 f) where
    bimap (NT f) (NT g) = NT (Prod1 . bimap f g . prod1)


instance CatLeftFunctor c0 c1 f => CatLeftFunctor (Dual c0) (Dual c1) f where
    left (Dual f) = Dual (left f)
instance CatRightFunctor c0 c1 f => CatRightFunctor (Dual c0) (Dual c1) f where
    right (Dual f) = Dual (right f)
instance CatBifunctor c0 c1 c2 f => CatBifunctor (Dual c0) (Dual c1) (Dual c2) f where
    bimap (Dual f) (Dual g) = Dual (bimap f g)

instance ( CatAssociative cat f
         , EndoLeftFunctor cat f
         , CatDistributive cat m
         , forall c. EndoFunctor cat (f c)
         , CatMonad cat m
         , EndoFunctor cat m
         ) => CatLeftFunctor (CatKleisli cat m) (CatKleisli cat m) f where
    left :: forall a b c. CatKleisli cat m a b -> CatKleisli cat m (f a c) (f b c)
    left (Kleisli f) = Kleisli (map assoc' . distribute . assoc . left f)
      where assoc' = assoc :: f c b `cat` f b c
instance ( CatAssociative cat f
         , EndoRightFunctor cat f
         , CatDistributive cat m
         , forall c. EndoFunctor cat (f c)
         , CatMonad cat m
         ) => CatRightFunctor (CatKleisli cat m) (CatKleisli cat m) f where
    right (Kleisli f) = Kleisli (distribute . right f)
instance ( CatAssociative cat f
         , CatDistributive cat m
         , EndoBifunctor cat f
         , forall c. EndoFunctor cat (f c)
         , CatMonad cat m
         ) => CatBifunctor (CatKleisli cat m) (CatKleisli cat m) (CatKleisli cat m) f where
    bimap f g = left f . right g

