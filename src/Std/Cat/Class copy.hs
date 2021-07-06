{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Std.Cat.Class
    ( HASK
    , CatFunctor(..), EndoFunctor, Functor, mapEndo, pam
    , CatPure(..), Pure, pure
    , CatAp(..), Ap
    , CatLift2(..), Lift2
    , CatApplicative, Applicative
    , CatBind(..), Bind, (>>=)
    , CatJoin(..), Join
    , CatMonad, Monad
    , CatMonadFix(..), MonadFix
    , CatMonadFail(..), MonadFail
    , Identity(..)
    , Semigroupoid(..)
    , CatId(..)
    , Category
    , Groupoid(..)
    , Closed(..), flip, const, on, on'
    , CatFix(..)
    , CatDistributive(..), Distributive
    , Dual(..), type (<--), type (<~)
    , NT(..), η, type (~>)
    , NT1, type (~~>)
    , CatKleisli(..), Kleisli
    , Cartesian(..), Cocartesian(..)
    , CatAssociative(..)
    , Prod1(..), Product1, Coproduct1
    , CatLeftFunctor(..), EndoLeftFunctor, LeftFunctor
    , CatRightFunctor(..), EndoRightFunctor, RightFunctor
    , CatBifunctor(..), EndoBifunctor, Bifunctor, bimap
    , CatProfunctor, EndoProfunctor, Profunctor, catDimap, dimap
    , CatExtract(..), Extract, CatExtend(..), Extend, CatDuplicate(..), Duplicate, CatComonad, Comonad
    , ($)
    ) where

import "base" Data.String ( String )
import "base" Data.Either
import "base" Data.Kind ( Type )
import "base" Data.Coerce ( coerce )
import "base" Data.Maybe ( Maybe )
import "base" Data.Function qualified as Base
import "base" Data.Functor qualified as Base
import "base" Data.Functor.Identity ( Identity(..) )
import "base" Control.Applicative qualified as Base
import "base" Control.Monad qualified as Base

import "this" Std.Constraint


type HASK = (->)

pam :: CatFunctor (Dual c0) c1 f => b `c0` a -> f a `c1` f b
pam = map . Dual


class EndoFunctor cat f => CatExtract cat f where
    extract :: f a `cat` a
type Extract = CatExtract HASK


class CatExtend cat m where
    (<<=) :: m a `cat` b -> m a `cat` m b
type Extend = CatExtend HASK

class EndoFunctor cat f => CatDuplicate cat f where
    duplicate :: f a `cat` f (f a)
type Duplicate = CatDuplicate HASK


class CatDistributive cat g where
    distribute :: EndoFunctor cat f => f (g a) `cat` g (f a)
type Distributive = CatDistributive HASK



class (Category cat, CatExtract cat f, CatExtend cat f, CatDuplicate cat f) => CatComonad cat f
type Comonad = CatComonad HASK

class (Closed cat, CatMonad cat f) => CatMonadFix cat f where
    mfix :: Exp cat a (f a) `cat` f a
type MonadFix = CatMonadFix HASK

class CatMonad cat f => CatMonadFail cat f where
    fail :: String `cat` f a
type MonadFail = CatMonadFail HASK

-- instances

instance CatFunctor      HASK HASK Identity where map = coerce
instance CatPure         HASK      Identity where catPure = coerce
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
instance CatPure        HASK      Maybe where catPure = Base.pure
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
class Category cat => Groupoid cat where
    invCat :: a `cat` b -> b `cat` a


instance Semigroupoid HASK where (.) = (Base..)
instance CatId        HASK where id = Base.id
instance Category     HASK

newtype Dual cat a b = Dual
    { unDual :: b `cat` a
    }
type (<--) = Dual HASK
type (<~) = Dual (~>)

instance Semigroupoid cat => Semigroupoid (Dual cat) where
    Dual g . Dual f = Dual (f . g)
instance CatId cat => CatId (Dual cat) where
    id = Dual id
instance Category cat => Category (Dual cat)

newtype NT cat f g = NT
    { eta :: forall a. f a `cat` g a
    }
η :: NT cat f g -> (forall a. f a `cat` g a)
η = eta
type (~>) = NT HASK

type NT1 = Prod1
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
    id = coerce (catPure :: a `cat` m a)
instance (CatMonad cat m, Category cat) => Category (CatKleisli cat m)


class Cartesian cat => Closed cat where
    type Exp cat :: k -> k -> k
    apply :: Product cat (Exp cat a b) a `cat` b
    curry :: (Product cat a b `cat` c) -> a `cat` Exp cat b c
    uncurry :: (a `cat` Exp cat b c) -> Product cat a b `cat` c

const :: Closed cat => a `cat` Exp cat b a
const = curry fst

flip :: (Closed cat, CatAssociative cat (Product cat)) => b `cat` Exp cat a c -> a `cat` Exp cat b c
flip f = curry (uncurry f . assoc)

on' :: Closed cat => a `cat` Exp cat b c -> a' `cat` a -> b' `cat` b -> a' `cat` Exp cat b' c
on' f g h = curry (uncurry f . (g *** h))

on :: Closed cat => a `cat` Exp cat a c -> b `cat` a -> b `cat` Exp cat b c
on f g = on' f g g
infixl 0 `on`

instance Closed HASK where
    type Exp HASK = HASK
    apply (f, a) = f a
    curry f a b = f (a, b)
    uncurry f (a, b) = f a b

instance Closed (~>) where
    type Exp (~>) = (~~>)
    apply = NT ((\(Prod1 f, a) -> f a) . prod1)
    curry (NT f) = NT (\a -> Prod1 (\b -> f (Prod1 (a, b))))
    uncurry (NT f) = NT (\(Prod1 (a, b)) -> prod1 (f a) b)

instance ( Monad m
         , Distributive m
         ) => Closed (Kleisli m) where
    type Exp (Kleisli m) = Kleisli m
    apply :: Kleisli m (Kleisli m a b, a) b
    apply = Kleisli (uncurry kleisli)
    curry (Kleisli f) = Kleisli (catPure . Kleisli . curry f)
    uncurry (Kleisli f) = Kleisli $ \(a, b) -> app ( (,b) <$> f a)  --(uncurry ((. flip kleisli) . (>>=) . f))
      where
        app :: m (Kleisli m a b, a) -> m b
        app m = do 
            (Kleisli g, b) <- m
            g b


class Closed cat => CatFix cat where
    fix :: Exp cat a a `cat` a

instance CatFix HASK where
    fix f = let x = f x in x

--instance CatMonadFix cat m => CatFix (CatKleisli cat m) where

class EndoBifunctor Unconstraint cat (Product cat) => Cartesian (cat :: k -> k -> Type) where
    type Product cat :: k -> k -> k
    fst :: Product cat a b `cat` a
    snd :: Product cat a b `cat` b

    copy :: a `cat` Product cat a a
    copy = id &&& id

    (&&&) :: b `cat` c -> b `cat` c' -> b `cat` Product cat c c'
    infixr 3 &&&
    f &&& g = catBimap f g . copy
    {-# MINIMAL fst, snd, (copy | (&&&)) #-}

class EndoBifunctor Unconstraint cat (Coproduct cat) => Cocartesian (cat :: k -> k -> Type) where
    type Coproduct cat :: k -> k -> k
    lft  :: a `cat` Coproduct cat a b
    rght :: b `cat` Coproduct cat a b

    fuse :: Coproduct cat a a `cat` a
    fuse = id ||| id

    (|||) :: b `cat` d -> c `cat` d -> Coproduct cat b c `cat` d
    infixr 2 |||
    f ||| g = fuse . catBimap f g
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
         , EndoBifunctor Unconstraint (CatKleisli cat m) (Product cat)
         ) => Cartesian (CatKleisli cat m) where
    type Product (CatKleisli cat m) = Product cat
    fst = Kleisli (catPure . fst)
    snd = Kleisli (catPure . snd)
    copy = Kleisli (catPure . copy)

instance ( CatMonad cat m
         , Cocartesian cat
         , EndoBifunctor Unconstraint (CatKleisli cat m) (Coproduct cat)
         ) => Cocartesian (CatKleisli cat m) where
    type Coproduct (CatKleisli cat m) = Coproduct cat
    lft = Kleisli (catPure . lft)
    rght = Kleisli (catPure . rght)
    fuse = Kleisli (catPure . fuse)


newtype Prod1 p f g a = Prod1
    { prod1 :: f a `p` g a
    }
type Product1 = Prod1 (,)
type Coproduct1 = Prod1 Either


instance ( CatFunctor c c f
         , CatFunctor c c g
         , CatBifunctor Unconstraint c c HASK p
         ) => CatFunctor c HASK (Prod1 p f g) where
    map :: forall a b. a `c` b -> Prod1 p f g a -> Prod1 p f g b
    map f = coerce (catBimap @Unconstraint @c @c (map f) (map f) :: p (f a) (g a) -> p (f b) (g b))


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





instance CatLeftFunctor c c0 c1 f => CatLeftFunctor c (Dual c0) (Dual c1) f where
    left (Dual f) = Dual (left f)
instance CatRightFunctor c c0 c1 f => CatRightFunctor c (Dual c0) (Dual c1) f where
    right (Dual f) = Dual (right f)
instance CatBifunctor c c0 c1 c2 f => CatBifunctor c (Dual c0) (Dual c1) (Dual c2) f where
    catBimap (Dual f) (Dual g) = Dual (catBimap f g)

instance CatLeftFunctor  Unconstraint (Dual HASK) HASK HASK where left (Dual g) f = f . g
instance CatRightFunctor Unconstraint       HASK  HASK HASK where right = (.)
instance CatBifunctor    Unconstraint (Dual HASK) HASK HASK HASK

instance ( CatAssociative HASK f
         , EndoLeftFunctor Unconstraint HASK f
         , CatDistributive HASK m
         , CatMonad HASK m
         , EndoFunctor HASK m
         ) => CatLeftFunctor Unconstraint (CatKleisli HASK m) (CatKleisli HASK m) f where
    left (Kleisli f) = Kleisli (map unLeft . distribute . map f . MkLeft)
instance ( CatAssociative HASK f
         , EndoRightFunctor Unconstraint HASK f
         , CatDistributive HASK m
         , CatMonad HASK m
         ) => CatRightFunctor Unconstraint (CatKleisli HASK m) (CatKleisli HASK m) f where
    right (Kleisli f) = Kleisli (map unRight . distribute . map f . MkRight)
instance ( CatAssociative HASK f
         , CatDistributive HASK m
         , EndoLeftFunctor Unconstraint HASK f
         , EndoRightFunctor Unconstraint HASK f
         , EndoBifunctor Unconstraint HASK f
         , CatMonad HASK m
         ) => CatBifunctor Unconstraint (CatKleisli HASK m) (CatKleisli HASK m) (CatKleisli HASK m) f where
    catBimap f g = left f . right g

