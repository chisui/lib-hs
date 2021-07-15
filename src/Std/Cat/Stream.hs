module Std.Cat.Stream where

import "base" Data.Functor.Identity

import "this" Std.Type
import "this" Std.Ord
import "this" Std.Bool
import "this" Std.Literal
import "this" Std.BinOp
import "this" Std.Cat


data StreamT m a where
    Stream :: (s -> m (StreamT m a, a)) -> s -> StreamT m a
type Stream = StreamT Identity

stepM :: StreamT m a -> m (StreamT m a, a)
stepM (Stream f s) = f s

step :: Stream a -> (Stream a, a)
step = runIdentity . stepM

takeSM :: (Monad m, Alternative f, Iterable i) => i -> StreamT m a -> m (f a)
takeSM i s
    | i == zero = pure empty
    | otherwise = do
        (s', a) <- stepM s
        cons a <$> takeSM (pred i) s'
        

instance Functor m => CatFunctor' Unconstrained HASK HASK (StreamT m) where
    f <$$> Stream g s = Stream (map (map f *** f) . g) s

instance (CatApplicative HASK m, CatMonad HASK m) => CatPure' Unconstrained HASK (StreamT m) where
    catPure a = let s = Stream (const (pure (s,a))) () in s

instance (CatApplicative HASK m, CatMonad HASK m) => CatLift2' Unconstrained HASK (StreamT m) where
    lift2 f a b = f <$> a <*> b
instance (CatApplicative HASK m, CatMonad HASK m) => CatAp' Unconstrained HASK (StreamT m) where
    mf <**> ma = do
        f <- mf
        a <- ma
        pure (f a)

instance (CatApplicative HASK m, CatMonad HASK m) => CatApplicative' Unconstrained HASK (StreamT m)

instance (CatApplicative HASK m, CatMonad HASK m) => CatBind' Unconstrained HASK (StreamT m) where
    f =<< (Stream g s) = Stream step' s 
      where
        step' s' = do
            (_, a) <- g s' -- only map first element since Stream is infinite.
            case f a of
                Stream h t -> do
                    (Stream h' t', b) <- h t
                    pure (Stream h' t', b)

instance (CatApplicative HASK m, CatMonad HASK m) => CatJoin' Unconstrained HASK (StreamT m) where
    join = (=<<) id
instance (CatApplicative HASK m, CatMonad HASK m) => CatMonad' Unconstrained HASK (StreamT m)

instance (CatApplicative HASK m, CatMonad HASK m) => CatCombine' Unconstrained HASK (StreamT m) where
    combine = fst -- just return left since the Stream is infinite

instance Comonad m => CatExtract' Unconstrained HASK (StreamT m) where
    extract (Stream f s) = snd . extract . f $ s
instance Comonad m => CatExtend' Unconstrained HASK (StreamT m) where
    (<<=) f = map f . duplicate
instance Comonad m => CatDuplicate' Unconstrained HASK (StreamT m) where
    duplicate :: forall a. StreamT m a -> StreamT m (StreamT m a)
    duplicate (Stream f s) = Stream (step' f) s
      where
        step' :: forall s. (s -> m (StreamT m a, a)) -> s -> m (StreamT m (StreamT m a), StreamT m a) 
        step' g s' = flip map (g s') $ \(Stream h s'', _) -> (Stream (step' h) s'', Stream g s')

instance Comonad m => CatComonad' Unconstrained HASK (StreamT m)

instance CatLeftFunctor' Monad Unconstrained (~>) HASK StreamT where
    left' f (Stream g s) = Stream step' s
      where
        step' = map (left (left' f)) . eta f . g
instance CatRightFunctor' Monad Unconstrained HASK HASK StreamT where
    right' = map
instance CatBifunctor' Monad Unconstrained (~>) HASK HASK StreamT
