{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Std.Complex
    ( Complex(..), realPart, imagPart
    , 𝒾
    ) where

import "base" Data.Complex

import "this" Std.Bool
import "this" Std.IfThenElse
import "this" Std.Type
import "this" Std.Literal
import "this" Std.BinOp
import "this" Std.Group
import "this" Std.Cat


𝒾 :: Ring a => Complex a
𝒾 = zero :+ one



instance (FromInteger a, Ring a) => FromInteger (Complex a) where
    fromInteger = (:+ zero) . fromInteger

liftOpComplex# :: forall op a. TotalBinOp op a => Proxy# op -> Complex a -> Complex a -> Complex a
liftOpComplex# p (a :+ ai) (b :+ bi) = op# p a b :+ op# p ai bi

instance Ring a => BinOp 'Add (Complex a) where op# = liftOpComplex#
instance Ring a => IdentityOp 'Add (Complex a) where identity# _ = (zero :+ zero)
instance Ring a => InverseOp 'Add (Complex a) where
    type InvOp 'Add (Complex a) = 'Sub
    inv# p (a :+ ai) = inv# p a :+ inv# p ai
instance Ring a => CommutativeOp 'Add (Complex a)
instance Ring a => AssociativeOp 'Add (Complex a)

instance Ring a => InverseOp 'Sub (Complex a) where
    type InvOp 'Sub (Complex a) = 'Add
    inv# p (a :+ ai) = inv# p a :+ inv# p ai
instance Ring a => BinOp 'Sub (Complex a) where op# = liftOpComplex#

instance Ring a => BinOp 'Mult (Complex a) where
    op# _ (a :+ ai) (b :+ bi) = (a * ai - b * bi) :+ (a * bi + b * ai)
instance Ring a => IdentityOp 'Mult (Complex a) where identity# _ = (one :+ zero)
instance Ring a => AssociativeOp 'Mult (Complex a)
instance Ring a => DistributiveOp 'Add 'Mult (Complex a)

deriving via (Basic1 Complex) instance CatFunctor'     Unconstrained HASK HASK Complex
deriving via (Basic1 Complex) instance CatPure'        Unconstrained HASK Complex
deriving via (Basic1 Complex) instance CatAp'          Unconstrained HASK Complex
deriving via (Basic1 Complex) instance CatLift2'       Unconstrained HASK Complex
deriving via (Basic1 Complex) instance CatApplicative' Unconstrained HASK Complex
deriving via (Basic1 Complex) instance CatBind'        Unconstrained HASK Complex
deriving via (Basic1 Complex) instance CatJoin'        Unconstrained HASK Complex
deriving via (Basic1 Complex) instance CatMonad'       Unconstrained HASK Complex

instance CatExtract'   Ring HASK Complex where extract = realPart
instance CatExtend'    Ring HASK Complex where (<<=) f = map f . duplicate
instance CatDuplicate' Ring HASK Complex where duplicate (a :+ ai) = (a :+ zero) :+ (zero :+ ai)
instance CatComonad'   Ring HASK Complex


instance CatDistributive HASK Complex where
    distribute wc = map realPart wc :+ map imagPart wc

instance CatRepresentable HASK HASK Complex where
    type CatRep HASK HASK Complex = Bool
    catRep = tabulate :<-> index
      where
        index (r :+ i) key = if key then i else r
        tabulate f = f False :+ f True
