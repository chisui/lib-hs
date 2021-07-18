module Std.Trigonometric where

import "this" Std.Group
import "this" Std.Cat


class Ring a => Exponential a where
    exp :: a -> a

class Exponential a => Euler a where
    {-# MINIMAL euler | 𝑒 #-}
    euler, 𝑒 :: a
    euler = 𝑒
    𝑒 = euler

class (Magma 'Div a, Quasigroup 'Mult a, Ring a) => Trigonometric a where
    {-# MINIMAL (sin, cos) | (sec, csc) #-}
    sin, cos, tan :: a -> a
    sin = recip . csc
    cos = recip . sec
    tan = lift2 (/) sin cos

    cot, sec, csc :: a -> a
    cot = lift2 (/) cos sin
    sec = recip . cos
    csc = recip . sin

class Trigonometric a => Pi a where
    {-# MINIMAL pi | π #-}
    pi, π :: a
    π = pi
    pi = π
