module Std.Free where

data Free f a
    = Pure a
    | Free (f (Free f a))
