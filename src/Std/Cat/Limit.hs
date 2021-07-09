module Std.Cat.Limit where

import "base" Data.Void

import "this" Std.Cat.Class
import "this" Std.Cat.Op
import "this" Std.Cat.Applicative
import "this" Std.Cat.Iso
import "this" Std.Cat.Kleisli


class CatTerminal cat where
    type Terminal cat
    terminate :: a `cat` Terminal cat

class CatInitial cat where
    type Initial cat
    initiate :: Initial cat `cat` a


instance CatTerminal HASK where
    type Terminal HASK = ()
    terminate _ = ()
instance CatInitial HASK where
    type Initial HASK = Void
    initiate = absurd

instance CatInitial cat => CatTerminal (Op cat) where
    type Terminal (Op cat) = Initial cat
    terminate = Op initiate
instance CatTerminal cat => CatInitial (Op cat) where
    type Initial (Op cat) = Terminal cat
    initiate = Op terminate

instance Pure m => CatTerminal (Kleisli m) where
    type Terminal (Kleisli m) = Terminal HASK
    terminate = Kleisli (catPure . terminate)
instance CatInitial (Kleisli m) where
    type Initial (Kleisli m) = Initial HASK
    initiate = Kleisli absurd

instance (CatTerminal cat, CatInitial cat, Terminal cat ~ Initial cat) => CatTerminal (Iso cat) where
    type Terminal (Iso cat) = Initial cat
    terminate = terminate :<-> initiate
instance (CatTerminal cat, CatInitial cat, Terminal cat ~ Initial cat) => CatInitial (Iso cat) where
    type Initial (Iso cat) = Terminal cat
    initiate = initiate :<-> terminate
