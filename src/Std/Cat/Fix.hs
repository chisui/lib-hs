module Std.Cat.Fix where

import "this" Std.Cat.Class
import "this" Std.Cat.Closed


class Closed cat => CatFix cat where
    fix :: Exp cat a a `cat` a

instance CatFix HASK where
    fix f = let x = f x in x
