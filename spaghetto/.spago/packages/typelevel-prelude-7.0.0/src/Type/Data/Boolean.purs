module Type.Data.Boolean
  ( module Prim.Boolean
  , class IsBoolean
  , reflectBoolean
  , reifyBoolean
  , class And
  , and
  , class Or
  , or
  , class Not
  , not
  , class If
  , if_
  ) where

import Prim.Boolean (True, False)
import Type.Proxy (Proxy(..))

-- | Class for reflecting a type level `Boolean` at the value level
class IsBoolean :: Boolean -> Constraint
class IsBoolean bool where
  reflectBoolean :: Proxy bool -> Boolean

instance isBooleanTrue :: IsBoolean True where reflectBoolean _ = true
instance isBooleanFalse :: IsBoolean False where reflectBoolean _ = false

-- | Use a value level `Boolean` as a type-level `Boolean`
reifyBoolean :: forall r. Boolean -> (forall o. IsBoolean o => Proxy o -> r) -> r
reifyBoolean true f = f (Proxy :: Proxy True)
reifyBoolean false f = f (Proxy :: Proxy False)

-- | And two `Boolean` types together
class And :: Boolean -> Boolean -> Boolean -> Constraint
class And lhs rhs out | lhs rhs -> out
instance andTrue :: And True rhs rhs
instance andFalse :: And False rhs False

and :: forall l r o. And l r o => Proxy l -> Proxy r -> Proxy o
and _ _ = Proxy

-- | Or two `Boolean` types together
class Or :: Boolean -> Boolean -> Boolean -> Constraint
class Or lhs rhs output | lhs rhs -> output
instance orTrue :: Or True rhs True
instance orFalse :: Or False rhs rhs

or :: forall l r o. Or l r o => Proxy l -> Proxy r -> Proxy o
or _ _ = Proxy

-- | Not a `Boolean`
class Not :: Boolean -> Boolean -> Constraint
class Not bool output | bool -> output
instance notTrue :: Not True False
instance notFalse :: Not False True

not :: forall i o. Not i o => Proxy i -> Proxy o
not _ = Proxy

-- | If - dispatch based on a boolean
class If :: forall k. Boolean -> k -> k -> k -> Constraint
class If bool onTrue onFalse output | bool onTrue onFalse -> output
instance ifTrue :: If True onTrue onFalse onTrue
instance ifFalse :: If False onTrue onFalse onFalse

if_ :: forall b t e o. If b t e o => Proxy b -> Proxy t -> Proxy e -> Proxy o
if_ _ _ _ = Proxy
