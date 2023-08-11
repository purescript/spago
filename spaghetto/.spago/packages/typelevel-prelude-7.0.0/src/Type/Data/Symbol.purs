module Type.Data.Symbol
  ( module Prim.Symbol
  , module Data.Symbol
  , append
  , compare
  , uncons
  , class Equals
  , equals
  ) where

import Prim.Symbol (class Append, class Compare, class Cons)
import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol)
import Type.Data.Ordering (EQ)
import Type.Data.Ordering (class Equals) as Ordering
import Type.Proxy (Proxy(..))

compare :: forall l r o. Compare l r o => Proxy l -> Proxy r -> Proxy o
compare _ _ = Proxy

append :: forall l r o. Append l r o => Proxy l -> Proxy r -> Proxy o
append _ _ = Proxy

uncons :: forall h t s. Cons h t s => Proxy s -> {head :: Proxy h, tail :: Proxy t}
uncons _ = {head : Proxy, tail : Proxy}

class Equals :: Symbol -> Symbol -> Boolean -> Constraint
class Equals lhs rhs out | lhs rhs -> out

instance equalsSymbol
  :: (Compare lhs rhs ord,
      Ordering.Equals EQ ord out)
  => Equals lhs rhs out

equals :: forall l r o. Equals l r o => Proxy l -> Proxy r -> Proxy o
equals _ _ = Proxy
