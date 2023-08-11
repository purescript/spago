module Type.Data.Ordering
  ( module PO
  , class IsOrdering
  , reflectOrdering
  , reifyOrdering
  , class Append
  , append
  , class Invert
  , invert
  , class Equals
  , equals
  ) where

import Prim.Ordering (LT, EQ, GT, Ordering) as PO
import Data.Ordering (Ordering(..))
import Type.Data.Boolean (True, False)
import Type.Proxy (Proxy(..))

-- | Class for reflecting a type level `Ordering` at the value level
class IsOrdering :: PO.Ordering -> Constraint
class IsOrdering ordering where
  reflectOrdering :: Proxy ordering -> Ordering

instance isOrderingLT :: IsOrdering PO.LT where reflectOrdering _ = LT
instance isOrderingEQ :: IsOrdering PO.EQ where reflectOrdering _ = EQ
instance isOrderingGT :: IsOrdering PO.GT where reflectOrdering _ = GT

-- | Use a value level `Ordering` as a type-level `Ordering`
reifyOrdering :: forall r. Ordering -> (forall o. IsOrdering o => Proxy o -> r) -> r
reifyOrdering LT f = f (Proxy :: Proxy PO.LT)
reifyOrdering EQ f = f (Proxy :: Proxy PO.EQ)
reifyOrdering GT f = f (Proxy :: Proxy PO.GT)

-- | Append two `Ordering` types together
-- | Reflective of the semigroup for value level `Ordering`
class Append :: PO.Ordering -> PO.Ordering -> PO.Ordering -> Constraint
class Append lhs rhs output | lhs -> rhs output
instance appendOrderingLT :: Append PO.LT rhs PO.LT
instance appendOrderingEQ :: Append PO.EQ rhs rhs
instance appendOrderingGT :: Append PO.GT rhs PO.GT

append :: forall l r o. Append l r o => Proxy l -> Proxy r -> Proxy o
append _ _ = Proxy

-- | Invert an `Ordering`
class Invert :: PO.Ordering -> PO.Ordering -> Constraint
class Invert ordering result | ordering -> result
instance invertOrderingLT :: Invert PO.LT PO.GT
instance invertOrderingEQ :: Invert PO.EQ PO.EQ
instance invertOrderingGT :: Invert PO.GT PO.LT

invert :: forall i o. Invert i o => Proxy i -> Proxy o
invert _ = Proxy

class Equals :: PO.Ordering -> PO.Ordering -> Boolean -> Constraint
class Equals lhs rhs out | lhs rhs -> out

instance equalsEQEQ :: Equals PO.EQ PO.EQ True
instance equalsLTLT :: Equals PO.LT PO.LT True
instance equalsGTGT :: Equals PO.GT PO.GT True
instance equalsEQLT :: Equals PO.EQ PO.LT False
instance equalsEQGT :: Equals PO.EQ PO.GT False
instance equalsLTEQ :: Equals PO.LT PO.EQ False
instance equalsLTGT :: Equals PO.LT PO.GT False
instance equalsGTLT :: Equals PO.GT PO.LT False
instance equalsGTEQ :: Equals PO.GT PO.EQ False

equals :: forall l r o. Equals l r o => Proxy l -> Proxy r -> Proxy o
equals _ _ = Proxy
