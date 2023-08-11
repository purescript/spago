module Data.Ring
  ( class Ring
  , sub
  , negate
  , (-)
  , module Data.Semiring
  , class RingRecord
  , subRecord
  ) where

import Data.Semiring (class Semiring, class SemiringRecord, add, mul, one, zero, (*), (+))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Unit (Unit, unit)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Proxy (Proxy(..))

-- | The `Ring` class is for types that support addition, multiplication,
-- | and subtraction operations.
-- |
-- | Instances must satisfy the following laws in addition to the `Semiring`
-- | laws:
-- |
-- | - Additive inverse: `a - a = zero`
-- | - Compatibility of `sub` and `negate`: `a - b = a + (zero - b)`
class Semiring a <= Ring a where
  sub :: a -> a -> a

infixl 6 sub as -

instance ringInt :: Ring Int where
  sub = intSub

instance ringNumber :: Ring Number where
  sub = numSub

instance ringUnit :: Ring Unit where
  sub _ _ = unit

instance ringFn :: Ring b => Ring (a -> b) where
  sub f g x = f x - g x

instance ringProxy :: Ring (Proxy a) where
  sub _ _ = Proxy

instance ringRecord :: (RL.RowToList row list, RingRecord list row row) => Ring (Record row) where
  sub = subRecord (Proxy :: Proxy list)

-- | `negate x` can be used as a shorthand for `zero - x`.
negate :: forall a. Ring a => a -> a
negate a = zero - a

foreign import intSub :: Int -> Int -> Int
foreign import numSub :: Number -> Number -> Number

-- | A class for records where all fields have `Ring` instances, used to
-- | implement the `Ring` instance for records.
class RingRecord :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class SemiringRecord rowlist row subrow <= RingRecord rowlist row subrow | rowlist -> subrow where
  subRecord :: Proxy rowlist -> Record row -> Record row -> Record subrow

instance ringRecordNil :: RingRecord RL.Nil row () where
  subRecord _ _ _ = {}

instance ringRecordCons ::
  ( IsSymbol key
  , Row.Cons key focus subrowTail subrow
  , RingRecord rowlistTail row subrowTail
  , Ring focus
  ) =>
  RingRecord (RL.Cons key focus rowlistTail) row subrow where
  subRecord _ ra rb = insert (get ra - get rb) tail
    where
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    key = reflectSymbol (Proxy :: Proxy key)
    get = unsafeGet key :: Record row -> focus
    tail = subRecord (Proxy :: Proxy rowlistTail) ra rb
