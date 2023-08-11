module Data.Bounded
  ( class Bounded
  , bottom
  , top
  , module Data.Ord
  , class BoundedRecord
  , bottomRecord
  , topRecord
  ) where

import Data.Ord (class Ord, class OrdRecord, Ordering(..), compare, (<), (<=), (>), (>=))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Unit (Unit, unit)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy(..))

-- | The `Bounded` type class represents totally ordered types that have an
-- | upper and lower boundary.
-- |
-- | Instances should satisfy the following law in addition to the `Ord` laws:
-- |
-- | - Bounded: `bottom <= a <= top`
class Ord a <= Bounded a where
  top :: a
  bottom :: a

instance boundedBoolean :: Bounded Boolean where
  top = true
  bottom = false

-- | The `Bounded` `Int` instance has `top :: Int` equal to 2^31 - 1,
-- | and `bottom :: Int` equal to -2^31, since these are the largest and smallest
-- | integers representable by twos-complement 32-bit integers, respectively.
instance boundedInt :: Bounded Int where
  top = topInt
  bottom = bottomInt

foreign import topInt :: Int
foreign import bottomInt :: Int

-- | Characters fall within the Unicode range.
instance boundedChar :: Bounded Char where
  top = topChar
  bottom = bottomChar

foreign import topChar :: Char
foreign import bottomChar :: Char

instance boundedOrdering :: Bounded Ordering where
  top = GT
  bottom = LT

instance boundedUnit :: Bounded Unit where
  top = unit
  bottom = unit

foreign import topNumber :: Number
foreign import bottomNumber :: Number

instance boundedNumber :: Bounded Number where
  top = topNumber
  bottom = bottomNumber

instance boundedProxy :: Bounded (Proxy a) where
  bottom = Proxy
  top = Proxy

class BoundedRecord :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class OrdRecord rowlist row <= BoundedRecord rowlist row subrow | rowlist -> subrow where
  topRecord :: Proxy rowlist -> Proxy row -> Record subrow
  bottomRecord :: Proxy rowlist -> Proxy row -> Record subrow

instance boundedRecordNil :: BoundedRecord RL.Nil row () where
  topRecord _ _ = {}
  bottomRecord _ _ = {}

instance boundedRecordCons ::
  ( IsSymbol key
  , Bounded focus
  , Row.Cons key focus rowTail row
  , Row.Cons key focus subrowTail subrow
  , BoundedRecord rowlistTail row subrowTail
  ) =>
  BoundedRecord (RL.Cons key focus rowlistTail) row subrow where
  topRecord _ rowProxy = insert top tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    tail = topRecord (Proxy :: Proxy rowlistTail) rowProxy

  bottomRecord _ rowProxy = insert bottom tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
    tail = bottomRecord (Proxy :: Proxy rowlistTail) rowProxy

instance boundedRecord ::
  ( RL.RowToList row list
  , BoundedRecord list row row
  ) =>
  Bounded (Record row) where
  top = topRecord (Proxy :: Proxy list) (Proxy :: Proxy row)
  bottom = bottomRecord (Proxy :: Proxy list) (Proxy :: Proxy row)
