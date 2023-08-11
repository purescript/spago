module Data.Show
  ( class Show
  , show
  , class ShowRecordFields
  , showRecordFields
  ) where

import Data.Semigroup ((<>))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Unit (Unit)
import Data.Void (Void, absurd)
import Prim.Row (class Nub)
import Prim.RowList as RL
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))

-- | The `Show` type class represents those types which can be converted into
-- | a human-readable `String` representation.
-- |
-- | While not required, it is recommended that for any expression `x`, the
-- | string `show x` be executable PureScript code which evaluates to the same
-- | value as the expression `x`.
class Show a where
  show :: a -> String

instance showUnit :: Show Unit where
  show _ = "unit"

instance showBoolean :: Show Boolean where
  show true = "true"
  show false = "false"

instance showInt :: Show Int where
  show = showIntImpl

instance showNumber :: Show Number where
  show = showNumberImpl

instance showChar :: Show Char where
  show = showCharImpl

instance showString :: Show String where
  show = showStringImpl

instance showArray :: Show a => Show (Array a) where
  show = showArrayImpl show

instance showProxy :: Show (Proxy a) where
  show _ = "Proxy"

instance showVoid :: Show Void where
  show = absurd

instance showRecord ::
  ( Nub rs rs
  , RL.RowToList rs ls
  , ShowRecordFields ls rs
  ) =>
  Show (Record rs) where
  show record = "{" <> showRecordFields (Proxy :: Proxy ls) record <> "}"

-- | A class for records where all fields have `Show` instances, used to
-- | implement the `Show` instance for records.
class ShowRecordFields :: RL.RowList Type -> Row Type -> Constraint
class ShowRecordFields rowlist row where
  showRecordFields :: Proxy rowlist -> Record row -> String

instance showRecordFieldsNil :: ShowRecordFields RL.Nil row where
  showRecordFields _ _ = ""
else
instance showRecordFieldsConsNil ::
  ( IsSymbol key
  , Show focus
  ) =>
  ShowRecordFields (RL.Cons key focus RL.Nil) row where
  showRecordFields _ record = " " <> key <> ": " <> show focus <> " "
    where
    key = reflectSymbol (Proxy :: Proxy key)
    focus = unsafeGet key record :: focus
else
instance showRecordFieldsCons ::
  ( IsSymbol key
  , ShowRecordFields rowlistTail row
  , Show focus
  ) =>
  ShowRecordFields (RL.Cons key focus rowlistTail) row where
  showRecordFields _ record = " " <> key <> ": " <> show focus <> "," <> tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    focus = unsafeGet key record :: focus
    tail = showRecordFields (Proxy :: Proxy rowlistTail) record

foreign import showIntImpl :: Int -> String
foreign import showNumberImpl :: Number -> String
foreign import showCharImpl :: Char -> String
foreign import showStringImpl :: String -> String
foreign import showArrayImpl :: forall a. (a -> String) -> Array a -> String
