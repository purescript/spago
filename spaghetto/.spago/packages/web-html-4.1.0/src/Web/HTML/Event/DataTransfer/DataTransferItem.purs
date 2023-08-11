module Web.HTML.Event.DataTransfer.DataTransferItem
  ( DataTransferItem
  , DataTransferItemKind(..)
  , DataTransferItemList
  , dataTransferItem
  , kind
  , length
  , type_
  ) where

import Prelude

import Data.Function.Uncurried (Fn5)
import Data.Function.Uncurried as Uncurried
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as Nullable

data DataTransferItemKind = Text | File

derive instance eqDataTransferItemKind :: Eq DataTransferItemKind
derive instance ordDataTransferItemKind :: Ord DataTransferItemKind

instance showDataTransferItemKind :: Show DataTransferItemKind where
  show = case _ of
    Text -> "Text"
    File -> "File"

foreign import _kind
  :: Fn5 (forall x. Maybe x)
       (forall x. x -> Maybe x)
       DataTransferItemKind
       DataTransferItemKind
       DataTransferItem
       (Maybe DataTransferItemKind)

-- | Returns the drag data item kind of the `DataTransferItem`. In the case
-- | where the `DataTransferItem` object is in _disabled mode_, `Nothing` is
-- | returned.
kind :: DataTransferItem -> Maybe DataTransferItemKind
kind = Uncurried.runFn5 _kind Nothing Just Text File

-- | A Unicode string giving the type or format of the data, generally given by
-- | a MIME type. Some values that are not MIME types are special-cased for
-- | legacy reasons. The API does not enforce the use of MIME types; other
-- | values can be used as well. In all cases, however, the values are all
-- | converted to ASCII lowercase by the API.
-- | There is a limit of one text item per item type string.
foreign import type_ :: DataTransferItem -> String

foreign import _dataTransferItem :: Int -> DataTransferItemList -> Nullable DataTransferItem

-- | Access an item in the `DataTransferItemList` by index.
dataTransferItem :: Int -> DataTransferItemList -> Maybe DataTransferItem
dataTransferItem = map Nullable.toMaybe <$> _dataTransferItem

foreign import _length :: DataTransferItemList -> Int

length :: DataTransferItemList -> Int
length = _length

foreign import data DataTransferItem :: Type

foreign import data DataTransferItemList :: Type
