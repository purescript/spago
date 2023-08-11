module Web.File.FileList
  ( FileList
  , length
  , item
  , items
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Tuple (Tuple(..))
import Web.File.File (File)
import Data.Unfoldable (class Unfoldable, unfoldr)

foreign import data FileList :: Type

-- | Number of files in the `FileList` object.
foreign import length :: FileList -> Int

foreign import _item :: Int -> FileList -> Nullable File

-- | Get `File` at the certain position
item :: Int -> FileList -> Maybe File
item i = toMaybe <<< _item i

items :: forall t. Unfoldable t => FileList -> t File
items fl = unfoldr (\i -> (flip Tuple (i + 1)) <$> item i fl) 0

