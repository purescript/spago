module Web.Storage.Event.StorageEvent
  ( StorageEvent
  , fromEvent
  , toEvent
  , key
  , oldValue
  , newValue
  , url
  , storageArea
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event)
import Web.Internal.FFI (unsafeReadProtoTagged)
import Web.Storage.Storage (Storage)

foreign import data StorageEvent :: Type

fromEvent :: Event -> Maybe StorageEvent
fromEvent = unsafeReadProtoTagged "StorageEvent"

toEvent :: StorageEvent -> Event
toEvent = unsafeCoerce

foreign import _key :: StorageEvent -> Nullable String

key :: StorageEvent -> Maybe String
key = toMaybe <<< _key

foreign import _oldValue :: StorageEvent -> Nullable String

oldValue :: StorageEvent -> Maybe String
oldValue = toMaybe <<< _oldValue

foreign import _newValue :: StorageEvent -> Nullable String

newValue :: StorageEvent -> Maybe String
newValue = toMaybe <<< _newValue

foreign import url :: StorageEvent -> String

foreign import _storageArea :: StorageEvent -> Nullable Storage

storageArea :: StorageEvent -> Maybe Storage
storageArea = toMaybe <<< _storageArea
