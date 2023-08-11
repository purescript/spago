module Web.File.FileReader
  ( FileReader
  , fromEventTarget
  , toEventTarget
  , fileReader
  , error
  , readyState
  , result
  , abort
  , readAsText
  , readAsArrayBuffer
  , readAsDataURL
  ) where

import Prelude

import Data.Maybe (Maybe, fromJust)
import Effect (Effect)
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (EventTarget)
import Web.File.Blob (Blob)
import Web.File.FileReader.ReadyState (toEnumReadyState, ReadyState)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data FileReader :: Type

fromEventTarget :: EventTarget -> Maybe FileReader
fromEventTarget = unsafeReadProtoTagged "FileReader"

toEventTarget :: FileReader -> EventTarget
toEventTarget = unsafeCoerce

foreign import fileReader :: Effect FileReader

foreign import error :: FileReader -> Effect Foreign

foreign import readyStateImpl :: FileReader -> Effect Int

readyState :: FileReader -> Effect ReadyState
readyState fr = do
  rs <- readyStateImpl fr
  pure $ unsafePartial $ fromJust $ toEnumReadyState rs

foreign import result :: FileReader -> Effect Foreign

foreign import abort :: FileReader -> Effect Unit

foreign import readAsText :: Blob -> FileReader -> Effect Unit

foreign import readAsArrayBuffer :: Blob -> FileReader -> Effect Unit

foreign import readAsDataURL :: Blob -> FileReader -> Effect Unit
