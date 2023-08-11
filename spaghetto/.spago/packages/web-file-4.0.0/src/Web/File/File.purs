module Web.File.File where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe)
import Data.MediaType (MediaType)
import Unsafe.Coerce (unsafeCoerce)
import Web.File.Blob as Blob

foreign import data File :: Type

toBlob :: File -> Blob.Blob
toBlob = unsafeCoerce

foreign import name :: File -> String

foreign import lastModified :: File -> Instant

-- | (Inherited from `Blob`) `MediaType` of the data contained in the `Blob`.
-- | Returns `Nothing` if the `MediaType` is unknown.
type_ :: File -> Maybe MediaType
type_ = unsafeCoerce >>> Blob.type_

-- | (Inherited from `Blob`) The size (in bytes) of the data contained in the `File`.
size :: File -> Number
size = unsafeCoerce >>> Blob.size
