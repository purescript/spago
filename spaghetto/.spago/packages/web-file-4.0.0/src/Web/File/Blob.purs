module Web.File.Blob
  ( Blob
  , fromString
  , fromArray
  , type_
  , size
  , StartByte(..)
  , EndByte(..)
  , ByteIdx
  , idxFromInt
  , idxFromNumber
  , slice
  , slice'
  ) where

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Number (round)
import Prelude ((==), (>>>))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Blob :: Type

-- | Creates a String with the given Mediatype
-- | For example:
-- | ```
-- | myBlob = fromString (unsafeStringify { name: "Carl", age: 25 }) (MediaType "application/json")
-- | ```
fromString :: String -> MediaType -> Blob
fromString str ct = blobImpl [str] ct

-- | Creates a Blob from an Array of strings with the given Mediatype
fromArray :: Array String -> MediaType -> Blob
fromArray args opts = blobImpl args opts

foreign import blobImpl :: Array String -> MediaType -> Blob
foreign import typeImpl :: Blob -> String

-- | `MediaType` of the data contained in the `Blob`.
-- | Returns `Nothing` if the `MediaType` is unknown.
type_ :: Blob -> Maybe MediaType
type_ blob =
  let
    blobType = typeImpl blob
  in
    if blobType == ""
      then Nothing
      else Just (MediaType blobType)

-- | The size (in bytes) of the data contained in the `Blob`.
foreign import size :: Blob -> Number

-- | An index into the Blob indicating the first byte to include in the new Blob.
-- | If you specify a negative value, it's treated as an offset from the end of the
-- | string toward the beginning. For example, -10 would be the 10th from last byte
-- | in the Blob. If you specify a value for start that is larger than the size
-- | of the source Blob, the returned Blob has size 0 and contains no data.
newtype StartByte = StartByte ByteIdx

-- | An index into the Blob indicating the first byte that will *not* be included
-- | in the new Blob (i.e. the byte exactly at this index is not included).
-- | If you specify a negative value, it's treated as an offset from the end of
-- | the string toward the beginning. For example, -10 would be the 10th from
-- | last byte in the Blob. The default value is size.
newtype EndByte = EndByte ByteIdx

foreign import data ByteIdx :: Type

-- | Creates `ByteIdx` from `Int` value
idxFromInt :: Int -> ByteIdx
idxFromInt = toNumber >>> unsafeCoerce

-- | Creates `ByteIdx` from `Number` value using `Math.round`.
idxFromNumber :: Number -> ByteIdx
idxFromNumber = round >>> unsafeCoerce

-- | Creates a new `Blob` object (with specified `MediaType`), containing the
-- | data in the specified range of bytes of the source Blob, by setting .
foreign import slice ∷ MediaType -> StartByte -> EndByte -> Blob -> Blob

-- | Creates a new `Blob` object containing the data in the specified range
-- | of bytes of the source Blob.
slice' ∷ StartByte -> EndByte -> Blob -> Blob
slice' = slice (MediaType "")
