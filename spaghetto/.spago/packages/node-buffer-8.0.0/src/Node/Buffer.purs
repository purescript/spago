-- | Mutable buffers and associated operations.
module Node.Buffer
  ( Buffer
  , module TypesExports
  , module Class
  ) where

import Effect (Effect)
import Node.Buffer.Class (class MutableBuffer)
import Node.Buffer.Class (class MutableBuffer, concat, concat', copy, create, fill, freeze, fromArray, fromArrayBuffer, fromString, getAtOffset, read, readString, setAtOffset, size, slice, thaw, toArray, toArrayBuffer, toString, unsafeFreeze, unsafeThaw, write, writeString) as Class
import Node.Buffer.Internal as Internal
import Node.Buffer.Types (BufferValueType(..), Octet, Offset) as TypesExports

-- | A reference to a mutable buffer for use with `Effect`
foreign import data Buffer :: Type

instance mutableBufferEffect :: MutableBuffer Buffer Effect where
  create = Internal.create
  freeze = Internal.copyAll
  unsafeFreeze = Internal.unsafeFreeze
  thaw = Internal.copyAll
  unsafeThaw = Internal.unsafeThaw
  fromArray = Internal.fromArray
  fromString = Internal.fromString
  fromArrayBuffer = Internal.fromArrayBuffer
  toArrayBuffer = Internal.toArrayBuffer
  read = Internal.read
  readString = Internal.readString
  toString = Internal.toString
  write = Internal.write
  writeString = Internal.writeString
  toArray = Internal.toArray
  getAtOffset = Internal.getAtOffset
  setAtOffset = Internal.setAtOffset
  slice = Internal.slice
  size = Internal.size
  concat = Internal.concat
  concat' = Internal.concat'
  copy = Internal.copy
  fill = Internal.fill
