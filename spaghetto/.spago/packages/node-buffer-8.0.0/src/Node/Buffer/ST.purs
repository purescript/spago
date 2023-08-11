module Node.Buffer.ST
  ( STBuffer
  , run
  ) where

import Prelude

import Control.Monad.ST (ST, Region)
import Control.Monad.ST as ST
import Node.Buffer.Class (class MutableBuffer, unsafeFreeze)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.Internal as Internal

-- | A reference to a mutable buffer for use with `ST`
-- |
-- | The type parameter represents the memory region which the buffer belongs to.
foreign import data STBuffer :: Region -> Type

-- | Runs an effect creating an `STBuffer` then freezes the buffer and returns
-- | it, without unneccessary copying.
run :: (forall h. ST h (STBuffer h)) -> ImmutableBuffer
run st = ST.run (st >>= unsafeFreeze)

instance mutableBufferST :: MutableBuffer (STBuffer h) (ST h) where
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
