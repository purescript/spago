-- | Immutable buffers and associated operations.
module Node.Buffer.Immutable
  ( ImmutableBuffer
  , create
  , fromArray
  , fromString
  , fromArrayBuffer
  , read
  , readString
  , toString
  , toArray
  , toArrayBuffer
  , getAtOffset
  , concat
  , concat'
  , slice
  , size
  ) where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe(..))
import Node.Buffer.Types (BufferValueType, Octet, Offset)
import Node.Encoding (Encoding, encodingToNode)

-- | An immutable buffer that exists independently of any memory region or effect.
foreign import data ImmutableBuffer :: Type

instance showBuffer :: Show ImmutableBuffer where
  show = showImpl

foreign import showImpl :: ImmutableBuffer -> String

instance eqBuffer :: Eq ImmutableBuffer where
  eq = eqImpl

foreign import eqImpl :: ImmutableBuffer -> ImmutableBuffer -> Boolean

instance ordBuffer :: Ord ImmutableBuffer where
  compare a b =
    case compareImpl a b of
      x | x < 0 -> LT
      x | x > 0 -> GT
      _ -> EQ

foreign import compareImpl :: ImmutableBuffer -> ImmutableBuffer -> Int

-- | Creates a new buffer of the specified size.
foreign import create :: Int -> ImmutableBuffer

-- | Creates a new buffer from an array of octets, sized to match the array.
foreign import fromArray :: Array Octet -> ImmutableBuffer

-- | Creates a buffer view from a JS ArrayByffer without copying data.
--
-- Requires Node >= v5.10.0
foreign import fromArrayBuffer :: ArrayBuffer -> ImmutableBuffer

-- | Creates a new buffer from a string with the specified encoding, sized to match the string.
fromString :: String -> Encoding -> ImmutableBuffer
fromString str = fromStringImpl str <<< encodingToNode

foreign import fromStringImpl :: String -> String -> ImmutableBuffer

-- | Reads a numeric value from a buffer at the specified offset.
read :: BufferValueType -> Offset -> ImmutableBuffer -> Number
read = readImpl <<< show

foreign import readImpl :: String -> Offset -> ImmutableBuffer -> Number

-- | Reads a section of a buffer as a string with the specified encoding.
readString :: Encoding -> Offset -> Offset -> ImmutableBuffer -> String
readString = readStringImpl <<< encodingToNode

foreign import readStringImpl ::
  String -> Offset -> Offset -> ImmutableBuffer -> String

-- | Reads the buffer as a string with the specified encoding.
toString :: Encoding -> ImmutableBuffer -> String
toString = toStringImpl <<< encodingToNode

foreign import toStringImpl :: String -> ImmutableBuffer -> String

-- | Creates an array of octets from a buffer's contents.
foreign import toArray :: ImmutableBuffer -> Array Octet

-- | Creates an `ArrayBuffer` by copying a buffer's contents.
foreign import toArrayBuffer :: ImmutableBuffer -> ArrayBuffer

-- | Reads an octet from a buffer at the specified offset.
getAtOffset :: Offset -> ImmutableBuffer -> Maybe Octet
getAtOffset = getAtOffsetImpl Just Nothing

foreign import getAtOffsetImpl ::
  (Octet -> Maybe Octet) -> Maybe Octet -> Offset -> ImmutableBuffer -> Maybe Octet

-- | Concatenates a list of buffers.
foreign import concat :: Array ImmutableBuffer -> ImmutableBuffer

-- | Concatenates a list of buffers, combining them into a new buffer of the
-- | specified length.
concat' :: Array ImmutableBuffer -> Int -> ImmutableBuffer
concat' = concatToLength

foreign import concatToLength :: Array ImmutableBuffer -> Int -> ImmutableBuffer

-- | Creates a new buffer slice that shares the memory of the original buffer.
foreign import slice :: Offset -> Offset -> ImmutableBuffer -> ImmutableBuffer

-- | Returns the size of a buffer.
foreign import size :: ImmutableBuffer -> Int
