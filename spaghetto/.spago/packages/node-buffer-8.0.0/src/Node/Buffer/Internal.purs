-- | Functions and types to support the other modules. Not for public use.
module Node.Buffer.Internal
  ( unsafeFreeze
  , unsafeThaw
  , usingFromImmutable
  , usingToImmutable
  , create
  , copyAll
  , fromArray
  , fromString
  , fromArrayBuffer
  , toArrayBuffer
  , read
  , readString
  , toString
  , write
  , writeString
  , toArray
  , getAtOffset
  , setAtOffset
  , slice
  , size
  , concat
  , concat'
  , copy
  , fill
  ) where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Buffer.Immutable as Immutable
import Node.Buffer.Types (BufferValueType, Octet, Offset)
import Node.Encoding (Encoding, encodingToNode)
import Unsafe.Coerce (unsafeCoerce)

unsafeFreeze :: forall buf m. Monad m => buf -> m ImmutableBuffer
unsafeFreeze = pure <<< unsafeCoerce

unsafeThaw :: forall buf m. Monad m => ImmutableBuffer -> m buf
unsafeThaw = pure <<< unsafeCoerce

usingFromImmutable :: forall buf m a. Monad m => (ImmutableBuffer -> a) -> buf -> m a
usingFromImmutable f buf = f <$> unsafeFreeze buf

usingToImmutable :: forall buf m a. Monad m => (a -> ImmutableBuffer) -> a -> m buf
usingToImmutable f x = unsafeThaw $ f x

create :: forall buf m. Monad m => Int -> m buf
create = usingToImmutable Immutable.create

foreign import copyAll :: forall a buf m. a -> m buf

fromArray :: forall buf m. Monad m => Array Octet -> m buf
fromArray = usingToImmutable Immutable.fromArray

fromString :: forall buf m. Monad m => String -> Encoding -> m buf
fromString s = usingToImmutable $ Immutable.fromString s

fromArrayBuffer :: forall buf m. Monad m => ArrayBuffer -> m buf
fromArrayBuffer = usingToImmutable Immutable.fromArrayBuffer

toArrayBuffer :: forall buf m. Monad m => buf -> m ArrayBuffer
toArrayBuffer = usingFromImmutable Immutable.toArrayBuffer

read :: forall buf m. Monad m => BufferValueType -> Offset -> buf -> m Number
read t o = usingFromImmutable $ Immutable.read t o

readString :: forall buf m. Monad m => Encoding -> Offset -> Offset -> buf -> m String
readString m o o' = usingFromImmutable $ Immutable.readString m o o'

toString :: forall buf m. Monad m => Encoding -> buf -> m String
toString m = usingFromImmutable $ Immutable.toString m

write :: forall buf m. Monad m => BufferValueType -> Number -> Offset -> buf -> m Unit
write = writeInternal <<< show

foreign import writeInternal :: forall buf m. String -> Number -> Offset -> buf -> m Unit

writeString :: forall buf m. Monad m => Encoding -> Offset -> Int -> String -> buf -> m Int
writeString = writeStringInternal <<< encodingToNode

foreign import writeStringInternal ::
  forall buf m. String -> Offset -> Int -> String -> buf -> m Int

toArray :: forall buf m. Monad m => buf -> m (Array Octet)
toArray = usingFromImmutable Immutable.toArray

getAtOffset :: forall buf m. Monad m => Offset -> buf -> m (Maybe Octet)
getAtOffset o = usingFromImmutable $ Immutable.getAtOffset o

foreign import setAtOffset :: forall buf m. Octet -> Offset -> buf -> m Unit

slice :: forall buf. Offset -> Offset -> buf -> buf
slice = unsafeCoerce Immutable.slice

size :: forall buf m. Monad m => buf -> m Int
size = usingFromImmutable Immutable.size

concat :: forall buf m. Array buf -> m buf
concat arrs = unsafeCoerce \_ -> Immutable.concat (unsafeCoerce arrs)

concat' :: forall buf m. Monad m => Array buf -> Int -> m buf
concat' arrs n = unsafeCoerce \_ -> Immutable.concat' (unsafeCoerce arrs) n

foreign import copy :: forall buf m. Offset -> Offset -> buf -> Offset -> buf -> m Int

foreign import fill :: forall buf m. Octet -> Offset -> Offset -> buf -> m Unit
