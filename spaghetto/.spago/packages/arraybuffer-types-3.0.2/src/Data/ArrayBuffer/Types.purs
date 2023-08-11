module Data.ArrayBuffer.Types where

-- | Represents a JS ArrayBuffer object
foreign import data ArrayBuffer :: Type

-- | Represents a JS DataView on an ArrayBuffer (a slice into the ArrayBuffer)
foreign import data DataView :: Type

-- | The unifying representation for the different typed arrays
foreign import data ArrayView :: ArrayViewType -> Type

-- | Offset in bytes into a DataView or ArrayBufer
type ByteOffset = Int

-- | Length in bytes of a DataView or ArrayBuffer
type ByteLength = Int

data ArrayViewType

foreign import data Int8 :: ArrayViewType
foreign import data Int16 :: ArrayViewType
foreign import data Int32 :: ArrayViewType
foreign import data Uint8 :: ArrayViewType
foreign import data Uint16 :: ArrayViewType
foreign import data Uint32 :: ArrayViewType
foreign import data Uint8Clamped :: ArrayViewType
foreign import data Float32 :: ArrayViewType
foreign import data Float64 :: ArrayViewType

type Int8Array = ArrayView Int8
type Int16Array = ArrayView Int16
type Int32Array = ArrayView Int32
type Uint8Array = ArrayView Uint8
type Uint16Array = ArrayView Uint16
type Uint32Array = ArrayView Uint32
type Uint8ClampedArray = ArrayView Uint8Clamped
type Float32Array = ArrayView Float32
type Float64Array = ArrayView Float64
