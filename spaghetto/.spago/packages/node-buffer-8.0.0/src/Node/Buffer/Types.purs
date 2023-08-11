module Node.Buffer.Types
  ( Octet
  , Offset
  , BufferValueType(..)
  ) where

import Prelude

-- | Type synonym indicating the value should be an octet (0-255). If the value
-- | provided is outside this range it will be used as modulo 256.
type Octet = Int

-- | Type synonym indicating the value refers to an offset in a buffer.
type Offset = Int

-- | Enumeration of the numeric types that can be written to a buffer.
data BufferValueType
  = UInt8
  | UInt16LE
  | UInt16BE
  | UInt32LE
  | UInt32BE
  | Int8
  | Int16LE
  | Int16BE
  | Int32LE
  | Int32BE
  | FloatLE
  | FloatBE
  | DoubleLE
  | DoubleBE

instance showBufferValueType :: Show BufferValueType where
  show UInt8    = "UInt8"
  show UInt16LE = "UInt16LE"
  show UInt16BE = "UInt16BE"
  show UInt32LE = "UInt32LE"
  show UInt32BE = "UInt32BE"
  show Int8     = "Int8"
  show Int16LE  = "Int16LE"
  show Int16BE  = "Int16BE"
  show Int32LE  = "Int32LE"
  show Int32BE  = "Int32BE"
  show FloatLE  = "FloatLE"
  show FloatBE  = "FloatBE"
  show DoubleLE = "DoubleLE"
  show DoubleBE = "DoubleBE"
