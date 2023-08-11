-- | This module defines bitwise operations for the `Int` type.
module Data.Int.Bits
  ( and, (.&.)
  , or, (.|.)
  , xor, (.^.)
  , shl
  , shr
  , zshr
  , complement
  ) where

-- | Bitwise AND.
foreign import and :: Int -> Int -> Int

infixl 10 and as .&.

-- | Bitwise OR.
foreign import or :: Int -> Int -> Int

infixl 10 or as .|.

-- | Bitwise XOR.
foreign import xor :: Int -> Int -> Int

infixl 10 xor as .^.

-- | Bitwise shift left.
foreign import shl :: Int -> Int -> Int

-- | Bitwise shift right.
foreign import shr :: Int -> Int -> Int

-- | Bitwise zero-fill shift right.
foreign import zshr :: Int -> Int -> Int

-- | Bitwise NOT.
foreign import complement :: Int -> Int
