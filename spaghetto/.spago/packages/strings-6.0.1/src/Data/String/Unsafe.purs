-- | Unsafe string and character functions.
module Data.String.Unsafe
  ( char
  , charAt
  ) where

-- | Returns the character at the given index.
-- |
-- | **Unsafe:** throws runtime exception if the index is out of bounds.
foreign import charAt :: Int -> String -> Char

-- | Converts a string of length `1` to a character.
-- |
-- | **Unsafe:** throws runtime exception if length is not `1`.
foreign import char :: String -> Char
