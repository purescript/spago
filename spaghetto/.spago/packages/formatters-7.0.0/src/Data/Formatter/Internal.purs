module Data.Formatter.Internal where

import Prelude

import Data.Foldable (class Foldable, foldl)

foldDigits :: forall f. Foldable f => f Int -> Int
foldDigits = foldl (\acc d -> acc * 10 + d) zero

repeat :: forall a. Monoid a => a -> Int -> a
repeat = repeat' mempty
  where
  repeat' :: a -> a -> Int -> a
  repeat' accum _ count
    | count < one = accum
  repeat' accum part count =
    repeat' (accum <> part) part (count - one)
