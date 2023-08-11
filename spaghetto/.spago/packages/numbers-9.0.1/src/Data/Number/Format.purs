-- | A module for formatting numbers as strings.
-- |
-- | Usage:
-- | ``` purs
-- | > let x = 1234.56789
-- |
-- | > toStringWith (precision 6) x
-- | "1234.57"
-- |
-- | > toStringWith (fixed 3) x
-- | "1234.568"
-- |
-- | > toStringWith (exponential 2) x
-- | "1.23e+3"
-- | ```
-- |
-- | The main method of this module is the `toStringWith` function that accepts
-- | a `Format` argument which can be constructed through one of the smart
-- | constructors `precision`, `fixed` and `exponential`. Internally, the
-- | number will be formatted with JavaScripts `toPrecision`, `toFixed` or
-- | `toExponential`.
module Data.Number.Format
  ( Format()
  , precision
  , fixed
  , exponential
  , toStringWith
  , toString
  ) where

import Prelude

foreign import toPrecisionNative ::   Int -> Number -> String
foreign import toFixedNative ::       Int -> Number -> String
foreign import toExponentialNative :: Int -> Number -> String

-- | The `Format` data type specifies how a number will be formatted.
data Format
  = Precision Int
  | Fixed Int
  | Exponential Int

-- | Create a `toPrecision`-based format from an integer. Values smaller than
-- | `1` and larger than `21` will be clamped.
precision :: Int -> Format
precision = Precision <<< clamp 1 21

-- | Create a `toFixed`-based format from an integer. Values smaller than `0`
-- | and larger than `20` will be clamped.
fixed :: Int -> Format
fixed = Fixed <<< clamp 0 20

-- | Create a `toExponential`-based format from an integer. Values smaller than
-- | `0` and larger than `20` will be clamped.
exponential :: Int -> Format
exponential = Exponential <<< clamp 0 20

-- | Convert a number to a string with a given format.
toStringWith :: Format -> Number -> String
toStringWith (Precision p)   = toPrecisionNative p
toStringWith (Fixed p)       = toFixedNative p
toStringWith (Exponential p) = toExponentialNative p

-- | Convert a number to a string via JavaScript's toString method.
-- |
-- | ```purs
-- | > toString 12.34
-- | "12.34"
-- |
-- | > toString 1234.0
-- | "1234"
-- |
-- | > toString 1.2e-10
-- | "1.2e-10"
-- | ```
foreign import toString :: Number -> String
