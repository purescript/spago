module Data.String.Unicode
  ( toUpper
  , toLower
  , caseFold
  , caselessMatch
  , toUpperSimple
  , toLowerSimple
  , caseFoldSimple
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Data.CodePoint.Unicode as CP
import Data.String (CodePoint, fromCodePointArray, toCodePointArray)

-- Full Unicode conversions

-- | Convert each code point in the string to its corresponding uppercase
-- | sequence. This is the full (locale-independent) Unicode algorithm,
-- | and may map single code points to more than one code point. For example,
-- | `toUpper "ß" == "SS"`.
-- |
-- | Because this matches on more rules, it may be slower than `toUpperSimple`,
-- | but it provides more correct results.
toUpper :: String -> String
toUpper = convertFull CP.toUpper

-- | Convert each code point in the string to its corresponding lower
-- | sequence. This is the full (locale-independent) Unicode algorithm,
-- | and may map single code points to more than one code point. For example,
-- | `toLower "\x0130" == "\x0069\x0307"`.
-- |
-- | Because this matches on more rules, it may be slower than `toLowerSimple`,
-- | but it provides more correct results.
toLower :: String -> String
toLower = convertFull CP.toLower

-- | The full Unicode case folding algorithm, may increase the length of the
-- | string by mapping individual code points to longer sequences.
caseFold :: String -> String
caseFold = convertFull CP.caseFold

-- | Caseless matching, based on `caseFold`.
caselessMatch :: String -> String -> Boolean
caselessMatch s1 s2 = caseFold s1 == caseFold s2

-- Simple code-point-to-code-point conversion algorithms

-- | Convert each code point in the string to its corresponding uppercase
-- | code point. This will preserve the number of code points in the string.
-- |
-- | Note: this is not the full Unicode algorithm, see `toUpper`.
toUpperSimple :: String -> String
toUpperSimple = convert CP.toUpperSimple

-- | Convert each code point in the string to its corresponding lowercase
-- | code point. This will preserve the number of code points in the string.
-- |
-- | Note: this is not the full Unicode algorithm, see `toLower`.
toLowerSimple :: String -> String
toLowerSimple = convert CP.toLowerSimple

-- | Convert each code point in the string to its corresponding case-folded
-- | code point. This will preserve the number of code points in the string.
-- |
-- | Note: this is not the full Unicode algorithm, see `caseFold`.
caseFoldSimple :: String -> String
caseFoldSimple = convert CP.caseFoldSimple

-- Helper functions
convert :: (CodePoint -> CodePoint) -> String -> String
convert f = toCodePointArray >>> map f >>> fromCodePointArray

convertFull :: (CodePoint -> Array CodePoint) -> String -> String
convertFull f = toCodePointArray >>> bindFlipped f >>> fromCodePointArray
