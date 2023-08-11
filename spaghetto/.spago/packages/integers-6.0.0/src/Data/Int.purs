module Data.Int
  ( fromNumber
  , ceil
  , floor
  , trunc
  , round
  , toNumber
  , fromString
  , Radix
  , radix
  , binary
  , octal
  , decimal
  , hexadecimal
  , base36
  , fromStringAs
  , toStringAs
  , Parity(..)
  , parity
  , even
  , odd
  , quot
  , rem
  , pow
  ) where

import Prelude

import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (isFinite)
import Data.Number as Number

-- | Creates an `Int` from a `Number` value. The number must already be an
-- | integer and fall within the valid range of values for the `Int` type
-- | otherwise `Nothing` is returned.
fromNumber :: Number -> Maybe Int
fromNumber = fromNumberImpl Just Nothing

foreign import fromNumberImpl
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Number
  -> Maybe Int

-- | Convert a `Number` to an `Int`, by taking the closest integer equal to or
-- | less than the argument. Values outside the `Int` range are clamped, `NaN`
-- | and `Infinity` values return 0.
floor :: Number -> Int
floor = unsafeClamp <<< Number.floor

-- | Convert a `Number` to an `Int`, by taking the closest integer equal to or
-- | greater than the argument. Values outside the `Int` range are clamped,
-- | `NaN` and `Infinity` values return 0.
ceil :: Number -> Int
ceil = unsafeClamp <<< Number.ceil

-- | Convert a `Number` to an `Int`, by dropping the decimal.
-- | Values outside the `Int` range are clamped, `NaN` and `Infinity`
-- | values return 0.
trunc :: Number -> Int
trunc = unsafeClamp <<< Number.trunc

-- | Convert a `Number` to an `Int`, by taking the nearest integer to the
-- | argument. Values outside the `Int` range are clamped, `NaN` and `Infinity`
-- | values return 0.
round :: Number -> Int
round = unsafeClamp <<< Number.round

-- | Convert an integral `Number` to an `Int`, by clamping to the `Int` range.
-- | This function will return 0 if the input is `NaN` or an `Infinity`.
unsafeClamp :: Number -> Int
unsafeClamp x
  | not (isFinite x) = 0
  | x >= toNumber top = top
  | x <= toNumber bottom = bottom
  | otherwise = fromMaybe 0 (fromNumber x)

-- | Converts an `Int` value back into a `Number`. Any `Int` is a valid `Number`
-- | so there is no loss of precision with this function.
foreign import toNumber :: Int -> Number

-- | Reads an `Int` from a `String` value. The number must parse as an integer
-- | and fall within the valid range of values for the `Int` type, otherwise
-- | `Nothing` is returned.
fromString :: String -> Maybe Int
fromString = fromStringAs (Radix 10)

-- | A type for describing whether an integer is even or odd.
-- |
-- | The `Ord` instance considers `Even` to be less than `Odd`.
-- |
-- | The `Semiring` instance allows you to ask about the parity of the results
-- | of arithmetical operations, given only the parities of the inputs. For
-- | example, the sum of an odd number and an even number is odd, so
-- | `Odd + Even == Odd`. This also works for multiplication, eg. the product
-- | of two odd numbers is odd, and therefore `Odd * Odd == Odd`.
-- |
-- | More generally, we have that
-- |
-- | ```purescript
-- | parity x + parity y == parity (x + y)
-- | parity x * parity y == parity (x * y)
-- | ```
-- |
-- | for any integers `x`, `y`. (A mathematician would say that `parity` is a
-- | *ring homomorphism*.)
-- |
-- | After defining addition and multiplication on `Parity` in this way, the
-- | `Semiring` laws now force us to choose `zero = Even` and `one = Odd`.
-- | This `Semiring` instance actually turns out to be a `Field`.
data Parity = Even | Odd

derive instance eqParity :: Eq Parity
derive instance ordParity :: Ord Parity

instance showParity :: Show Parity where
  show Even = "Even"
  show Odd = "Odd"

instance boundedParity :: Bounded Parity where
  bottom = Even
  top = Odd

instance semiringParity :: Semiring Parity where
  zero = Even
  add x y = if x == y then Even else Odd
  one = Odd
  mul Odd Odd = Odd
  mul _ _ = Even

instance ringParity :: Ring Parity where
  sub = add

instance commutativeRingParity :: CommutativeRing Parity

instance euclideanRingParity :: EuclideanRing Parity where
  degree Even = 0
  degree Odd = 1
  div x _ = x
  mod _ _ = Even

instance divisionRingParity :: DivisionRing Parity where
  recip = identity

-- | Returns whether an `Int` is `Even` or `Odd`.
-- |
-- | ``` purescript
-- | parity 0 == Even
-- | parity 1 == Odd
-- | ```
parity :: Int -> Parity
parity n = if even n then Even else Odd

-- | Returns whether an `Int` is an even number.
-- |
-- | ``` purescript
-- | even 0 == true
-- | even 1 == false
-- | ```
even :: Int -> Boolean
even x = x .&. 1 == 0

-- | The negation of `even`.
-- |
-- | ``` purescript
-- | odd 0 == false
-- | odd 1 == true
-- | ```
odd :: Int -> Boolean
odd x = x .&. 1 /= 0

-- | The number of unique digits (including zero) used to represent integers in
-- | a specific base.
newtype Radix = Radix Int

-- | The base-2 system.
binary :: Radix
binary = Radix 2

-- | The base-8 system.
octal :: Radix
octal = Radix 8

-- | The base-10 system.
decimal :: Radix
decimal = Radix 10

-- | The base-16 system.
hexadecimal :: Radix
hexadecimal = Radix 16

-- | The base-36 system.
base36 :: Radix
base36 = Radix 36

-- | Create a `Radix` from a number between 2 and 36.
radix :: Int -> Maybe Radix
radix n | n >= 2 && n <= 36 = Just (Radix n)
        | otherwise         = Nothing

-- | Like `fromString`, but the integer can be specified in a different base.
-- |
-- | Example:
-- | ``` purs
-- | fromStringAs binary      "100" == Just 4
-- | fromStringAs hexadecimal "ff"  == Just 255
-- | ```
fromStringAs :: Radix -> String -> Maybe Int
fromStringAs = fromStringAsImpl Just Nothing

-- | The `quot` function provides _truncating_ integer division (see the
-- | documentation for the `EuclideanRing` class). It is identical to `div` in
-- | the `EuclideanRing Int` instance if the dividend is positive, but will be
-- | slightly different if the dividend is negative. For example:
-- |
-- | ```purescript
-- | div 2 3 == 0
-- | quot 2 3 == 0
-- |
-- | div (-2) 3 == (-1)
-- | quot (-2) 3 == 0
-- |
-- | div 2 (-3) == 0
-- | quot 2 (-3) == 0
-- | ```
foreign import quot :: Int -> Int -> Int

-- | The `rem` function provides the remainder after _truncating_ integer
-- | division (see the documentation for the `EuclideanRing` class). It is
-- | identical to `mod` in the `EuclideanRing Int` instance if the dividend is
-- | positive, but will be slightly different if the dividend is negative. For
-- | example:
-- |
-- | ```purescript
-- | mod 2 3 == 2
-- | rem 2 3 == 2
-- |
-- | mod (-2) 3 == 1
-- | rem (-2) 3 == (-2)
-- |
-- | mod 2 (-3) == 2
-- | rem 2 (-3) == 2
-- | ```
foreign import rem :: Int -> Int -> Int

-- | Raise an Int to the power of another Int.
foreign import pow :: Int -> Int -> Int

foreign import fromStringAsImpl
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Radix
  -> String
  -> Maybe Int

foreign import toStringAs :: Radix -> Int -> String
