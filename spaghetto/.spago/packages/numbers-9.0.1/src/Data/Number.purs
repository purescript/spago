-- | Functions for working with PureScripts builtin `Number` type.
module Data.Number
  ( fromString
  , nan
  , isNaN
  , infinity
  , isFinite
  , abs
  , acos
  , asin
  , atan
  , atan2
  , ceil
  , cos
  , exp
  , floor
  , log
  , max
  , min
  , pow
  , remainder, (%)
  , round
  , sign
  , sin
  , sqrt
  , tan
  , trunc
  , e
  , ln2
  , ln10
  , log10e
  , log2e
  , pi
  , sqrt1_2
  , sqrt2
  , tau
  ) where

import Data.Function.Uncurried (Fn4, runFn4)
import Data.Maybe (Maybe(..))

-- | Not a number (NaN).
-- | ```purs
-- | > nan
-- | NaN
-- | ```
foreign import nan :: Number

-- | Test whether a number is NaN.
-- | ```purs
-- | > isNaN 0.0
-- | false
-- |
-- | > isNaN nan
-- | true
-- | ```
foreign import isNaN :: Number -> Boolean

-- | Positive infinity. For negative infinity use `(-infinity)`
-- | ```purs
-- | > infinity
-- | Infinity
-- |
-- | > (-infinity)
-- | - Infinity
-- | ```
foreign import infinity :: Number

-- | Test whether a number is finite.
-- | ```purs
-- | > isFinite 0.0
-- | true
-- |
-- | > isFinite infinity
-- | false
-- |
-- | > isFinite (-infinity)
-- | false
-- |
-- | > isFinite nan
-- | false
-- | ```
foreign import isFinite :: Number -> Boolean

-- | Attempt to parse a `Number` using JavaScripts `parseFloat`. Returns
-- | `Nothing` if the parse fails or if the result is not a finite number.
-- |
-- | Example:
-- | ```purs
-- | > fromString "123"
-- | (Just 123.0)
-- |
-- | > fromString "12.34"
-- | (Just 12.34)
-- |
-- | > fromString "1e4"
-- | (Just 10000.0)
-- |
-- | > fromString "1.2e4"
-- | (Just 12000.0)
-- |
-- | > fromString "bad"
-- | Nothing
-- | ```
-- |
-- | Note that `parseFloat` allows for trailing non-digit characters and
-- | whitespace as a prefix:
-- | ```
-- | > fromString "  1.2 ??"
-- | (Just 1.2)
-- | ```
fromString :: String -> Maybe Number
fromString str = runFn4 fromStringImpl str isFinite Just Nothing

foreign import fromStringImpl :: Fn4 String (Number -> Boolean) (forall a. a -> Maybe a) (forall a. Maybe a) (Maybe Number)

-- | Returns the absolute value of the argument.
-- | ```purs
-- | > x = -42.0
-- | > sign x * abs x == x
-- | true
-- | ```
foreign import abs :: Number -> Number

-- | Returns the inverse cosine in radians of the argument.
-- | ```purs
-- | > acos 0.0 == pi / 2.0
-- | true
-- | ```
foreign import acos :: Number -> Number

-- | Returns the inverse sine in radians of the argument.
-- | ```purs
-- | > asin 1.0 == pi / 2.0
-- | true
-- | ```
foreign import asin :: Number -> Number

-- | Returns the inverse tangent in radians of the argument.
-- | ```purs
-- | > atan 1.0 == pi / 4.0
-- | true
-- | ```
foreign import atan :: Number -> Number

-- | Four-quadrant tangent inverse. Given the arguments `y` and `x`, returns
-- | the inverse tangent of `y / x`, where the signs of both arguments are used
-- | to determine the sign of the result.
-- | If the first argument is negative, the result will be negative.
-- | The result is the angle between the positive x axis and a point `(x, y)`.
-- | ```purs
-- | > atan2 0.0 1.0
-- | 0.0
-- | > atan2 1.0 0.0 == pi / 2.0
-- | true
-- | ```
foreign import atan2 :: Number -> Number -> Number

-- | Returns the smallest integer not smaller than the argument.
-- | ```purs
-- | > ceil 1.5
-- | 2.0
-- | ```
foreign import ceil :: Number -> Number

-- | Returns the cosine of the argument, where the argument is in radians.
-- | ```purs
-- | > cos (pi / 4.0) == sqrt2 / 2.0
-- | true
-- | ```
foreign import cos :: Number -> Number

-- | Returns `e` exponentiated to the power of the argument.
-- | ```purs
-- | > exp 1.0
-- | 2.718281828459045
-- | ```
foreign import exp :: Number -> Number

-- | Returns the largest integer not larger than the argument.
-- | ```purs
-- | > floor 1.5
-- | 1.0
-- | ```
foreign import floor :: Number -> Number

-- | Returns the natural logarithm of a number.
-- | ```purs
-- | > log e
-- | 1.0
foreign import log :: Number -> Number

-- | Returns the largest of two numbers. Unlike `max` in Data.Ord this version
-- | returns NaN if either argument is NaN.
foreign import max :: Number -> Number -> Number

-- | Returns the smallest of two numbers. Unlike `min` in Data.Ord this version
-- | returns NaN if either argument is NaN.
foreign import min :: Number -> Number -> Number

-- | Return  the first argument exponentiated to the power of the second argument.
-- | ```purs
-- | > pow 3.0 2.0
-- | 9.0
-- | > sqrt 42.0 == pow 42.0 0.5
-- | true
-- | ```

foreign import pow :: Number -> Number -> Number

-- | Computes the remainder after division. This is the same as JavaScript's `%` operator.
-- ```purs
-- > 5.3 % 2.0
-- 1.2999999999999998
-- ```
foreign import remainder :: Number -> Number -> Number

infixl 7 remainder as %

-- | Returns the integer closest to the argument.
-- | ```purs
-- | > round 1.5
-- | 2.0
-- | ```
foreign import round :: Number -> Number

-- | Returns either a positive or negative +/- 1, indicating the sign of the
-- | argument. If the argument is 0, it will return a +/- 0. If the argument is
-- | NaN it will return NaN.
-- | ```purs
-- | > x = -42.0
-- | > sign x * abs x == x
-- | true
-- | ```
foreign import sign :: Number -> Number

-- | Returns the sine of the argument, where the argument is in radians.
-- | ```purs
-- | > sin (pi / 2.0)
-- | 1.0
-- | ```
foreign import sin :: Number -> Number

-- | Returns the square root of the argument.
-- | ```purs
-- | > sqrt 49.0
-- | 7.0
-- | ```
foreign import sqrt :: Number -> Number

-- | Returns the tangent of the argument, where the argument is in radians.
-- | ```
-- | > tan (pi / 4.0)
-- | 0.9999999999999999
-- | ```
foreign import tan :: Number -> Number

-- | Truncates the decimal portion of a number. Equivalent to `floor` if the
-- | number is positive, and `ceil` if the number is negative.
-- | ```purs
-- | ceil 1.5
-- | 2.0
-- | ```
foreign import trunc :: Number -> Number

-- | The base of the natural logarithm, also known as Euler's number or *e*.
-- | ```purs
-- | > log e
-- | 1.0
-- |
-- | > exp 1.0 == e
-- | true
-- |
-- | > e
-- | 2.718281828459045
-- | ```
e :: Number
e = 2.718281828459045

-- | The natural logarithm of 2.
-- | ```purs
-- | > log 2.0 == ln2
-- | true
-- |
-- | > ln2
-- | 0.6931471805599453
-- | ```
ln2 :: Number
ln2 = 0.6931471805599453

-- | The natural logarithm of 10.
-- | ```purs
-- | > log 10.0 == ln10
-- | true
-- |
-- | > ln10
-- | 2.302585092994046
-- | ```
ln10 :: Number
ln10 = 2.302585092994046

-- | Base 10 logarithm of `e`.
-- | ```purs
-- | > 1.0 / ln10 - log10e
-- | -5.551115123125783e-17
-- |
-- | > log10e
-- | 0.4342944819032518
-- | ```
log10e :: Number
log10e = 0.4342944819032518

-- | The base 2 logarithm of `e`.
-- | ```purs
-- | > 1.0 / ln2 == log2e
-- | true
-- |
-- | > log2e
-- | 1.4426950408889634
-- | ```
log2e :: Number
log2e = 1.4426950408889634

-- | The ratio of the circumference of a circle to its diameter.
-- | ```purs
-- | > pi
-- | 3.141592653589793
-- | ```
pi :: Number
pi = 3.141592653589793

-- | The square root of one half.
-- | ```purs
-- | > sqrt 0.5 == sqrt1_2
-- | true
-- |
-- | > sqrt1_2
-- | 0.7071067811865476
-- | ```
sqrt1_2 :: Number
sqrt1_2 = 0.7071067811865476

-- | The square root of two.
-- | ```purs
-- | > sqrt 2.0 == sqrt2
-- | true
-- |
-- | > sqrt2
-- | 1.4142135623730951
-- | ```
sqrt2 :: Number
sqrt2 = 1.4142135623730951

-- | The ratio of the circumference of a circle to its radius.
-- | ```purs
-- | > 2.0 * pi == tau
-- | true
-- |
-- | > tau
-- | 6.283185307179586
-- | ```
tau :: Number
tau = 6.283185307179586
