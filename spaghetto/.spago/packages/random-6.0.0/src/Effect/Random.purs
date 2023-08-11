module Effect.Random where

import Prelude

import Effect (Effect)

import Data.Int (toNumber, floor)

-- | Returns a random number between 0 (inclusive) and 1 (exclusive). This is
-- | a direct wrapper around JavaScript's `Math.random()`.
foreign import random :: Effect Number

-- | Takes a range specified by `low` (the first argument) and `high` (the
-- | second), and returns a random integer uniformly distributed in the closed
-- | interval `[low, high]`. It is unspecified what happens if `low > high`,
-- | or if either of `low` or `high` is not an integer.
-- |
-- | For example:
-- | ``` purescript
-- | randomInt 1 10 >>= Console.print
-- | ```
-- | will print a random integer between 1 and 10.
randomInt :: Int -> Int -> Effect Int
randomInt low high = do
  n <- random
  let asNumber = (toNumber high - toNumber low + one) * n + toNumber low
  pure $ floor asNumber

-- | Returns a random number between a minimum value (inclusive) and a maximum
-- | value (exclusive). It is unspecified what happens if `maximum < minimum`.
-- |
-- | For example:
-- | ``` purescript
-- | randomRange 1.0 2.0 >>= Console.print
-- | ```
-- | will print a random number between 1 and 2.
randomRange :: Number -> Number -> Effect Number
randomRange min max = do
    n <- random
    pure (n * (max - min) + min)

-- | Returns a random boolean value with an equal chance of being `true` or
-- | `false`.
randomBool :: Effect Boolean
randomBool = (_ < 0.5) <$> random
