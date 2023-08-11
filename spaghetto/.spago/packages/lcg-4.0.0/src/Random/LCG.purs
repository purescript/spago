module Random.LCG
  ( Seed
  , mkSeed
  , unSeed
  , randomSeed
  , lcgA
  , lcgC
  , lcgM
  , lcgNext
  , lcgPerturb
  ) where

import Prelude

import Effect (Effect)
import Effect.Random (randomInt)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (fromJust)
import Data.Number ((%))
import Partial.Unsafe (unsafePartial)

-- | A seed for the linear congruential generator. We omit a `Semiring`
-- | instance because there is no `zero` value, as 0 is not an acceptable
-- | seed for the generator.
newtype Seed = Seed Int

derive instance eqSeed :: Eq Seed
derive instance ordSeed :: Ord Seed

instance showSeed :: Show Seed where
  show (Seed x) = "Seed " <> show x

mkSeed :: Int -> Seed
mkSeed x = Seed (ensureBetween seedMin seedMax x)
  where
    ensureBetween :: Int -> Int -> Int -> Int
    ensureBetween min max n =
      let
        rangeSize = max - min
        n' = n `mod` rangeSize
      in
        if n' < min then n' + max else n'

unSeed :: Seed -> Int
unSeed (Seed x) = x

-- | Create a random seed
randomSeed :: Effect Seed
randomSeed = mkSeed <$> randomInt seedMin seedMax

-- | The minimum permissible Seed value.
seedMin :: Int
seedMin = 1

-- | The maximum permissible Seed value.
seedMax :: Int
seedMax = lcgM - 1

-- | The *multiplier*: a magic constant for the linear congruential generator
lcgA :: Int
lcgA = 48271

-- | The *increment*: a magic constant for the linear congruential generator
lcgC :: Int
lcgC = 0

-- | The *modulus*: a magic constant for the linear congruential generator.
-- | It is equal to 2^31 - 1, a Mersenne prime. It is useful for this value to
-- | be prime, because then the requirement of the initial seed being coprime
-- | to the modulus is satisfied when the seed is between 1 and lcgM - 1.
lcgM :: Int
lcgM = 2147483647

-- | Perturb a seed value
-- Note that `Int` operations are truncated to 32-bits, so we convert to
-- `Number` for this calculation to avoid overflow errors.
lcgPerturb :: Int -> Seed -> Seed
lcgPerturb d (Seed n) =
  Seed $ unsafePartial fromJust $ fromNumber $
    (toNumber lcgA * toNumber n + toNumber d) % toNumber lcgM

-- | Step the linear congruential generator
lcgNext :: Seed -> Seed
lcgNext = lcgPerturb lcgC
