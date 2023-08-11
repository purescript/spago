-- | This module provides the `bench` function, which prints a short summary
-- | of the running times of a synchronous function to the console.
-- |
-- | For benchmarking tasks which require finer accuracy, or graphs as output,
-- | consider using `purescript-benchotron` instead.

module Performance.Minibench
  ( bench
  , benchWith
  , benchWith'
  , BenchResult
  , withUnits
  ) where

import Prelude hiding (min,max)

import Data.Int (toNumber)
import Effect (Effect, forE)
import Effect.Console (log)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Data.Number (infinity, max, min, sqrt)

-- | Returns the number of nanoseconds it takes to evaluate the given closure.
foreign import timeNs :: forall a. EffectFn1 (Unit -> a) Number

-- | Force garbage collection.
-- | Requires node to be run with the --force-gc flag.
foreign import gc :: Effect Unit

foreign import toFixed :: Number -> String

withUnits :: Number -> String
withUnits t
  | t < 1.0e3 = toFixed t <> " ns"
  | t < 1.0e6 = toFixed (t / 1.0e3) <> " μs"
  | t < 1.0e9 = toFixed (t / 1.0e6) <> " ms"
  | otherwise = toFixed (t / 1.0e9) <> " s"

-- | Estimate the running time of a function and print a summary to the console,
-- | specifying the number of samples to take. More samples will give a better
-- | estimate of both mean and standard deviation, but will increase running time.
-- |
-- | To increase benchmark accuracy by forcing garbage collection before the
-- | benchmark is run, node should be invoked with the '--expose-gc' flag.
benchWith
  :: forall a
   . Int
  -> (Unit -> a)
  -> Effect Unit
benchWith n f = do
  res <- benchWith' n f
  log ("mean   = " <> withUnits res.mean)
  log ("stddev = " <> withUnits res.stdDev)
  log ("min    = " <> withUnits res.min)
  log ("max    = " <> withUnits res.max)

type BenchResult =
  { mean :: Number
  , stdDev :: Number
  , min :: Number
  , max :: Number
  }

benchWith'
  :: forall a
   . Int
  -> (Unit -> a)
  -> Effect BenchResult
benchWith' n f = do
  sumRef <- Ref.new 0.0
  sum2Ref <- Ref.new 0.0
  minRef <- Ref.new infinity
  maxRef <- Ref.new 0.0
  gc
  forE 0 n \_ -> do
    ns <- runEffectFn1 timeNs f
    let square = ns * ns
    _ <- Ref.modify (_ + ns) sumRef
    _ <- Ref.modify (_ + square) sum2Ref
    _ <- Ref.modify (_ `min` ns) minRef
    _ <- Ref.modify (_ `max` ns) maxRef
    pure unit
  sum <- Ref.read sumRef
  sum2 <- Ref.read sum2Ref
  min' <- Ref.read minRef
  max' <- Ref.read maxRef
  let n'     = toNumber n
      mean   = sum / n'
      stdDev = sqrt ((sum2 - n' * mean * mean) / (n' - 1.0))
  pure
    { mean
    , stdDev
    , min: min'
    , max: max'
    }

-- | Estimate the running time of a function and print a summary to the console,
-- | by running the function 1000 times.
-- |
-- | For example:
-- |
-- | ```
-- | > import Data.Array
-- | > import Data.Foldable
-- | > import Performance.Minibench
-- | > bench \_ -> sum (1 .. 10000)
-- |
-- | mean   = 414.00 μs
-- | stddev = 494.82 μs
-- | ```
bench :: forall a. (Unit -> a) -> Effect Unit
bench = benchWith 1000
