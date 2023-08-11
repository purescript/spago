-- | This module is a partial port of the Haskell QuickCheck library.
-- |
-- | QuickCheck provides a way to write _property-based_ tests.
-- |
-- | The `Arbitrary` and `CoArbitrary` type classes allow us to create
-- | random data with which we can run our tests. This module provides
-- | instances of both classes for PureScript's core data structures,
-- | as well as functions for writing new instances.
-- |
-- | Test suites can use the `quickCheck` and `quickCheckPure` functions
-- | to test properties.
-- |
-- | For example:
-- |
-- | ```purescript
-- | main = quickCheck \n -> n + 1 > n
-- | ```
module Test.QuickCheck
  ( quickCheck
  , quickCheckGen
  , quickCheck'
  , quickCheckGen'
  , quickCheckWithSeed
  , quickCheckGenWithSeed
  , quickCheckPure
  , quickCheckPure'
  , quickCheckGenPure
  , quickCheckGenPure'
  , ResultSummary
  , checkResults
  , printSummary
  , class Testable
  , test
  , Result(..)
  , withHelp
  , (<?>)
  , assertEquals
  , (===)
  , (==?)
  , assertNotEquals
  , (/==)
  , (/=?)
  , assertLessThan
  , (<?)
  , assertLessThanEq
  , (<=?)
  , assertGreaterThan
  , (>?)
  , assertGreaterThanEq
  , (>=?)
  , module Random.LCG
  , module Test.QuickCheck.Arbitrary
  ) where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Foldable (for_)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throwException, error)
import Random.LCG (Seed, mkSeed, unSeed, randomSeed)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, class Coarbitrary, coarbitrary)
import Test.QuickCheck.Gen (Gen, runGen)

-- | Test a property.
-- |
-- | This function generates a new random seed, runs 100 tests and
-- | prints the test results to the console.
quickCheck :: forall prop. Testable prop => prop -> Effect Unit
quickCheck prop = quickCheck' 100 prop

-- | A version of `quickCheck` with the property specialized to `Gen`.
-- |
-- | The `quickCheckGen` variants are useful for writing property tests where a
-- | `MonadGen` constraint (or QuickCheck's `Gen` directly) is being used,
-- | rather than relying on `Arbitrary` instances. Especially useful for the
-- | `MonadGen`-constrained properties as they will not infer correctly when
-- | used with the `quickCheck` functions unless an explicit type annotation is
-- | used.
quickCheckGen :: forall prop. Testable prop => Gen prop -> Effect Unit
quickCheckGen = quickCheck

-- | A variant of the `quickCheck` function which accepts an extra parameter
-- | representing the number of tests which should be run.
quickCheck' :: forall prop. Testable prop => Int -> prop -> Effect Unit
quickCheck' n prop = do
  seed <- randomSeed
  quickCheckWithSeed seed n prop

-- | A version of `quickCheck'` with the property specialized to `Gen`.
quickCheckGen' :: forall prop. Testable prop => Int -> Gen prop -> Effect Unit
quickCheckGen' = quickCheck'

-- | A variant of the `quickCheck'` function that accepts a specific seed as
-- | well as the number tests that should be run.
quickCheckWithSeed
  :: forall prop. Testable prop => Seed -> Int -> prop -> Effect Unit
quickCheckWithSeed initialSeed n prop = do
  let result = tailRec loop { seed: initialSeed, index: 0, successes: 0, firstFailure: mempty }
  log $ show result.successes <> "/" <> show n <> " test(s) passed."
  for_ result.firstFailure \{ index, message, seed: failureSeed } ->
    throwException $ error
      $ "Test " <> show (index + 1)
      <> " (seed " <> show (unSeed failureSeed) <> ") failed: \n"
      <> message
  where
  loop :: LoopState -> Step LoopState LoopState
  loop state@{ seed, index, successes, firstFailure }
    | index == n = Done state
    | otherwise =
        case runGen (test prop) { newSeed: seed, size: 10 } of
          Tuple Success s ->
            Loop
              { seed: s.newSeed
              , index: index + 1
              , successes: successes + 1
              , firstFailure
              }
          Tuple (Failed message) s ->
            Loop
              { seed: s.newSeed
              , index: index + 1
              , successes
              , firstFailure:
                  firstFailure <> First (Just { index, message, seed })
              }

-- | A version of `quickCheckWithSeed` with the property specialized to `Gen`.
quickCheckGenWithSeed :: forall prop. Testable prop => Seed -> Int -> Gen prop -> Effect Unit
quickCheckGenWithSeed = quickCheckWithSeed

type LoopState =
  { successes :: Int
  , firstFailure :: First { index :: Int, message :: String, seed :: Seed }
  , seed :: Seed
  , index :: Int
  }

-- | Test a property, returning all test results as a List.
-- |
-- | The first argument is the _random seed_ to be passed to the random generator.
-- | The second argument is the number of tests to run.
quickCheckPure :: forall prop. Testable prop => Seed -> Int -> prop -> List Result
quickCheckPure s n prop = map snd (quickCheckPure' s n prop)

-- | Test a property, returning all test results as a List, with the Seed that
-- | was used for each result.
-- |
-- | The first argument is the _random seed_ to be passed to the random generator.
-- | The second argument is the number of tests to run.
quickCheckPure' :: forall prop. Testable prop => Seed -> Int -> prop -> List (Tuple Seed Result)
quickCheckPure' s n prop = tailRec loop { seed: s, index: 0, results: mempty }
  where
    loop :: PureLoopState -> Step PureLoopState (List (Tuple Seed Result))
    loop { seed, index, results }
      | index == n = Done (List.reverse (results))
      | otherwise =
          case runGen (test prop) { newSeed: seed, size: 10 } of
            Tuple r {newSeed} ->
              Loop
                { seed: newSeed
                , index: index + 1
                , results: (Tuple seed r) : results
                }

type PureLoopState =
  { seed :: Seed
  , index :: Int
  , results :: List (Tuple Seed Result)
  }

-- | A version of `quickCheckPure` with the property specialized to `Gen`.
quickCheckGenPure :: forall prop. Testable prop => Seed -> Int -> Gen prop -> List Result
quickCheckGenPure = quickCheckPure

-- | A version of `quickCheckPure'` with the property specialized to `Gen`.
quickCheckGenPure' :: forall prop. Testable prop => Seed -> Int -> Gen prop -> List (Tuple Seed Result)
quickCheckGenPure' = quickCheckPure'

-- | A type used to summarise the results from `quickCheckPure'`
type ResultSummary =
  { total :: Int
  , successes :: Int
  , failures :: List { index :: Int, seed :: Seed, message :: String }
  }

-- | Processes the results from `quickCheckPure'` to produce a `ResultSummary`.
checkResults :: List (Tuple Seed Result) -> ResultSummary
checkResults = foldlWithIndex go { total: 0, successes: 0, failures: List.Nil }
  where
    go :: Int -> ResultSummary -> Tuple Seed Result -> ResultSummary
    go index st (Tuple seed result) =
      case result of
        Success ->
          st { total = st.total + 1, successes = st.successes + 1 }
        Failed message ->
          st { total = st.total + 1, failures = List.Cons { index, seed, message } st.failures }

-- | Print a one-line summary in the form "x/y test(s) passed."
printSummary :: ResultSummary -> String
printSummary summary =
  show summary.successes <> "/" <> show summary.total
    <> if summary.total == 1 then " test passed." else " tests passed."

-- | The `Testable` class represents _testable properties_.
-- |
-- | A testable property is a function of zero or more `Arbitrary` arguments,
-- | returning a `Boolean` or `Result`.
-- |
-- | Testable properties can be passed to the `quickCheck` function.
class Testable prop where
  test :: prop -> Gen Result

instance testableResult :: Testable Result where
  test = pure

instance testableBoolean :: Testable Boolean where
  test true = pure Success
  test false = pure $ Failed "Test returned false"

instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop) where
  test f = arbitrary >>= test <<< f

instance testableGen :: Testable prop => Testable (Gen prop) where
  test = flip bind test

-- | The result of a test: success or failure (with an error message).
data Result = Success | Failed String

instance showResult :: Show Result where
  show Success = "Success"
  show (Failed msg) = "Failed: " <> msg

-- | This operator attaches an error message to a failed test.
-- |
-- | For example:
-- |
-- | ```purescript
-- | test x = myProperty x <?> ("myProperty did not hold for " <> show x)
-- | ```
withHelp :: Boolean -> String -> Result
withHelp true _ = Success
withHelp false msg = Failed msg

infix 2 withHelp as <?>

-- | Self-documenting comparison operation
assertOp :: forall a. Eq a => Show a => (a -> a -> Boolean) -> String -> a -> a -> Result
assertOp op failString a b = a `op` b <?> show a <> failString <> show b

-- | Self-documenting equality assertion
assertEquals :: forall a. Eq a => Show a => a -> a -> Result
assertEquals = assertOp (==) " /= "

infix 2 assertEquals as ===
infix 2 assertEquals as ==?

-- | Self-documenting inequality assertion
assertNotEquals :: forall a. Eq a => Show a => a -> a -> Result
assertNotEquals = assertOp (/=) " == "

infix 2 assertNotEquals as /==
infix 2 assertNotEquals as /=?

assertLessThan :: forall a. Ord a => Show a => a -> a -> Result
assertLessThan = assertOp (<) " >= "

infix 2 assertLessThan as <?

assertLessThanEq :: forall a. Ord a => Show a => a -> a -> Result
assertLessThanEq = assertOp (<=) " > "

infix 2 assertLessThanEq as <=?

assertGreaterThan :: forall a. Ord a => Show a => a -> a -> Result
assertGreaterThan = assertOp (>) " <= "

infix 2 assertGreaterThan as >?

assertGreaterThanEq :: forall a. Ord a => Show a => a -> a -> Result
assertGreaterThanEq = assertOp (>=) " < "

infix 2 assertGreaterThanEq as >=?
