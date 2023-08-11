-- | This module defines the random generator monad used by the `Test.QuickCheck`
-- | module, as well as helper functions for constructing random generators.
module Test.QuickCheck.Gen
  ( Gen
  , unGen
  , GenState
  , Size
  , repeatable
  , stateful
  , variant
  , suchThat
  , sized
  , resize
  , choose
  , chooseInt
  , oneOf
  , frequency
  , arrayOf
  , arrayOf1
  , enum
  , listOf
  , vectorOf
  , elements
  , shuffle
  , runGen
  , evalGen
  , perturbGen
  , uniform
  , sample
  , randomSample
  , randomSample'
  , randomSampleOne
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Lazy (class Lazy)
import Control.Monad.Gen.Class (class MonadGen)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (State, runState, evalState)
import Control.Monad.State.Class (modify, state)
import Data.Array ((:), length, zip, sortBy)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Semigroup.Foldable (foldMap1)
import Data.Int (toNumber, floor)
import Data.List (List(..), toUnfoldable)
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Number ((%))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Random.LCG (Seed, lcgPerturb, lcgM, lcgNext, unSeed, randomSeed)

-- | Tests are parameterized by the `Size` of the randomly-generated data,
-- | the meaning of which depends on the particular generator used.
type Size = Int

-- | The state of the random generator monad
type GenState = { newSeed :: Seed, size :: Size }

-- | The random generator monad
-- |
-- | `Gen` is a state monad which encodes a linear congruential generator.
newtype Gen a = Gen (State GenState a)

derive newtype instance functorGen :: Functor Gen
derive newtype instance applyGen :: Apply Gen
derive newtype instance applicativeGen :: Applicative Gen
derive newtype instance bindGen :: Bind Gen
derive newtype instance monadGen :: Monad Gen
derive newtype instance altGen :: Alt Gen
derive newtype instance monadRecGen :: MonadRec Gen
derive newtype instance lazyGen :: Lazy (Gen a)

instance monadGenGen :: MonadGen Gen where
  chooseInt = chooseInt
  chooseFloat = choose
  chooseBool = (_ < 0.5) <$> uniform
  resize f g = sized \s -> resize (f s) g
  sized = sized

-- | Exposes the underlying State implementation.
unGen :: forall a. Gen a -> State GenState a
unGen (Gen st) = st

-- | Create a random generator for a function type.
repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
repeatable f = Gen $ state \s -> Tuple (\a -> fst (runGen (f a) s)) (s { newSeed = lcgNext s.newSeed })

-- | Create a random generator which uses the generator state explicitly.
stateful :: forall a. (GenState -> Gen a) -> Gen a
stateful f = Gen $ state \s -> runGen (f s) s

-- | Modify a random generator by setting a new random seed.
variant :: forall a. Seed -> Gen a -> Gen a
variant n g = Gen $ state \s -> runGen g s { newSeed = n }

-- | Ensure that a generator only produces values that match a predicate. If
-- | the predicate always returns false the generator will loop forever.
suchThat :: forall a. Gen a -> (a -> Boolean) -> Gen a
suchThat gen pred = tailRecM go unit
  where
  go :: Unit -> Gen (Step Unit a)
  go _ = do
    a <- gen
    pure if pred a then Done a else Loop unit

-- | Create a random generator which depends on the size parameter.
sized :: forall a. (Size -> Gen a) -> Gen a
sized f = stateful (\s -> f s.size)

-- | Modify a random generator by setting a new size parameter.
resize :: forall a. Size -> Gen a -> Gen a
resize sz g = Gen $ state \{ newSeed, size } ->
  (_ {size = size} ) <$> runGen g { newSeed, size: sz}

-- | Create a random generator which samples a range of `Number`s i
-- | with uniform probability.
choose :: Number -> Number -> Gen Number
choose a b = (*) (max' - min') >>> (+) min' >>> unscale <$> uniform where
  unscale = (_ * 2.0)
  scale = (_ * 0.5)
  min' = scale $ min a b
  max' = scale $ max a b

-- | Create a random generator which chooses uniformly distributed
-- | integers from the closed interval `[a, b]`.
-- | Note that very large intervals will cause a loss of uniformity.
chooseInt :: Int -> Int -> Gen Int
chooseInt a b = if a <= b then chooseInt' a b else chooseInt' b a

-- guaranteed a <= b
chooseInt' :: Int -> Int -> Gen Int
chooseInt' a b = floor <<< clamp <$> choose32BitPosNumber
  where
    choose32BitPosNumber :: Gen Number
    choose32BitPosNumber =
      (+) <$> choose31BitPosNumber <*> (((*) 2.0) <$> choose31BitPosNumber)

    choose31BitPosNumber :: Gen Number
    choose31BitPosNumber = toNumber <$> lcgStep

    clamp :: Number -> Number
    clamp x = numA + (x % (numB - numA + one))

    numA = toNumber a
    numB = toNumber b

-- | Create a random generator which selects and executes a random generator from
-- | a non-empty array of random generators with uniform probability.
oneOf :: forall a. NonEmptyArray (Gen a) -> Gen a
oneOf xs = do
  n <- chooseInt zero (NEA.length xs - one)
  unsafePartial $ NEA.unsafeIndex xs n

-- | Create a random generator which selects and executes a random generator from
-- | a non-empty, weighted list of random generators.
frequency :: forall a. NonEmptyArray (Tuple Number (Gen a)) -> Gen a
frequency xxs =
  let
    default = snd $ NEA.head xxs
    total = unwrap $ foldMap1 (Additive <<< fst) xxs
    pick i n = case NEA.index xxs i of
      Nothing -> default
      Just (Tuple k x')
        | n <= k -> x'
        | otherwise -> pick (i + 1) (n - k)
  in do
    n <- choose zero total
    pick 0 n

-- | Create a random generator which generates an array of random values.
arrayOf :: forall a. Gen a -> Gen (Array a)
arrayOf g = sized $ \n ->
  do k <- chooseInt zero n
     vectorOf k g

-- | Create a random generator which generates a non-empty array of random values.
arrayOf1 :: forall a. Gen a -> Gen (NonEmptyArray a)
arrayOf1 g = sized $ \n ->
  do k <- chooseInt zero n
     x <- g
     xs <- vectorOf (k - one) g
     pure $ unsafePartial fromJust $ NEA.fromArray $ x : xs

-- | Create a random generator for a finite enumeration.
-- | `toEnum i` must be well-behaved:
-- | It must return a value wrapped in Just for all Ints between
-- | `fromEnum bottom` and `fromEnum top`.
enum :: forall a. BoundedEnum a => Gen a
enum = do
  i <- chooseInt (fromEnum (bottom :: a)) (fromEnum (top :: a))
  pure (unsafePartial $ fromJust $ toEnum i)

replicateMRec :: forall m a. MonadRec m => Int -> m a -> m (List a)
replicateMRec k _ | k <= 0 = pure Nil
replicateMRec k gen = tailRecM go (Tuple Nil k)
  where
  go :: (Tuple (List a) Int) -> m (Step (Tuple (List a) Int) (List a))
  go (Tuple acc 0) = pure $ Done acc
  go (Tuple acc n) = gen <#> \x -> Loop (Tuple (Cons x acc) (n - 1))

-- | Create a random generator which generates a list of random values of the specified size.
listOf :: forall a. Int -> Gen a -> Gen (List a)
listOf = replicateMRec

-- | Create a random generator which generates a vector of random values of a specified size.
vectorOf :: forall a. Int -> Gen a -> Gen (Array a)
vectorOf k g = toUnfoldable <$> listOf k g

-- | Create a random generator which selects a value from a non-empty array with
-- | uniform probability.
elements :: forall a. NonEmptyArray a -> Gen a
elements xs = do
  n <- chooseInt zero (NEA.length xs - one)
  pure $ unsafePartial $ NEA.unsafeIndex xs n

-- | Generate a random permutation of the given array
shuffle :: forall a. Array a -> Gen (Array a)
shuffle xs = do
  ns <- vectorOf (length xs) (chooseInt 0 top)
  pure (map snd (sortBy (comparing fst) (zip ns xs)))

-- | Run a random generator
runGen :: forall a. Gen a -> GenState -> Tuple a GenState
runGen = runState <<< unGen

-- | Run a random generator, keeping only the randomly-generated result
evalGen :: forall a. Gen a -> GenState -> a
evalGen = evalState <<< unGen

-- | Sample a random generator
sample :: forall a. Seed -> Size -> Gen a -> Array a
sample seed sz g = evalGen (vectorOf sz g) { newSeed: seed, size: sz }

-- | Generate a single value using a randomly generated seed.
randomSampleOne :: forall a. Gen a -> Effect a
randomSampleOne gen = do
  seed <- randomSeed
  pure $ evalGen gen { newSeed: seed, size: 10 }

-- | Sample a random generator, using a randomly generated seed
randomSample' :: forall a. Size -> Gen a -> Effect (Array a)
randomSample' n g = do
  seed <- randomSeed
  pure $ sample seed n g

-- | Get a random sample of 10 values. For a single value, use `randomSampleOne`.
randomSample :: forall a. Gen a -> Effect (Array a)
randomSample = randomSample' 10

-- | A random generator which simply outputs the current seed
lcgStep :: Gen Int
lcgStep = Gen $ state f where
  f s = Tuple (unSeed s.newSeed) (s { newSeed = lcgNext s.newSeed })

-- | A random generator which approximates a uniform random variable on `[0, 1]`
uniform :: Gen Number
uniform = (\n -> toNumber n / toNumber lcgM) <$> lcgStep

foreign import float32ToInt32 :: Number -> Int

-- | Perturb a random generator by modifying the current seed
perturbGen :: forall a. Number -> Gen a -> Gen a
perturbGen n gen = Gen do
  void $ modify \s -> s { newSeed = lcgPerturb (float32ToInt32 n) s.newSeed }
  unGen gen
