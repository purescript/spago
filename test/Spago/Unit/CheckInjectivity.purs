module Test.Spago.Unit.CheckInjectivity where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Enum (enumFromTo)
import Data.Generic.Rep (class Generic)
import Data.List (List, fold, (:))
import Data.List as List
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Spago.Config (fileSystemCharEscape)
import Test.QuickCheck (Result)
import Test.QuickCheck as QC
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen
import Test.Spec (Spec)
import Test.Spec as Spec

-- A value of this type represents a failure of a function to be injective,
-- specifically, that all of the distinct values in the `inputs` list each
-- mapped to the same output.
newtype InjectivityFailure a b = InjectivityFailure
  { inputs :: Array a
  -- ^ should all be distinct and should have at least 2 elements.
  , allMapTo :: b
  }

derive instance (Eq a, Eq b) => Eq (InjectivityFailure a b)
derive instance (Ord a, Ord b) => Ord (InjectivityFailure a b)
derive instance Newtype (InjectivityFailure a b) _
derive instance Generic (InjectivityFailure a b) _
instance (Show a, Show b) => Show (InjectivityFailure a b) where
  show x = genericShow x

-- | Given a generator some type, return a new generator which returns a list
-- of that type, with the given length, where all elements are distinct.
uniqueVectorOf :: forall a. Ord a => Int -> Gen a -> Gen (Array a)
uniqueVectorOf count gen = go count List.Nil Set.empty
  where
  go :: Int -> List a -> Set a -> Gen (Array a)
  go 0 ls _ = pure $ Array.fromFoldable ls
  go remaining ls set = do
    next <- gen
    if not $ Set.member next set then do
      go (remaining - 1) (next : ls) (Set.insert next set)
    else do
      go remaining ls set

-- | Randomly resize a generator up to the provided maximum size.
randomlyResize :: forall a. Int -> Gen a -> Gen a
randomlyResize maxSize gen = do
  size <- Gen.chooseInt 1 maxSize
  Gen.resize size gen

checkInjective :: forall a b. Ord a => Ord b => Show a => Show b => (a -> b) -> Gen a -> Effect Unit
checkInjective f gen = QC.quickCheckGen' 1 do
  map check $ uniqueVectorOf countInputs $ randomlyResize maxSize gen
  where
  maxSize = 5
  countInputs = 1_000

  check :: Array a -> Gen Result
  check inputs = do
    let
      collisions = findCollisions inputs
      maxCounterexamples = 10
    if Array.null collisions then
      pure QC.Success
    else do
      let
        numCollisions = Array.length collisions
        count = if numCollisions <= maxCounterexamples then "all" else show maxCounterexamples
        msgPrefix = "Showing " <> count <> " of the " <> show numCollisions <> " counterexamples:\n"
      pure $ QC.Failed $ msgPrefix <> Array.intercalate "\n" (map show $ Array.take maxCounterexamples collisions)

  findCollisions :: Array a -> Array (InjectivityFailure a b)
  findCollisions =
    map (\(Tuple k v) -> InjectivityFailure { inputs: v, allMapTo: k })
      <<< Map.toUnfoldable
      <<< Map.filter (\xs -> Array.length xs > 1)
      <<< Map.fromFoldableWith (<>)
      <<< map (\x -> Tuple (f x) [ x ])

-- This needs to be a small generator if we want collisions to come up
genBranchName :: Gen String
genBranchName =
  map fold $ Gen.arrayOf1 $ Gen.frequency $ NonEmptyArray
    [ Tuple 2.0 $ Gen.elements $ NonEmptyArray [ "/", "\\", ":", ";", "%" ]
    , Tuple 1.0 $ Gen.elements $ map SCU.singleton $ enumFromTo 'A' 'F'
    , Tuple 1.0 $ Gen.elements $ map SCU.singleton $ enumFromTo 'a' 'f'
    , Tuple 1.0 $ Gen.elements $ map SCU.singleton $ enumFromTo '0' '9'
    ]

spec :: Spec Unit
spec = Spec.describe "injectivity" $ do
  Spec.it "fileSystemCharEscape is (case insensitively) injective" $ do
    liftEffect $ checkInjective fileSystemCharEscape genBranchName
