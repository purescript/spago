-- Unit tests
module UnitSpec where

import           Data.List.Extra    (nubOrd)
import qualified Data.Map           as Map
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Prelude
import           Test.Hspec         (Spec, describe, it, shouldBe)
import           Test.QuickCheck    (Gen, Property)
import qualified Test.QuickCheck    as QC

import           Spago.Dhall        (TemplateComments (..), processComments)
import           Spago.FetchPackage (getCacheVersionDir)

-- A value of this type represents a failure of a function to be injective,
-- specifically, that all of the distinct values in the `inputs` list each
-- mapped to the same output.
data InjectivityFailure a b
  = InjectivityFailure
      { inputs   :: [a]
        -- ^ should all be distinct and should have at least 2 elements.
      , allMapTo :: b
      }
  deriving (Eq, Ord, Show)

-- | Given a generator some type, return a new generator which returns a list
-- of that type, with the given length, where all elements are distinct.
uniqVectorOf :: Ord a => Int -> Gen a -> Gen [a]
uniqVectorOf count gen =
  fmap (take count . nubOrd) (QC.infiniteListOf gen)

-- | Randomly resize a generator up to the provided maximum size.
randomlyResize :: Int -> Gen a -> Gen a
randomlyResize maxSize gen = do
  size <- QC.choose (1, maxSize)
  QC.resize size gen

checkInjective :: forall a b. (Ord a, Ord b, Show a, Show b) => (a -> b) -> Gen a -> Property
checkInjective f gen =
  QC.once $
    fmap check $
      -- Generate a lot of distinct inputs of various (small) sizes
      uniqVectorOf countInputs $
        randomlyResize maxSize gen

  where
  maxSize = 5
  countInputs = 50000

  check :: [a] -> Property
  check inputs =
    let
      collisions = findCollisions inputs
      maxCounterexamples = 10
    in
      if null collisions
        then
          QC.property True
        else
          let
            numCollisions = length collisions
            msgPrefix = concat
              [ "Showing "
              , if numCollisions <= maxCounterexamples
                  then "all"
                  else show maxCounterexamples
              , " of the "
              , show numCollisions
              , " counterexamples:\n"
              ]
          in
            QC.counterexample
              (msgPrefix ++ unlines (map show (take maxCounterexamples collisions)))
              False

  findCollisions :: [a] -> [InjectivityFailure a b]
  findCollisions =
    map (uncurry (flip InjectivityFailure))
    . Map.toList
    . Map.filter (\xs -> length xs > 1)
    . Map.fromListWith (++)
    . map (\x -> (f x, [x]))

-- This needs to be a small generator if we want collisions to come up
genBranchName :: Gen Text
genBranchName =
  fmap mconcat $ QC.listOf1 $ QC.elements $
    [ "/" , "\\" , ":" , ";" , "%" ]
    ++ map Text.singleton [ 'A' .. 'F' ]
    ++ map Text.singleton [ 'a' .. 'f' ]
    ++ map Text.singleton [ '0' .. '9' ]

spec :: Spec
spec = describe "unit tests" $ do
  it "getCacheVersionDir is (case insensitively) injective" $ do
    checkInjective (Text.toLower . getCacheVersionDir) genBranchName

  describe "Dhall.processComments" $ do

    let dhallWithComment = "{- a comment -}\n[1,2]"

    it "does not change dhall source when WithComments is used" $
      processComments WithComments dhallWithComment `shouldBe` dhallWithComment
    it "removes comments from dhall source and formats it" $
      processComments NoComments dhallWithComment `shouldBe` "[ 1, 2 ]"
    it "formats dhall source when no comments are present" $
      processComments NoComments "[1,2]" `shouldBe` "[ 1, 2 ]"
