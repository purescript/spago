module Test.IndexBuilder where

import Prelude

import Test.Extra

import Docs.Search.IndexBuilder

import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.List (List(..), (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Set as Set
import Data.Tuple (Tuple(..), snd)
import Effect.Aff (Aff)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = do
  suite "IndexBuilder" do
    suite "patchHTML" do
      test "works" do
        let input = "</body>"
        Assert.assertFalse "patchHTML works" (snd (patchHTML input) == input)
      test "is idempotent" do
        let input = "</body>"
        Assert.equal (snd $ patchHTML $ snd $ patchHTML input) (snd $ patchHTML input)
