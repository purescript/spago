module Test.IndexBuilder where

import Prelude

import Docs.Search.IndexBuilder (patchHTML)

import Data.Tuple (snd)
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
