module Test.ModuleIndex where

import Docs.Search.ModuleIndex (extractModuleNameParts)

import Prelude

import Data.List as List
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert


tests :: TestSuite
tests = do
  suite "ModuleIndex" do

    test "test #0" do
      Assert.equal (extractModuleNameParts "Data.Array.ST") (
        List.fromFoldable [ "st", "array.st", "data.array.st" ]
      )

    test "test #1" do
      Assert.equal (extractModuleNameParts "Foo") (
        List.fromFoldable [ "foo" ]
      )
