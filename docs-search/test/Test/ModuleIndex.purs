module Test.ModuleIndex where

import Docs.Search.ModuleIndex (extractModuleNameParts)

import Prelude

import Data.List as List
import Data.Newtype (wrap)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  describe "ModuleIndex" do

    it "test #0" do
      extractModuleNameParts (wrap "Data.Array.ST") `shouldEqual`
        List.fromFoldable [ "st", "array.st", "data.array.st" ]

    it "test #1" do
      extractModuleNameParts (wrap "Foo") `shouldEqual`
        List.fromFoldable [ "foo" ]
