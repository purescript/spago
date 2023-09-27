module Test.IndexBuilder where

import Prelude

import Docs.Search.IndexBuilder (patchHTML)

import Data.Tuple (snd)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy)

tests :: Spec Unit
tests = do
  describe "IndexBuilder" do
    describe "patchHTML" do
      it "works" do
        let input = "</body>"
        shouldNotSatisfy (snd (patchHTML input)) (eq input)
      it "is idempotent" do
        let input = "</body>"
        shouldEqual (snd $ patchHTML $ snd $ patchHTML input) (snd $ patchHTML input)
