module Test.IndexBuilder where

import Prelude

import Docs.Search.IndexBuilder (patchHtml)

import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

tests :: Spec Unit
tests = do
  describe "IndexBuilder" do
    describe "patchHtml" do
      it "works" do
        let input = "</body>"
        let patched = patchHtml input
        shouldNotEqual patched (Just input)
        shouldNotEqual patched Nothing
      it "only patches once" do
        let input = "</body>"
        let patchTwice = patchHtml >=> patchHtml
        shouldEqual (patchTwice "</body>") Nothing
