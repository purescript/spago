module Test.Spago.Docs where

import Test.Prelude

import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Spago.FS as FS

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "docs" do

    Spec.it "documents successfully with no flags" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "docs" ] >>= shouldBeSuccess
      FS.exists "generated-docs/html/index.html" `Assert.shouldReturn` true

    Spec.it "builds successfully a solver-only package" \{ spago } -> do
      spago [ "init", "--name", "aaa", "--use-solver" ] >>= shouldBeSuccess
      spago [ "docs" ] >>= shouldBeSuccess
      FS.exists "generated-docs/html/index.html" `Assert.shouldReturn` true

    Spec.it "can output ctags instead of html" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "docs", "--format", "ctags" ] >>= shouldBeSuccess
      FS.exists "tags" `Assert.shouldReturn` true
