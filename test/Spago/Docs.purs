module Test.Spago.Docs where

import Test.Prelude

import Node.FS.Aff as FSA
import Node.Path as Path
import Registry.Version as Version
import Spago.Command.Init as Init
import Spago.Core.Config as Config
import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "docs" do

    Spec.it "documents successfully with no flags" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "docs" ] >>= shouldBeSuccess

    Spec.it "builds successfully a solver-only package" \{ spago } -> do
      spago [ "init", "--name", "aaa", "--use-solver" ] >>= shouldBeSuccess
      spago [ "docs" ] >>= shouldBeSuccess

    Spec.it "can output ctags instead of html" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "docs", "--format", "ctags" ] >>= shouldBeSuccess
