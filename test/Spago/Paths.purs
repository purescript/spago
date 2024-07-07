module Test.Spago.Paths where

import Test.Prelude

import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

import Spago.Paths (toGitSearchPath)

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "paths" do
    Spec.it "generate four paths to parent directories of working directory, plus working directory" \ _ -> do
      toGitSearchPath "~/a/b/c/d/e" `Assert.shouldEqual`
        [ "~/a/b/c/d/e"
        , "~/a/b/c/d/e/../"
        , "~/a/b/c/d/e/../../"
        , "~/a/b/c/d/e/../../../"
        , "~/a/b/c/d/e/../../../../"
        ]
