module Test.Spago.Unit.Git where

import Prelude

import Spago.Git (parseRemote)
import Test.Prelude (Maybe(..), shouldEqual)
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = do
  Spec.describe "Git" do
    Spec.describe "parseRemote" do

      Spec.it "parses a remote with a git protocol" do
        parseRemote "origin\tgit@github.com:foo/bar.git (fetch)"
          `shouldEqual` Just { name: "origin", url: "git@github.com:foo/bar.git", owner: "foo", repo: "bar" }

      Spec.it "parses a remote with an https protocol" do
        parseRemote "origin\thttps://github.com/foo/bar.git (push)"
          `shouldEqual` Just { name: "origin", url: "https://github.com/foo/bar.git", owner: "foo", repo: "bar" }

      Spec.it "rejects malformed remotes" do
        parseRemote "origin\tgit@github.com:foo/bar.git" `shouldEqual` Nothing
        parseRemote "origin\tgit@github.com:foo/bar (push)" `shouldEqual` Nothing
        parseRemote "origin git@github.com:foo/bar.git (fetch)" `shouldEqual` Nothing
        parseRemote "origin\tgit@github.com:foo.git (push)" `shouldEqual` Nothing
        parseRemote "origin\thttps://foo.com/bar.git (push)" `shouldEqual` Nothing
