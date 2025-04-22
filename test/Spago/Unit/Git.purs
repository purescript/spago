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
      Spec.describe "successfully parses" do
        let
          mkTest input expectedUrl = Spec.it input $ parseRemote input `shouldEqual` Just { name: "origin", url: expectedUrl, owner: "foo", repo: "bar" }

        Spec.describe "remote with a git protocol" do
          mkTest "origin\tgit@github.com:foo/bar.git (fetch)" "git@github.com:foo/bar.git"
          mkTest "origin  git@github.com:foo/bar.git (fetch)" "git@github.com:foo/bar.git"

        Spec.describe "remote with an https protocol" do
          mkTest "origin\thttps://github.com/foo/bar.git (push)" "https://github.com/foo/bar.git"
          mkTest "origin  https://github.com/foo/bar.git (push)" "https://github.com/foo/bar.git"

        Spec.describe "remote with an ssh protocol" do
          mkTest "origin\tssh://git@github.com/foo/bar.git (push)" "ssh://git@github.com/foo/bar.git"
          mkTest "origin  ssh://git@github.com/foo/bar.git (push)" "ssh://git@github.com/foo/bar.git"

      Spec.describe "rejects" do
        let mkTest input = Spec.it input $ parseRemote input `shouldEqual` Nothing

        Spec.describe "rejects malformed remotes" do
          Spec.describe "missing trailing (push) or (fetch) field" do
            mkTest "origin\tgit@github.com:foo/bar.git"
            mkTest "origin  git@github.com:foo/bar.git"

          Spec.describe "missing .git at the end of repo URL" do
            mkTest "origin\tgit@github.com:foo/bar (push)"
            mkTest "origin  git@github.com:foo/bar (push)"

          Spec.describe "missing repo name (only owner.git given)" do
            mkTest "origin\tgit@github.com:foo.git (push)"
            mkTest "origin  git@github.com:foo.git (push)"

          Spec.describe "non-GitHub domain (unsupported host)" do
            mkTest "origin\thttps://foo.com/bar.git (push)"
            mkTest "origin  https://foo.com/bar.git (push)"
