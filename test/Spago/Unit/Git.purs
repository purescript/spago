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
          -- spago should be tolerant to missing .git
          mkTest "origin\tgit@github.com:foo/bar (fetch)" "git@github.com:foo/bar"
          mkTest "origin  git@github.com:foo/bar (fetch)" "git@github.com:foo/bar"

        Spec.describe "remote with an https protocol" do
          mkTest "origin\thttps://github.com/foo/bar.git (push)" "https://github.com/foo/bar.git"
          mkTest "origin  https://github.com/foo/bar.git (push)" "https://github.com/foo/bar.git"
          -- spago should be tolerant to missing .git
          mkTest "origin\thttps://github.com/foo/bar (push)" "https://github.com/foo/bar"
          mkTest "origin  https://github.com/foo/bar (push)" "https://github.com/foo/bar"

        Spec.describe "remote with an https protocol" do
          mkTest "origin\thttp://github.com/foo/bar.git (push)" "http://github.com/foo/bar.git"
          mkTest "origin  http://github.com/foo/bar.git (push)" "http://github.com/foo/bar.git"
          -- spago should be tolerant to missing .git
          mkTest "origin\thttp://github.com/foo/bar (push)" "http://github.com/foo/bar"
          mkTest "origin  http://github.com/foo/bar (push)" "http://github.com/foo/bar"

        Spec.describe "remote with an ssh protocol" do
          mkTest "origin\tssh://git@github.com/foo/bar.git (push)" "ssh://git@github.com/foo/bar.git"
          mkTest "origin  ssh://git@github.com/foo/bar.git (push)" "ssh://git@github.com/foo/bar.git"
          -- spago should be tolerant to missing .git
          mkTest "origin\tssh://git@github.com/foo/bar (push)" "ssh://git@github.com/foo/bar"
          mkTest "origin  ssh://git@github.com/foo/bar (push)" "ssh://git@github.com/foo/bar"

      Spec.describe "rejects" do
        let mkTest input = Spec.it input $ parseRemote input `shouldEqual` Nothing

        Spec.describe "rejects malformed remotes" do
          Spec.describe "missing trailing (push) or (fetch) field" do
            -- yes, not possible even if edit .git/config
            mkTest "origin\tgit@github.com:foo/bar.git"
            mkTest "origin  git@github.com:foo/bar.git"

          Spec.describe "missing repo name (with or without .git given)" do
            -- git
            mkTest "origin\tgit@github.com:foo.git (push)"
            mkTest "origin  git@github.com:foo.git (push)"
            mkTest "origin\tgit@github.com:foo (push)"
            mkTest "origin  git@github.com:foo (push)"
            -- https
            mkTest "origin\thttps://github.com:foo.git (push)"
            mkTest "origin  https://github.com:foo.git (push)"
            mkTest "origin\thttps://github.com:foo (push)"
            mkTest "origin  https://github.com:foo (push)"
            -- http
            mkTest "origin\thttp://github.com:foo.git (push)"
            mkTest "origin  http://github.com:foo.git (push)"
            mkTest "origin\thttp://github.com:foo (push)"
            mkTest "origin  http://github.com:foo (push)"
            -- ssh
            mkTest "origin\tssh://github.com:foo.git (push)"
            mkTest "origin  ssh://github.com:foo.git (push)"
            mkTest "origin\tssh://github.com:foo (push)"
            mkTest "origin  ssh://github.com:foo (push)"
