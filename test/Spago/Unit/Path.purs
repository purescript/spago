module Test.Spago.Unit.Path where

import Test.Prelude

import Effect.Unsafe (unsafePerformEffect)
import Spago.Path as Path
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.describe "Paths" do

  Spec.describe "RootPath" do
    Spec.it "can append strings" do
      (root "/foo" </> "bar") `shouldPointAt` "/foo/bar"
      (root "/foo" </> "bar" </> "baz") `shouldPointAt` "/foo/bar/baz"
      (root "/foo/x/y" </> "/bar" </> "baz") `shouldPointAt` "/bar/baz"
      (root "/foo/x/y" </> "/foo/x/y/z") `shouldPointAt` "/foo/x/y/z"

  Spec.describe "LocalPath" do
    Spec.it "can append strings" do
      let p = Path.withForwardSlashes $ root "/foo" </> "bar" </> "baz"
      Path.localPart p `shouldEqual` "bar/baz"
      (p </> "x") `shouldPointAt` "/foo/bar/baz/x"
      (p </> "../x") `shouldPointAt` "/foo/bar/x"
      (p </> "../.." </> "x") `shouldPointAt` "/foo/x"
      (p </> "x" </> "y" </> "z") `shouldPointAt` "/foo/bar/baz/x/y/z"

    Spec.it "always keeps the original root" do
      let p1 = Path.withForwardSlashes $ root "/foo/x/y" </> "/bar" </> "baz"
      Path.localPart p1 `shouldEqual` "../../../bar/baz"
      Path.rootPart p1 `shouldPointAt` "/foo/x/y"

      let p2 = Path.withForwardSlashes $ root "/foo/x/y" </> "bar" </> "baz"
      Path.localPart p2 `shouldEqual` "bar/baz"
      Path.rootPart p2 `shouldPointAt` "/foo/x/y"

      let p3 = Path.withForwardSlashes $ root "/foo/x/y" </> "../../bar" </> "baz"
      p3 `shouldPointAt` "/foo/bar/baz"
      Path.localPart p3 `shouldEqual` "../../bar/baz"
      Path.rootPart p3 `shouldPointAt` "/foo/x/y"

  Spec.describe "GlobalPath" do
    Spec.it "can append strings" do
      (Path.global "/foo" </> "bar") `shouldPointAt` "/foo/bar"
      (Path.global "/foo" </> "bar" </> "baz") `shouldPointAt` "/foo/bar/baz"
      (Path.global "/foo/x/y" </> "/bar" </> "baz") `shouldPointAt` "/bar/baz"
      (Path.global "/foo/x/y" </> "/foo/x/y/z") `shouldPointAt` "/foo/x/y/z"
      (Path.global "/foo/x/y" </> ".." </> ".." </> "bar") `shouldPointAt` "/foo/bar"

  where
  root = unsafePerformEffect <<< Path.mkRoot <<< Path.global

  shouldPointAt :: ∀ path. Path.IsPath path => path -> String -> _
  shouldPointAt path raw = Path.toRaw (Path.withForwardSlashes $ Path.toGlobal path) `shouldEqual` raw
