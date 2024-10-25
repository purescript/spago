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

    Spec.it "can append LocalPath" do
      pure unit

    Spec.it "can append GlobalPath" do
      pure unit

    Spec.it "has to have absolute root" do
      pure unit

  Spec.describe "LocalPath" do
    Spec.it "always keeps the original root" do
      pure unit

  where
  root = unsafePerformEffect <<< Path.mkRoot <<< Path.global

  shouldPointAt path raw = Path.toRaw path `shouldEqual` raw
