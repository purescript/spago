module Test.Spago.Unit.Init where

import Test.Prelude

import Registry.PackageName as PackageName
import Spago.Command.Init (folderToPackageName)
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions (shouldSatisfy)

spec :: Spec Unit
spec = Spec.describe "Init" do

  Spec.describe "folderToPackageName" do

    Spec.it "converts to lowercase" do
      folderToPackageName "MyProject" `shouldEqualPkg` "myproject"
      folderToPackageName "ALLCAPS" `shouldEqualPkg` "allcaps"

    Spec.it "replaces spaces with dashes" do
      folderToPackageName "my project" `shouldEqualPkg` "my-project"
      folderToPackageName "My Project Dir" `shouldEqualPkg` "my-project-dir"

    Spec.it "removes apostrophes (straight and curly)" do
      folderToPackageName "Tim's Test" `shouldEqualPkg` "tims-test"
      folderToPackageName "Tim's Test" `shouldEqualPkg` "tims-test"
      folderToPackageName "it's" `shouldEqualPkg` "its"

    Spec.it "removes double quotes" do
      folderToPackageName "my\"project" `shouldEqualPkg` "myproject"
      folderToPackageName "\"test\"" `shouldEqualPkg` "test"

    Spec.it "replaces special characters with dashes" do
      folderToPackageName "test#1" `shouldEqualPkg` "test-1"
      folderToPackageName "test(dev)" `shouldEqualPkg` "test-dev"
      folderToPackageName "test@home" `shouldEqualPkg` "test-home"
      folderToPackageName "test_underscore" `shouldEqualPkg` "test-underscore"

    Spec.it "collapses consecutive dashes" do
      folderToPackageName "test--project" `shouldEqualPkg` "test-project"
      folderToPackageName "a   b" `shouldEqualPkg` "a-b"
      folderToPackageName "Test #1 (dev)" `shouldEqualPkg` "test-1-dev"

    Spec.it "strips leading dashes" do
      folderToPackageName "-test" `shouldEqualPkg` "test"
      folderToPackageName "---test" `shouldEqualPkg` "test"
      folderToPackageName "#test" `shouldEqualPkg` "test"

    Spec.it "strips trailing dashes" do
      folderToPackageName "test-" `shouldEqualPkg` "test"
      folderToPackageName "test---" `shouldEqualPkg` "test"
      folderToPackageName "test#" `shouldEqualPkg` "test"

    Spec.it "handles digits" do
      folderToPackageName "project123" `shouldEqualPkg` "project123"
      folderToPackageName "123project" `shouldEqualPkg` "123project"

    Spec.it "returns Nothing for invalid inputs" do
      -- All special characters results in empty string
      shouldBeNothing $ folderToPackageName "..."
      shouldBeNothing $ folderToPackageName "###"
      shouldBeNothing $ folderToPackageName "'''"

    Spec.it "converts accented characters to ASCII" do
      -- NFD normalization decomposes accents, keeping the base letter
      folderToPackageName "café" `shouldEqualPkg` "cafe"
      folderToPackageName "naïve" `shouldEqualPkg` "naive"
      folderToPackageName "über" `shouldEqualPkg` "uber"
      folderToPackageName "señor" `shouldEqualPkg` "senor"
      folderToPackageName "Ångström" `shouldEqualPkg` "angstrom"

    Spec.it "strips purescript- prefix" do
      folderToPackageName "purescript-foo" `shouldEqualPkg` "foo"
      folderToPackageName "Purescript-Bar" `shouldEqualPkg` "bar"

  where
  shouldEqualPkg actual expected =
    (PackageName.print <$> actual) `shouldEqual` Just expected

  shouldBeNothing actual =
    (PackageName.print <$> actual) `shouldSatisfy` isNothing
