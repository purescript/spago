module Test.Spago.Glob where

import Test.Prelude

import Effect.Aff as Aff
import Node.Path as Path
import Spago.FS as FS
import Spago.Glob as Glob
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

globTmpDir :: (String -> Aff Unit) -> Aff Unit
globTmpDir m = Aff.bracket make cleanup m
  where
    touch name base = FS.writeTextFile (Path.concat [base, name]) ""
    dir name contents base = do
      FS.mkdirp $ Path.concat [base, name]
      for_ contents \f -> f $ Path.concat [base, name]
    cleanup _ = pure unit
    make = do
      base <- mkTemp' $ Just "spago-test-"
      dir
        ".git"
        [ dir "fruits" [touch "apple"] ]
        base
      dir
        "fruits"
        [touch "apple", touch "orange", touch "banana"]
        base
      dir
        "sports"
        [touch "baseball", touch "scrabble"]
        base
      dir
        "src"
        [ dir "fruits" [touch "apple"]
        , dir "sports" [touch "baseball"]
        ]
        base
      pure base

spec :: Spec Unit
spec = Spec.around globTmpDir do
  Spec.describe "glob" do
    Spec.describe "gitignoringGlob" do
      Spec.it "walks files, ignoring .git/**/*" \p -> do
        a <- Glob.gitignoringGlob p ["**/fruits/apple/**"]
        a `Assert.shouldEqual` ["fruits/apple", "src/fruits/apple"]

        b <- Glob.gitignoringGlob p ["sports/**"]
        b `Assert.shouldEqual` ["sports", "sports/baseball", "sports/scrabble"]

      Spec.it "walks files, respecting .gitignore patterns that do not ignore the search pattern" \p -> do
        FS.writeTextFile (Path.concat [p, ".gitignore"]) "/fruits"
        a <- Glob.gitignoringGlob p ["src/fruits/apple/**"]
        a `Assert.shouldEqual` ["src/fruits/apple"]

      Spec.it "walks files, ignoring .gitignore patterns that ignore the search pattern" \p -> do
        for_ ["/fruits", "fruits", "fruits/", "**/fruits", "fruits/**", "**/fruits/**"] \gitignore -> do
          FS.writeTextFile (Path.concat [p, ".gitignore"]) gitignore
          a <- Glob.gitignoringGlob p ["fruits/apple/**"]
          a `Assert.shouldEqual` ["fruits/apple"]

      Spec.it "walks files, ignoring some .gitignore patterns" \p -> do
        FS.writeTextFile (Path.concat [p, ".gitignore"]) """/fruits\n/src"""
        a <- Glob.gitignoringGlob p ["fruits/apple/**"]
        a `Assert.shouldEqual` ["fruits/apple"]
