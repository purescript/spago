module Test.Spago.Glob where

import Test.Prelude

import Data.Foldable (intercalate)
import Data.String.Gen (genAlphaLowercaseString)
import Effect.Aff as Aff
import Node.Path as Path
import Spago.FS as FS
import Spago.Glob as Glob
import Test.QuickCheck.Gen (randomSample', resize)
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
        [ touch "apple"
        , touch "orange"
        , touch "banana"
        , dir "special"
          [ touch "apple"
          ]
        ]
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
      Spec.it "when no .gitignore, yields all matches" \p -> do
        a <- Glob.gitignoringGlob p ["**/apple"]
        a `Assert.shouldEqual` ["fruits/apple", "fruits/special/apple", "src/fruits/apple"]

      Spec.it "respects a .gitignore pattern that doesn't conflict with search" \p -> do
        FS.writeTextFile (Path.concat [p, ".gitignore"]) "/fruits"
        a <- Glob.gitignoringGlob p ["**/apple"]
        a `Assert.shouldEqual` ["src/fruits/apple"]

      Spec.it "respects a negated .gitignore pattern" \p -> do
        FS.writeTextFile (Path.concat [p, ".gitignore"]) "!/fruits/special/apple\n/fruits/apple"
        a <- Glob.gitignoringGlob p ["**/apple"]
        a `Assert.shouldEqual` ["fruits/special/apple", "src/fruits/apple"]

      for_ ["/fruits", "fruits", "fruits/", "**/fruits", "fruits/**", "**/fruits/**"] \gitignore -> do
        Spec.it
          ("does not respect a .gitignore pattern that conflicts with search: " <> gitignore)
          \p -> do
            FS.writeTextFile (Path.concat [p, ".gitignore"]) gitignore
            a <- Glob.gitignoringGlob p ["fruits/apple/**"]
            a `Assert.shouldEqual` ["fruits/apple"]

      Spec.it "respects some .gitignore patterns" \p -> do
        FS.writeTextFile (Path.concat [p, ".gitignore"]) """/fruits\n/src"""
        a <- Glob.gitignoringGlob p ["fruits/apple/**"]
        a `Assert.shouldEqual` ["fruits/apple"]

      Spec.it "is stacksafe" \p -> do
        hugeGitignore <- liftEffect $ intercalate "\n" <$> randomSample' 10000 (resize 4 $ genAlphaLowercaseString)
        FS.writeTextFile (Path.concat [p, ".gitignore"]) hugeGitignore
        a <- Glob.gitignoringGlob p ["fruits/apple/**"]
        a `Assert.shouldEqual` ["fruits/apple"]
