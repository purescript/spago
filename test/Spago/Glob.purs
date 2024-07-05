module Test.Spago.Glob where

import Test.Prelude

import Data.Array as Array
import Data.Foldable (intercalate)
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
  touch name base = FS.writeTextFile (Path.concat [ base, name ]) ""
  dir name contents base = do
    FS.mkdirp $ Path.concat [ base, name ]
    for_ contents \f -> f $ Path.concat [ base, name ]
  cleanup _ = pure unit
  make = do
    base <- mkTemp' $ Just "spago-test-"
    dir
      ".git"
      [ dir "fruits" [ touch "apple" ] ]
      base
    dir
      "fruits"
      [ dir "left"
          [ touch "apple"
          ]
      , dir "right"
          [ touch "apple"
          ]
      ]
      base
    dir
      "src"
      [ dir "fruits" [ touch "apple" ]
      , dir "sports" [ touch "baseball" ]
      ]
      base
    pure base

spec :: Spec Unit
spec = Spec.around globTmpDir do
  Spec.describe "glob" do
    Spec.describe "glob behavior" do
      Spec.it "'**/..' matches 0 or more directories" \p -> do
        a <- Glob.gitignoringGlob (Path.concat [ p, "fruits/left" ]) [ "**/apple" ]
        b <- Glob.gitignoringGlob (Path.concat [ p, "fruits" ]) [ "**/apple" ]
        Array.sort a `Assert.shouldEqual` [ "apple" ]
        Array.sort b `Assert.shouldEqual` [ "left/apple", "right/apple" ]

      Spec.it "'../**/..' matches 0 or more directories" \p -> do
        a <- Glob.gitignoringGlob p [ "fruits/**/apple" ]
        Array.sort a `Assert.shouldEqual` [ "fruits/left/apple", "fruits/right/apple" ]

      Spec.it "'../**' matches 0 or more directories" \p -> do
        a <- Glob.gitignoringGlob p [ "fruits/left/**" ]
        Array.sort a `Assert.shouldEqual` [ "fruits/left", "fruits/left/apple" ]

    Spec.describe "gitignoringGlob" do
      Spec.it "when no .gitignore, yields all matches" \p -> do
        a <- Glob.gitignoringGlob p [ "**/apple" ]
        Array.sort a `Assert.shouldEqual` [ "fruits/left/apple", "fruits/right/apple", "src/fruits/apple" ]

      Spec.it "respects a .gitignore pattern that doesn't conflict with search" \p -> do
        FS.writeTextFile (Path.concat [ p, ".gitignore" ]) "fruits/right"
        a <- Glob.gitignoringGlob p [ "fruits/**/apple" ]
        Array.sort a `Assert.shouldEqual` [ "fruits/left/apple" ]

      Spec.it "respects some .gitignore patterns" \p -> do
        FS.writeTextFile (Path.concat [ p, ".gitignore" ]) "fruits\nfruits/right"
        a <- Glob.gitignoringGlob p [ "fruits/**/apple" ]
        Array.sort a `Assert.shouldEqual` [ "fruits/left/apple" ]

      Spec.it "respects a negated .gitignore pattern" \p -> do
        FS.writeTextFile (Path.concat [ p, ".gitignore" ]) "!/fruits/left/apple\n/fruits/**/apple"
        a <- Glob.gitignoringGlob p [ "**/apple" ]
        Array.sort a `Assert.shouldEqual` [ "fruits/left/apple", "src/fruits/apple" ]

      for_ [ "/fruits", "fruits", "fruits/", "**/fruits", "fruits/**", "**/fruits/**" ] \gitignore -> do
        Spec.it
          ("does not respect a .gitignore pattern that conflicts with search: " <> gitignore)
          \p -> do
            FS.writeTextFile (Path.concat [ p, ".gitignore" ]) gitignore
            a <- Glob.gitignoringGlob p [ "fruits/**/apple" ]
            Array.sort a `Assert.shouldEqual` [ "fruits/left/apple", "fruits/right/apple" ]

      Spec.it "is stacksafe" \p -> do
        let
          chars = [ "a", "b", "c", "d", "e", "f", "g", "h" ]
          -- 4000-line gitignore
          words = [ \a b c d -> a <> b <> c <> d ] <*> chars <*> chars <*> chars <*> chars
          hugeGitignore = intercalate "\n" words
        -- Write it in a few places
        FS.writeTextFile (Path.concat [ p, ".gitignore" ]) hugeGitignore
        FS.writeTextFile (Path.concat [ p, "fruits", ".gitignore" ]) hugeGitignore
        FS.writeTextFile (Path.concat [ p, "fruits", "left", ".gitignore" ]) hugeGitignore
        FS.writeTextFile (Path.concat [ p, "fruits", "right", ".gitignore" ]) hugeGitignore
        a <- Glob.gitignoringGlob p [ "fruits/**/apple" ]
        Array.sort a `Assert.shouldEqual` [ "fruits/left/apple", "fruits/right/apple" ]
