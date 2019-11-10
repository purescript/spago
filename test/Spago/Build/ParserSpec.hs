module Spago.Build.ParserSpec (spec) where

import           Data.List.NonEmpty
import           Prelude
import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified Text.Megaparsec as Parser

import           Spago.Build.Parser    (PsModule (..), ModuleExportType(..))
import qualified Spago.Build.Parser    as Parser

spec :: Spec
spec = do
  describe "Parser for module declarations" $ do
    it "should fail on bad inputs" $ do
      let p = Parser.parse Parser.moduleDeclaration ""

      p `shouldFailOn` "module Test.Main () where"
      p `shouldFailOn` "module Test.Main (,) where"

    it "should succeed" $ do
      let p = Parser.parse Parser.moduleDeclaration ""

      p "module Test where"
        `shouldParse`
          PsModule "Test" Nothing

      p "module Test.Main where"
        `shouldParse`
          PsModule "Test.Main" Nothing

      p "module Test.Main (main) where"
        `shouldParse`
          PsModule "Test.Main" (Just $ fromList [ExportFunction "main"])

      p "module Test.Main (main, test) where"
        `shouldParse`
          PsModule "Test.Main" (Just $ fromList [ExportFunction"main", ExportFunction "test"])

      p "module A (module B) where"
        `shouldParse`
          PsModule "A" (Just $ fromList [ExportModule "B"])

      p "module A (module B.C) where"
        `shouldParse`
          PsModule "A" (Just $ fromList [ExportModule "B.C"])

      p "module A (module A, module B) where"
        `shouldParse`
          PsModule "A" (Just $ fromList [ExportModule "A", ExportModule "B"])

      p "module Test (class Foldable, foldr, foldl, foldMap) where"
        `shouldParse`
          PsModule "Test" (Just $ fromList $ [ ExportClass    "Foldable"
                                             , ExportFunction "foldr"
                                             , ExportFunction "foldl"
                                             , ExportFunction "foldMap"
                                             ]
                       )
