module Spago.Build.ParserSpec (spec) where

import Prelude
import Test.Hspec
import Text.Megaparsec
import Test.Hspec.Megaparsec
import Data.List.NonEmpty

import Spago.Build.Parser

spec :: Spec
spec = do
  describe "PureScript Module Parser" $ do
    it "pModule (fail)" $ do
      let p = parse pModule ""

      p `shouldFailOn` "module Test.Main () where"
      p `shouldFailOn` "module Test.Main (,) where"

    it "pModule (success)" $ do
      let p = parse pModule ""
      
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