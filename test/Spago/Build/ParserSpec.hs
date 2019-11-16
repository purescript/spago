module Spago.Build.ParserSpec (spec) where

import           Data.List.NonEmpty
import           Prelude
import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified Text.Megaparsec as Parser

import           Spago.Build.Parser    (PsModule (..), ModuleExportType(..), DataMembers(..))
import qualified Spago.Build.Parser    as Parser

spec :: Spec
spec = do
  describe "Parser for module declarations" $ do
    it "should fail on bad inputs" $ do
      let p = Parser.parse Parser.moduleDecl ""

      p `shouldFailOn` "module Test.Main () where"
      p `shouldFailOn` "module Test.Main (,) where"
      p `shouldFailOn` "module 1 where"
      p `shouldFailOn` "module test where"
      p `shouldFailOn` "module _ where"
      p `shouldFailOn` "module Test_Main where"
      p `shouldFailOn` "module Test' where"
      p `shouldFailOn` "module Test.Main (..) where"
      p `shouldFailOn` "module A..B where"
      p `shouldFailOn` "module A (T( .. )) where"
      p `shouldFailOn` "module A (M.T) where"
      p `shouldFailOn` "module A (T (f)) where"

    it "should succeed" $ do
      let p = Parser.parse Parser.moduleDecl ""

      p "module Test where"
        `shouldParse`
          PsModule "Test" Nothing

      p "module Test123 where"
        `shouldParse`
          PsModule "Test123" Nothing

      p "module Test.Main where"
        `shouldParse`
          PsModule "Test.Main" Nothing

      p "module Test.Main (main) where"
        `shouldParse`
          PsModule "Test.Main" (Just $ fromList [ExportValue "main"])
        
      p "module Test.Main (m_a_i_n) where"
        `shouldParse`
          PsModule "Test.Main" (Just $ fromList [ExportValue "m_a_i_n"])

      p "module Test.Main (main, test) where"
        `shouldParse`
          PsModule "Test.Main" (Just $ fromList [ExportValue "main", ExportValue "test"])

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
          PsModule "Test" (Just $ fromList $ [ ExportClass "Foldable"
                                             , ExportValue "foldr"
                                             , ExportValue "foldl"
                                             , ExportValue "foldMap"
                                             ]
                       )

      p "module Test (($)) where"
        `shouldParse`
          PsModule "Test" (Just $ fromList [ExportOp "$"])

      p "module Test (T) where"
        `shouldParse`
          PsModule "Test" (Just $ fromList [ExportType "T" Nothing])

      p "module Test (T (..)) where"
        `shouldParse`
          PsModule "Test" (Just $ fromList [ExportType "T" (Just DataAll)])

      p "module Test (T ()) where"
        `shouldParse`
          PsModule "Test" (Just $ fromList [ExportType "T" (Just $ DataEnumerated [])])

      p "module Test (T (S, U)) where"
        `shouldParse`
          PsModule "Test" (Just $ fromList [ExportType "T" (Just $ DataEnumerated ["S", "U"])])

      p "module Test (T (S, U)) where"
        `shouldParse`
          PsModule "Test" (Just $ fromList [ExportType "T" (Just $ DataEnumerated ["S", "U"])])

      p "module Test (type (++)) where"
        `shouldParse`
          PsModule "Test" (Just $ fromList [ExportTypeOp "++"])
      
      p "module Test (kind T) where"
        `shouldParse`
          PsModule "Test" (Just $ fromList [ExportKind "T"])

      p "-- | comment \nmodule T where"
        `shouldParse`
          PsModule "T" Nothing

      p "module T (HalogenIO\n, module Data.Lazy,module B) where"
        `shouldParse`
          PsModule "T" (Just $ fromList [ ExportType   "HalogenIO" Nothing
                                        , ExportModule "Data.Lazy"
                                        , ExportModule "B"
                                        ]
                       )

      p "module A (class_) where"
        `shouldParse`
          PsModule "A" (Just $ fromList [ExportValue "class_"])

      p "module A (module_) where"
        `shouldParse`
          PsModule "A" (Just $ fromList [ExportValue "module_"])