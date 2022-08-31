module Test.ModuleParser where

import Docs.Search.IndexBuilder (getPathsByGlobs)
import Docs.Search.ModuleParser (multiLineComment, parseModuleName, singleLineComment)
import Docs.Search.Types (ModuleName(..))

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import StringParser (runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  describe "ModuleParser" do
    it "test #0" do
      parseModuleName "module Foo" `shouldEqual` Just (ModuleName "Foo")
    it "test #1" do
      parseModuleName "module Foo.Bar.B" `shouldEqual` Just (ModuleName "Foo.Bar.B")
    it "test #2" do
      parseModuleName "   module Foo" `shouldEqual` Just (ModuleName "Foo")
    it "test #3" do
      parseModuleName " {- asdas -} module Foo" `shouldEqual` Just (ModuleName "Foo")
    it "test #4" do
      parseModuleName "{--}module Foo" `shouldEqual` Just (ModuleName "Foo")
    it "test #5" do
      parseModuleName "--\nmodule Foo" `shouldEqual` Just (ModuleName "Foo")
    it "test #6" do
      parseModuleName "--\n-- foo\nmodule Foo" `shouldEqual` Just (ModuleName "Foo")
    it "test #7" do
      parseModuleName "-- \n  -- foo\n {- bar -} --baz\n module Foo" `shouldEqual` Just (ModuleName "Foo")
    it "multiline comment #1" do
      runParser multiLineComment "{--}" `shouldEqual` Right unit
    it "multiline comment #1" do
      runParser multiLineComment "{- foo -}" `shouldEqual` Right unit
    it "multiline comment #1" do
      runParser multiLineComment "{- foo\nbar\nbar\n -}" `shouldEqual` Right unit
    it "single line comment #1" do
      runParser singleLineComment "-- asd\n" `shouldEqual` Right unit
    it "single line comment #1" do
      runParser singleLineComment "--\n" `shouldEqual` Right unit
    it "Parses every module in .spago/" do
      files <- getPathsByGlobs ["./.spago/**/*.purs"]
      liftEffect $ Console.log $ "Modules in .spago: " <> show (Array.length files)
      for_ files \filePath -> do
        fileContents <- readTextFile UTF8 filePath
        case parseModuleName fileContents of
          Nothing -> do
            liftEffect $ throw $
              "Module header decoding failed for " <> filePath <>
              ", unable to extract module name"
          Just _ -> pure unit
