module Test.ModuleParser where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Docs.Search.ModuleParser (multiLineComment, parseModuleName, singleLineComment)
import Docs.Search.Types (ModuleName(..))
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
