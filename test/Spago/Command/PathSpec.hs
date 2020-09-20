module Spago.Command.PathSpec (spec) where

import            Prelude
import            Test.Hspec

import            Spago.Prelude hiding (link)
import            Spago.Env
import qualified  Spago.Command.Path   as Path

spec :: Spec
spec = do
  describe "Path findFlag" $ do
    it "[\"-o\", \"something\"]" $ do
      let a = fromMaybe "" $ Path.findFlag 'o' "output" [PursArg "-o" , PursArg "something"]
      let b = "something"
      a `shouldBe` b
    it "[\"--output\", \"something\"]" $ do
      let a = fromMaybe "" $ Path.findFlag 'o' "output" [PursArg "--output" , PursArg "something"]
      let b = "something"
      a `shouldBe` b
    it "[\"-o something\"]" $ do
      let a = fromMaybe "" $ Path.findFlag 'o' "output" [PursArg "-o something"]
      let b = "something"
      a `shouldBe` b
    it "[\"--output something\"]" $ do
      let a = fromMaybe "" $ Path.findFlag 'o' "output" [PursArg "--output something"]
      let b = "something"
      a `shouldBe` b
    it "[\"--output=something\"]" $ do
      let a = fromMaybe "" $ Path.findFlag 'o' "output" [PursArg "--output=something"]
      let b = "something"
      a `shouldBe` b
