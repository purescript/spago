module Spago.PursSpec (spec) where

import            Prelude
import            Test.Hspec

import            Spago.Prelude hiding (link)
import            Spago.Env
import qualified  Spago.Purs as Purs

spec :: Spec
spec = do
  describe "findFlag" $ do
    it "[\"-o\", \"something\"]" $ do
      let a = fromMaybe "" $ Purs.findFlag 'o' "output" [PursArg "-o" , PursArg "something"]
      let b = "something"
      a `shouldBe` b
    it "[\"--output\", \"something\"]" $ do
      let a = fromMaybe "" $ Purs.findFlag 'o' "output" [PursArg "--output" , PursArg "something"]
      let b = "something"
      a `shouldBe` b
    it "[\"-o something\"]" $ do
      let a = fromMaybe "" $ Purs.findFlag 'o' "output" [PursArg "-o something"]
      let b = "something"
      a `shouldBe` b
    it "[\"--output something\"]" $ do
      let a = fromMaybe "" $ Purs.findFlag 'o' "output" [PursArg "--output something"]
      let b = "something"
      a `shouldBe` b
    it "[\"--output=something\"]" $ do
      let a = fromMaybe "" $ Purs.findFlag 'o' "output" [PursArg "--output=something"]
      let b = "something"
      a `shouldBe` b
