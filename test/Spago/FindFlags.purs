module Test.Spago.FindFlags where

import Prelude

import Data.Maybe (fromMaybe)
import Spago.Cmd as Cmd
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "findFlag" $ do
    it "[\"-o\", \"something\"]" $ do
      let a = fromMaybe "" $ Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "-o", "something" ] }
      let b = "something"
      a `shouldEqual` b
    it "[\"--output\", \"something\"]" $ do
      let a = fromMaybe "" $ Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "--output", "something" ] }
      let b = "something"
      a `shouldEqual` b
    it "[\"-o something\"]" $ do
      let a = fromMaybe "" $ Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "-o something" ] }
      let b = "something"
      a `shouldEqual` b
    it "[\"--output something\"]" $ do
      let a = fromMaybe "" $ Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "--output something" ] }
      let b = "something"
      a `shouldEqual` b
    it "[\"--output=something\"]" $ do
      let a = fromMaybe "" $ Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "--output=something" ] }
      let b = "something"
      a `shouldEqual` b
