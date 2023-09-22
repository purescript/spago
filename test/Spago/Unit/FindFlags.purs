module Test.Spago.Unit.FindFlags where

import Prelude

import Data.Maybe (fromMaybe)
import Spago.Cmd as Cmd
import Test.Spec as Spec
import Test.Spec (Spec)
import Test.Spec.Assertions as Assertions

spec :: Spec Unit
spec = do
  Spec.describe "findFlags" $ do
    Spec.it "[\"-o\", \"something\"]" $ do
      let a = fromMaybe "" $ Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "-o", "something" ] }
      let b = "something"
      a `Assertions.shouldEqual` b
    Spec.it "[\"--output\", \"something\"]" $ do
      let a = fromMaybe "" $ Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "--output", "something" ] }
      let b = "something"
      a `Assertions.shouldEqual` b
    Spec.it "[\"-o something\"]" $ do
      let a = fromMaybe "" $ Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "-o something" ] }
      let b = "something"
      a `Assertions.shouldEqual` b
    Spec.it "[\"--output something\"]" $ do
      let a = fromMaybe "" $ Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "--output something" ] }
      let b = "something"
      a `Assertions.shouldEqual` b
    Spec.it "[\"--output=something\"]" $ do
      let a = fromMaybe "" $ Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "--output=something" ] }
      let b = "something"
      a `Assertions.shouldEqual` b
