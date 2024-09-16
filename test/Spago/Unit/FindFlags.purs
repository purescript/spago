module Test.Spago.Unit.FindFlags where

import Prelude

import Data.Maybe (Maybe(..))
import Spago.Cmd as Cmd
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assertions

spec :: Spec Unit
spec = do
  Spec.describe "findFlags" $ do
    Spec.it "[\"-o\", \"something\"]" $ do
      let a = Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "-o", "something" ] }
      let b = Just "something"
      a `Assertions.shouldEqual` b
    Spec.it "[\"--output\", \"something\"]" $ do
      let a = Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "--output", "something" ] }
      let b = Just "something"
      a `Assertions.shouldEqual` b
    Spec.it "[\"-o something\"]" $ do
      let a = Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "-o something" ] }
      let b = Just "something"
      a `Assertions.shouldEqual` b
    Spec.it "[\"--output something\"]" $ do
      let a = Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "--output something" ] }
      let b = Just "something"
      a `Assertions.shouldEqual` b
    Spec.it "[\"--output=something\"]" $ do
      let a = Cmd.findFlag { flags: [ "-o", "--output" ], args: [ "--output=something" ] }
      let b = Just "something"
      a `Assertions.shouldEqual` b
    Spec.it "[ '--verbose-errors', '--json-errors' ]" do
      let a = Cmd.findFlag { flags: [ "--json-errors" ], args: [ "--verbose-errors", "--json-errors" ] }
      let b = Just ""
      a `Assertions.shouldEqual` b
