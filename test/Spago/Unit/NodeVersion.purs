module Test.Spago.Unit.NodeVersion where

import Prelude

import Spago.NodeVersion (NodeVersionCheck(..), checkNodeVersion)
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assertions

minimum :: { major :: Int, minor :: Int }
minimum = { major: 22, minor: 5 }

spec :: Spec Unit
spec = do
  Spec.describe "checkNodeVersion" do
    Spec.describe "accepts valid versions" do
      Spec.it "v22.5.0" do
        checkNodeVersion minimum "v22.5.0" `Assertions.shouldEqual` NodeVersionOk
      Spec.it "22.5.0" do
        checkNodeVersion minimum "22.5.0" `Assertions.shouldEqual` NodeVersionOk
      Spec.it "v22.6.0" do
        checkNodeVersion minimum "v22.6.0" `Assertions.shouldEqual` NodeVersionOk
      Spec.it "v23.0.0" do
        checkNodeVersion minimum "v23.0.0" `Assertions.shouldEqual` NodeVersionOk
      Spec.it "v25.2.1" do
        checkNodeVersion minimum "v25.2.1" `Assertions.shouldEqual` NodeVersionOk

    Spec.describe "rejects old versions" do
      Spec.it "v22.4.0" do
        checkNodeVersion minimum "v22.4.0" `Assertions.shouldEqual` NodeVersionTooOld "v22.4.0"
      Spec.it "v21.0.0" do
        checkNodeVersion minimum "v21.0.0" `Assertions.shouldEqual` NodeVersionTooOld "v21.0.0"
      Spec.it "v18.17.0" do
        checkNodeVersion minimum "v18.17.0" `Assertions.shouldEqual` NodeVersionTooOld "v18.17.0"

    Spec.describe "handles unparseable versions" do
      Spec.it "garbage" do
        checkNodeVersion minimum "garbage" `Assertions.shouldEqual` NodeVersionUnparseable "garbage"
      Spec.it "empty string" do
        checkNodeVersion minimum "" `Assertions.shouldEqual` NodeVersionUnparseable ""
