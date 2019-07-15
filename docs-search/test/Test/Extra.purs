module Test.Extra where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Test.Unit.Assert as Assert

assertRight
  :: forall a b
  .  Show a
  => Show b
  => Eq a
  => Eq b
  => Either b a
  -> a
  -> Aff Unit
assertRight eiActual expected =
  case eiActual of
    Left string -> do
      Assert.equal (Right expected) eiActual
    Right actual -> do
      Assert.equal (Right expected) eiActual
