module Test.Extra where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Test.Spec.Assertions (shouldEqual)


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
    Left _ -> do
      Right expected `shouldEqual` eiActual
    Right _ -> do
      Right expected `shouldEqual` eiActual
