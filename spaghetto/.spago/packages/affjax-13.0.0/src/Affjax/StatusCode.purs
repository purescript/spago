module Affjax.StatusCode where

import Prelude
import Data.Newtype (class Newtype)

newtype StatusCode = StatusCode Int

derive instance eqStatusCode :: Eq StatusCode
derive instance ordStatusCode :: Ord StatusCode
derive instance newtypeStatusCode :: Newtype StatusCode _

instance showStatusCode :: Show StatusCode where
  show (StatusCode code) = "(StatusCode " <> show code <> ")"
