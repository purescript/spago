module Data.Either.Inject where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))

class Inject a b where
  inj :: a -> b
  prj :: b -> Maybe a

instance injectReflexive :: Inject a a where
  inj = identity
  prj = Just

else instance injectLeft :: Inject a (Either a b) where
  inj = Left
  prj = either Just (const Nothing)

else instance injectRight :: Inject a b => Inject a (Either c b) where
  inj = Right <<< inj
  prj = either (const Nothing) prj

