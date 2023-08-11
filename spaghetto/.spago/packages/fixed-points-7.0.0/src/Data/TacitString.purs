module Data.TacitString
  ( TacitString
  , hush
  ) where

import Prelude

newtype TacitString = TacitString String

derive instance eqTacitString :: Eq TacitString
derive instance ordTacitString :: Ord TacitString

instance showTacitString :: Show TacitString where
  show (TacitString str) = str

hush :: String -> TacitString
hush = TacitString
