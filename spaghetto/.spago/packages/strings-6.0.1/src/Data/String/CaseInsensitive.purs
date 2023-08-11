module Data.String.CaseInsensitive where

import Prelude

import Data.Newtype (class Newtype)
import Data.String (toLower)

-- | A newtype for case insensitive string comparisons and ordering.
newtype CaseInsensitiveString = CaseInsensitiveString String

instance eqCaseInsensitiveString :: Eq CaseInsensitiveString where
  eq (CaseInsensitiveString s1) (CaseInsensitiveString s2) =
    toLower s1 == toLower s2

instance ordCaseInsensitiveString :: Ord CaseInsensitiveString where
  compare (CaseInsensitiveString s1) (CaseInsensitiveString s2) =
    compare (toLower s1) (toLower s2)

instance showCaseInsensitiveString :: Show CaseInsensitiveString where
  show (CaseInsensitiveString s) = "(CaseInsensitiveString " <> show s <> ")"

derive instance newtypeCaseInsensitiveString :: Newtype CaseInsensitiveString _
