module Data.String.NonEmpty.CaseInsensitive where

import Prelude

import Data.Newtype (class Newtype)
import Data.String.NonEmpty (NonEmptyString, toLower)

-- | A newtype for case insensitive string comparisons and ordering.
newtype CaseInsensitiveNonEmptyString = CaseInsensitiveNonEmptyString NonEmptyString

instance eqCaseInsensitiveNonEmptyString :: Eq CaseInsensitiveNonEmptyString where
  eq (CaseInsensitiveNonEmptyString s1) (CaseInsensitiveNonEmptyString s2) =
    toLower s1 == toLower s2

instance ordCaseInsensitiveNonEmptyString :: Ord CaseInsensitiveNonEmptyString where
  compare (CaseInsensitiveNonEmptyString s1) (CaseInsensitiveNonEmptyString s2) =
    compare (toLower s1) (toLower s2)

instance showCaseInsensitiveNonEmptyString :: Show CaseInsensitiveNonEmptyString where
  show (CaseInsensitiveNonEmptyString s) = "(CaseInsensitiveNonEmptyString " <> show s <> ")"

derive instance newtypeCaseInsensitiveNonEmptyString :: Newtype CaseInsensitiveNonEmptyString _
