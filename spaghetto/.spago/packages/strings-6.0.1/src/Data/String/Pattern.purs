module Data.String.Pattern where

import Prelude

import Data.Newtype (class Newtype)

-- | A newtype used in cases where there is a string to be matched.
-- |
-- | ```purescript
-- | pursPattern = Pattern ".purs"
-- | --can be used like this:
-- | contains pursPattern "Test.purs"
-- |    == true
-- | ```
-- |
newtype Pattern = Pattern String

derive instance eqPattern :: Eq Pattern
derive instance ordPattern :: Ord Pattern
derive instance newtypePattern :: Newtype Pattern _

instance showPattern :: Show Pattern where
  show (Pattern s) = "(Pattern " <> show s <> ")"

-- | A newtype used in cases to specify a replacement for a pattern.
newtype Replacement = Replacement String

derive instance eqReplacement :: Eq Replacement
derive instance ordReplacement :: Ord Replacement
derive instance newtypeReplacement :: Newtype Replacement _

instance showReplacement :: Show Replacement where
  show (Replacement s) = "(Replacement " <> show s <> ")"
