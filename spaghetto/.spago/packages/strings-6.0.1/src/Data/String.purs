module Data.String
  ( module Data.String.Common
  , module Data.String.CodePoints
  , module Data.String.Pattern
  ) where

import Data.String.CodePoints

import Data.String.Common (joinWith, localeCompare, null, replace, replaceAll, split, toLower, toUpper, trim)
import Data.String.Pattern (Pattern(..), Replacement(..))
