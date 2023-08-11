module Data.String.Regex.Unsafe
  ( unsafeRegex
  ) where

import Control.Category (identity)
import Data.Either (either)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (RegexFlags)
import Partial.Unsafe (unsafeCrashWith)

-- | Constructs a `Regex` from a pattern string and flags. Fails with
-- | an exception if the pattern contains a syntax error.
unsafeRegex :: String -> RegexFlags -> Regex
unsafeRegex s f = either unsafeCrashWith identity (regex s f)
