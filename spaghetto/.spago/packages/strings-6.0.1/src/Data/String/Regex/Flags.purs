module Data.String.Regex.Flags where

import Prelude

import Control.MonadPlus (guard)
import Data.Newtype (class Newtype)
import Data.String (joinWith)

type RegexFlagsRec =
  { global :: Boolean
  , ignoreCase :: Boolean
  , multiline :: Boolean
  , dotAll :: Boolean
  , sticky :: Boolean
  , unicode :: Boolean
  }

-- | Flags that control matching.
newtype RegexFlags = RegexFlags RegexFlagsRec

derive instance newtypeRegexFlags :: Newtype RegexFlags _

-- | All flags set to false.
noFlags :: RegexFlags
noFlags = RegexFlags
  { global: false
  , ignoreCase: false
  , multiline: false
  , dotAll: false
  , sticky: false
  , unicode: false
  }

-- | Only global flag set to true
global :: RegexFlags
global = RegexFlags
  { global: true
  , ignoreCase: false
  , multiline: false
  , dotAll: false
  , sticky: false
  , unicode: false
  }

-- | Only ignoreCase flag set to true
ignoreCase :: RegexFlags
ignoreCase = RegexFlags
  { global: false
  , ignoreCase: true
  , multiline: false
  , dotAll: false
  , sticky: false
  , unicode: false
  }

-- | Only multiline flag set to true
multiline :: RegexFlags
multiline = RegexFlags
  { global: false
  , ignoreCase: false
  , multiline: true
  , dotAll: false
  , sticky: false
  , unicode: false
  }

-- | Only sticky flag set to true
sticky :: RegexFlags
sticky = RegexFlags
  { global: false
  , ignoreCase: false
  , multiline: false
  , dotAll: false
  , sticky: true
  , unicode: false
  }

-- | Only unicode flag set to true
unicode :: RegexFlags
unicode = RegexFlags
  { global: false
  , ignoreCase: false
  , multiline: false
  , dotAll: false
  , sticky: false
  , unicode: true
  }

-- | Only dotAll flag set to true
dotAll :: RegexFlags
dotAll = RegexFlags
  { global: false
  , ignoreCase: false
  , multiline: false
  , dotAll: true
  , sticky: false
  , unicode: false
  }

instance semigroupRegexFlags :: Semigroup RegexFlags where
  append (RegexFlags x) (RegexFlags y) = RegexFlags
    { global: x.global || y.global
    , ignoreCase: x.ignoreCase || y.ignoreCase
    , multiline: x.multiline || y.multiline
    , dotAll: x.dotAll || y.dotAll
    , sticky: x.sticky || y.sticky
    , unicode: x.unicode || y.unicode
    }

instance monoidRegexFlags :: Monoid RegexFlags where
  mempty = noFlags

derive newtype instance eqRegexFlags :: Eq RegexFlags

instance showRegexFlags :: Show RegexFlags where
  show (RegexFlags flags) =
    let
      usedFlags =
        []
        <> (guard flags.global $> "global")
        <> (guard flags.ignoreCase $> "ignoreCase")
        <> (guard flags.multiline $> "multiline")
        <> (guard flags.dotAll $> "dotAll")
        <> (guard flags.sticky $> "sticky")
        <> (guard flags.unicode $> "unicode")
    in
      if usedFlags == []
      then "noFlags"
      else "(" <> joinWith " <> " usedFlags <> ")"
