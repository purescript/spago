module Options.Applicative.Internal.Utils
  ( unLines
  , unWords
  , lines
  , words
  , whitespaceRegex
  , startsWith
  , apApplyFlipped
  , (<**>)
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, intercalate)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafeCrashWith)

unLines :: forall f. Foldable f => f String -> String
unLines = intercalate "\n"

unWords :: forall f. Foldable f => f String -> String
unWords = intercalate " "

lines :: String -> Array String
lines "" = []
lines s = String.split (String.Pattern "\n") s

words :: String -> Array String
words "" = []
words s = Regex.split whitespaceRegex s

whitespaceRegex :: Regex
whitespaceRegex = case Regex.regex "\\s+" noFlags of
  Left err -> unsafeCrashWith $ "whitespaceRegex: `\\s+` seems to be invlaid, err: " <> err
  Right r -> r


startsWith :: String.Pattern -> String -> Boolean
startsWith p s = String.indexOf p s == Just 0

infixl 4 apApplyFlipped as <**>

apApplyFlipped :: forall f b a. Apply f => f a -> f (a -> b) -> f b
apApplyFlipped = lift2 (#)
