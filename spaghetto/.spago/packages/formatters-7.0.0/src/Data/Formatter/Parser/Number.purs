module Data.Formatter.Parser.Number
  ( parseInteger
  , parseMaybeInteger
  , parseNumber
  , parseDigit
  ) where

import Prelude

import Data.Int (toNumber)
import Data.Array (some)
import Data.Formatter.Internal (foldDigits)
import Data.Tuple (Tuple(..))
import Parsing as P
import Parsing.Combinators as PC
import Data.Formatter.Parser.Utils (oneOfAs)
import Parsing.String as PS
import Parsing.String.Basic as PSB
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Data.Foldable (foldMap)

parseInteger :: forall m. Monad m => P.ParserT String m Int
parseInteger = some parseDigit <#> foldDigits

parseMaybeInteger :: forall m. Monad m => P.ParserT String m (Maybe Int)
parseMaybeInteger = PC.optionMaybe parseInteger

parseFractional :: forall m. Monad m => P.ParserT String m Number
parseFractional = do
  digitStr <- (some parseDigit) <#> (foldMap show >>> ("0." <> _))
  case fromString digitStr of
    Just n -> pure n
    Nothing -> P.fail ("Not a number: " <> digitStr)

parseNumber :: forall m. Monad m => P.ParserT String m Number
parseNumber = (+)
  <$> (parseInteger <#> toNumber)
  <*> (PC.option 0.0 $ PC.try $ PSB.oneOf [ '.', ',' ] *> parseFractional)

parseDigit :: forall m. Monad m => P.ParserT String m Int
parseDigit = PC.try $ PS.char `oneOfAs`
  [ Tuple '0' 0
  , Tuple '1' 1
  , Tuple '2' 2
  , Tuple '3' 3
  , Tuple '4' 4
  , Tuple '5' 5
  , Tuple '6' 6
  , Tuple '7' 7
  , Tuple '8' 8
  , Tuple '9' 9
  ]
