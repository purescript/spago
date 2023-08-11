module Node.Library.Execa.ParseCommand
  ( parseCommand
  , parseCommand'
  ) where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Either (Either, hush)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..), optional)
import Data.String.CodeUnits as SCU
import Data.String.NonEmpty as NES
import Data.String.NonEmpty.Internal (NonEmptyString(..))
import Data.Tuple (Tuple(..))
import Parsing (ParseError, fail, runParser)
import Parsing.Combinators.Array (many, many1)
import Parsing.String (anyChar, anyTill, eof, string)
import Parsing.String.Basic (skipSpaces)
import Safe.Coerce (coerce)

parseCommand :: String -> Maybe { file :: String, args :: Array String }
parseCommand = hush <<< parseCommand'

parseCommand' :: String -> Either ParseError { file :: String, args :: Array String }
parseCommand' = dropNES <<< parseCommand''
  where
  dropNES
    :: Either ParseError { file :: NonEmptyString, args :: Array NonEmptyString }
    -> Either ParseError { file :: String, args :: Array String }
  dropNES = coerce

parseCommand'' :: String -> Either ParseError { file :: NonEmptyString, args :: Array NonEmptyString }
parseCommand'' command = runParser command parseFileArgs
  where
  backslash = """\"""
  squote = "'"
  dquote = "\""
  space = " "

  parseFileArgs = do
    skipSpaces
    file <- parseArg
    args <- many parseArg
    pure { file, args }

  parseArg = do
    mbStartBoundary <- optional $ oneOf
      [ NonEmptyString <$> string dquote
      , NonEmptyString <$> string squote
      ]
    case mbStartBoundary of
      Nothing ->
        parseArgEnd ""
      Just quoteStr ->
        parseArgPart quoteStr ""

  parseArgEnd acc = do
    Tuple parsed (Tuple wasEscapedChar str) <- anyTill $ oneOf
      [ Tuple true <<< SCU.singleton <$> (string backslash *> anyChar)
      , Tuple false <<< NEA.fold1 <$> (many1 $ string space)
      , Tuple false "" <$ eof
      ]
    if wasEscapedChar then
      parseArgEnd (acc <> parsed <> str)
    else case NES.fromString $ acc <> parsed of
      Just x -> pure x
      Nothing -> fail "Arg was empty"

  parseArgPart :: NonEmptyString -> String -> _
  parseArgPart quoteStr acc = do
    Tuple parsed escapedQuote <- anyTill $ oneOf
      [ true <$ string (backslash <> NES.toString quoteStr)
      , false <$ string (NES.toString quoteStr)
      ]
    if escapedQuote then do
      parseArgPart quoteStr (acc <> parsed <> NES.toString quoteStr)
    else do
      skipSpaces
      case NES.fromString $ acc <> parsed of
        Just ns -> pure ns
        Nothing -> fail "Arg was empty"

data QuoteType
  = DoubleQuote
  | SingleQuote
  | EscapedDoubleQuote
  | EscapedSingleQuote

derive instance Eq QuoteType

data StopperType
  = EscapedSpace
  | EscapedQuote
  | RightBoundary
