module PureScript.CST.Print
  ( printToken
  , printSourceToken
  , TokenOption(..)
  , printTokenWithOption
  , printSourceTokenWithOption
  , printComment
  , printLineFeed
  , printQualified
  ) where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (unwrap)
import PureScript.CST.Types (Comment(..), LineFeed(..), ModuleName, SourceStyle(..), Token(..), SourceToken)

data TokenOption
  = ShowLayout
  | HideLayout

printSourceToken :: SourceToken -> String
printSourceToken = printSourceTokenWithOption HideLayout

printSourceTokenWithOption :: TokenOption -> SourceToken -> String
printSourceTokenWithOption option tok =
  foldMap (printComment printLineFeed) tok.leadingComments
    <> printTokenWithOption option tok.value
    <> foldMap (printComment absurd) tok.trailingComments

printToken :: Token -> String
printToken = printTokenWithOption HideLayout

printTokenWithOption :: TokenOption -> Token -> String
printTokenWithOption option = case _ of
  TokLeftParen ->
    "("
  TokRightParen ->
    ")"
  TokLeftBrace ->
    "{"
  TokRightBrace ->
    "}"
  TokLeftSquare ->
    "["
  TokRightSquare ->
    "]"
  TokLeftArrow style ->
    case style of
      ASCII -> "<-"
      Unicode -> "←"
  TokRightArrow style ->
    case style of
      ASCII -> "->"
      Unicode -> "→"
  TokRightFatArrow style ->
    case style of
      ASCII -> "=>"
      Unicode -> "⇒"
  TokDoubleColon style ->
    case style of
      ASCII -> "::"
      Unicode -> "∷"
  TokForall style ->
    case style of
      ASCII -> "forall"
      Unicode -> "∀"
  TokEquals ->
    "="
  TokPipe ->
    "|"
  TokTick ->
    "`"
  TokDot ->
    "."
  TokComma ->
    ","
  TokUnderscore ->
    "_"
  TokBackslash ->
    "\\"
  TokAt ->
    "@"
  TokLowerName moduleName name ->
    printQualified moduleName name
  TokUpperName moduleName name ->
    printQualified moduleName name
  TokOperator moduleName name ->
    printQualified moduleName name
  TokSymbolName moduleName name ->
    printQualified moduleName ("(" <> name <> ")")
  TokSymbolArrow style ->
    case style of
      ASCII -> "(->)"
      Unicode -> "(→)"
  TokHole name ->
    "?" <> name
  TokChar raw _ ->
    "'" <> raw <> "'"
  TokString raw _ ->
    "\"" <> raw <> "\""
  TokRawString raw ->
    "\"\"\"" <> raw <> "\"\"\""
  TokInt raw _ ->
    raw
  TokNumber raw _ ->
    raw
  TokLayoutStart _ ->
    case option of
      ShowLayout -> "{"
      HideLayout -> ""
  TokLayoutSep _ ->
    case option of
      ShowLayout -> ";"
      HideLayout -> ""
  TokLayoutEnd _ ->
    case option of
      ShowLayout -> "}"
      HideLayout -> ""

printQualified :: Maybe ModuleName -> String -> String
printQualified moduleName name = case moduleName of
  Nothing -> name
  Just mn -> unwrap mn <> "." <> name

printComment :: forall l. (l -> String) -> Comment l -> String
printComment k = case _ of
  Comment str -> str
  Space n -> power " " n
  Line l n -> power (k l) n

printLineFeed :: LineFeed -> String
printLineFeed = case _ of
  LF -> "\n"
  CRLF -> "\r\n"
