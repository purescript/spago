module PureScript.CST.Errors
  ( RecoveredError(..)
  , ParseError(..)
  , printParseError
  , printTokenError
  ) where

import Prelude

import PureScript.CST.Print (printQualified)
import PureScript.CST.Types (SourcePos, SourceStyle(..), Token(..), SourceToken)

newtype RecoveredError = RecoveredError
  { error :: ParseError
  , position :: SourcePos
  , tokens :: Array SourceToken
  }

data ParseError
  = UnexpectedEof
  | ExpectedEof Token
  | UnexpectedToken Token
  | ExpectedToken Token Token
  | ExpectedClass String Token
  | LexExpected String String
  | LexInvalidCharEscape String
  | LexCharEscapeOutOfRange String
  | LexHexOutOfRange String
  | LexIntOutOfRange String
  | LexNumberOutOfRange String

printParseError :: ParseError -> String
printParseError = case _ of
  UnexpectedEof ->
    "Unexpected end of file"
  ExpectedEof tok ->
    "Expected end of file, saw " <> printTokenError tok
  UnexpectedToken tok ->
    "Unexpected " <> printTokenError tok
  ExpectedToken tok saw ->
    "Expected " <> printTokenError tok <> ", saw " <> printTokenError saw
  ExpectedClass cls saw ->
    "Expected " <> cls <> ", saw " <> printTokenError saw
  LexExpected str saw ->
    "Expected " <> str <> ", saw " <> saw
  LexInvalidCharEscape str ->
    "Invalid character escape \\" <> str
  LexCharEscapeOutOfRange str ->
    "Character escape out of range \\" <> str
  LexHexOutOfRange str ->
    "Hex integer out of range 0x" <> str
  LexIntOutOfRange str ->
    "Int out of range " <> str
  LexNumberOutOfRange str ->
    "Number out of range " <> str

printTokenError :: Token -> String
printTokenError = case _ of
  TokLeftParen ->
    "'('"
  TokRightParen ->
    "')'"
  TokLeftBrace ->
    "'{'"
  TokRightBrace ->
    "'}'"
  TokLeftSquare ->
    "'['"
  TokRightSquare ->
    "']'"
  TokLeftArrow style ->
    case style of
      ASCII -> "'<-'"
      Unicode -> "'←'"
  TokRightArrow style ->
    case style of
      ASCII -> "'->'"
      Unicode -> "'→'"
  TokRightFatArrow style ->
    case style of
      ASCII -> "'=>'"
      Unicode -> "'⇒'"
  TokDoubleColon style ->
    case style of
      ASCII -> "'::'"
      Unicode -> "'∷'"
  TokForall style ->
    case style of
      ASCII -> "forall"
      Unicode -> "'∀'"
  TokEquals ->
    "'='"
  TokPipe ->
    "'|'"
  TokTick ->
    "`"
  TokDot ->
    "."
  TokComma ->
    "','"
  TokUnderscore ->
    "'_'"
  TokBackslash ->
    "'\\'"
  TokAt ->
    "'@'"
  TokLowerName moduleName name ->
    "identifier " <> printQualified moduleName name
  TokUpperName moduleName name ->
    "proper identifier " <> printQualified moduleName name
  TokOperator moduleName name ->
    "operator " <> printQualified moduleName name
  TokSymbolName moduleName name ->
    "symbol " <> printQualified moduleName name
  TokSymbolArrow style ->
    case style of
      ASCII -> "(->)"
      Unicode -> "(→)"
  TokHole name ->
    "hole ?" <> name
  TokChar raw _ ->
    "char literal '" <> raw <> "'"
  TokString _ _ ->
    "string literal"
  TokRawString _ ->
    "raw string literal"
  TokInt raw _ ->
    "int literal " <> raw
  TokNumber raw _ ->
    "number literal " <> raw
  TokLayoutStart _ ->
    "start of indented block"
  TokLayoutSep _ ->
    "new indented block item"
  TokLayoutEnd _ ->
    "end of indented block"
