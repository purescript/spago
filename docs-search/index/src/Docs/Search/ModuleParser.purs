module Docs.Search.ModuleParser where

import Prelude

import Data.Either (hush)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe)
import Data.Newtype (wrap)
import Docs.Search.Types (ModuleName)
import StringParser (Parser, char, choice, many, manyTill, noneOf, regex, runParser, sepBy1, string, try, whiteSpace)

parseModuleName :: String -> Maybe ModuleName
parseModuleName = map wrap <<< hush <<< runParser do
  void $ many whiteSpaceOrComment
  moduleHeader

whiteSpaceOrComment :: Parser Unit
whiteSpaceOrComment =
  choice
    [ try $ multiLineComment
    , try $ singleLineComment
    , try $ void whiteSpace
    ]

multiLineComment :: Parser Unit
multiLineComment = do
  void $ string "{-"
  void $ manyTill (void $ noneOf []) (string "-}")

singleLineComment :: Parser Unit
singleLineComment = do
  void $ string "--"
  void $ manyTill (void $ noneOf [ '\n' ]) (char '\n')

moduleHeader :: Parser String
moduleHeader = do
  void $ string "module"
  void $ many whiteSpaceOrComment
  moduleName

moduleName :: Parser String
moduleName = sepBy1 moduleNameWord (string ".") <#> intercalate "."

moduleNameWord :: Parser String
moduleNameWord = do
  first <- regex "[A-Z]"
  rest <- regex "[A-Za-z0-9]*"
  pure $ first <> rest
