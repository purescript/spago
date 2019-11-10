module Spago.Build.Parser
  ( ModuleExportType (..)
  , PsModule (..)
  , moduleDeclaration
  , checkModuleNameMatches
  ) where

import qualified RIO.ByteString             as ByteString
import qualified RIO.NonEmpty.Partial       as NonEmpty
import           Spago.Prelude              hiding (many, try)

import           Text.Megaparsec
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as Lexer

type Parser = Parsec Void ByteString

data PsModule = PsModule
  { psModuleName       :: ByteString
  , psModuleExportList :: Maybe (NonEmpty ModuleExportType)
  } deriving (Eq, Show)

data ModuleExportType
  = ExportModule   ByteString
  | ExportFunction ByteString
  | ExportClass    ByteString
  deriving (Eq, Show)

moduleDeclaration :: Parser PsModule
moduleDeclaration = between (space *> symbol "module") (symbol "where") $ PsModule
  <$> lexeme moduleFullName
  <*> optional (lexeme (between (symbol "(") (symbol ")") moduleExportList))

moduleFullName :: Parser ByteString
moduleFullName = do
  res <- sepBy moduleName (symbol ".")
  return (ByteString.intercalate "." res)

moduleName :: Parser ByteString
moduleName = fmap ByteString.pack ((:) <$> upperChar <*> many letterChar)

moduleExportList :: Parser (NonEmpty ModuleExportType)
moduleExportList = do
  let exportP = choice [ exportClassOrModule, exportFunc ]
  exports <- sepBy exportP (symbol ",")
  if null exports then
    fail "Invalid syntax: export list"
  else
    return $ NonEmpty.fromList exports

exportFunc :: Parser ModuleExportType
exportFunc = do
  name <- ByteString.pack <$> lexeme (many letterChar)
  if ByteString.null name then
    fail "no input"
  else
    return $ ExportFunction name

exportClassOrModule :: Parser ModuleExportType
exportClassOrModule = ($)
  <$> choice [ ExportModule <$ string "module"
             , ExportClass  <$ string "class"
             ]
  <*  space1
  <*> moduleFullName

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

symbol :: ByteString -> Parser ByteString
symbol = Lexer.symbol sc

sc :: Parser ()
sc = Lexer.space
  space1
  (Lexer.skipLineComment "//")
  (Lexer.skipBlockComment "{-" "-}")

checkModuleNameMatches :: ByteString -> ByteString -> Bool
checkModuleNameMatches expectedModuleName content = case parse moduleDeclaration "" content of
  Left _                  -> False
  Right (PsModule name _) -> name == expectedModuleName
