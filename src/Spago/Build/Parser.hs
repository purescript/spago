module Spago.Build.Parser
  ( ModuleExportType (..)
  , PsModule (..)
  , pModule
  , checkExistTestModule
  ) where

import           Spago.Prelude hiding (many, try)
import qualified RIO.ByteString as B
import qualified RIO.NonEmpty.Partial as NE'

import           Text.Megaparsec
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

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

pModule :: Parser PsModule
pModule = between (space *> symbol "module") (symbol "where") $ PsModule
  <$> lexeme pModuleFullName
  <*> optional (lexeme (between (symbol "(") (symbol ")") pModuleExportList))

pModuleFullName :: Parser ByteString
pModuleFullName = do
  res <- sepBy pModuleName (symbol ".")
  return (B.intercalate "." res)

pModuleName :: Parser ByteString
pModuleName = fmap B.pack ((:) <$> upperChar <*> many letterChar)

pModuleExportList :: Parser (NonEmpty ModuleExportType)
pModuleExportList = do
  let exportP = choice [pExportClassOrModule, pExportFunc]
  exports <- sepBy exportP (symbol ",")
  if null exports then
    fail "Invalid syntax: export list"
  else
    return $ NE'.fromList exports

pExportFunc :: Parser ModuleExportType
pExportFunc = do
  name <- B.pack <$> lexeme (many letterChar)
  if B.null name then
    fail "no input"
  else
    return $ ExportFunction name

pExportClassOrModule :: Parser ModuleExportType
pExportClassOrModule = ($)
  <$> choice [ ExportModule <$ string "module"
             , ExportClass  <$ string "class"
             ]
  <*  space1
  <*> pModuleFullName

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: ByteString -> Parser ByteString
symbol = L.symbol sc

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "{-" "-}")

checkExistTestModule :: ByteString -> Bool
checkExistTestModule content = case parse pModule "" content of
  Left _ -> False
  Right (PsModule name _) -> name == "Test.Main"