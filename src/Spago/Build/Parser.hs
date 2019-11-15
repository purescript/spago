module Spago.Build.Parser
  ( ModuleExportType (..)
  , PsModule (..)
  , DataMembers (..)
  , moduleDecl
  , checkModuleNameMatches
  ) where

import qualified RIO.ByteString             as ByteString
import qualified RIO.Char                   as Char
import qualified RIO.NonEmpty.Partial       as NonEmpty
import           Spago.Prelude              hiding (many, try, some)

import qualified Data.ByteString.Internal   as ByteString (w2c)

import           Text.Megaparsec
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as Lexer

type Parser = Parsec Void ByteString

data PsModule = PsModule
  { psModuleName       :: ByteString
  , psModuleExportList :: Maybe (NonEmpty ModuleExportType)
  } deriving (Eq, Show)

data ModuleExportType
  = ExportValue    ByteString
  | ExportOp       ByteString
  | ExportType     ByteString (Maybe DataMembers)
  | ExportTypeOp   ByteString
  | ExportClass    ByteString
  | ExportKind     ByteString
  | ExportModule   ByteString
  deriving (Eq, Show)

data DataMembers
  = DataAll
  | DataEnumerated [ByteString]
  deriving (Eq, Show)

moduleDecl :: Parser PsModule
moduleDecl = between (space *> symbol "module") (symbol "where") $
  PsModule
    <$> lexeme moduleFullName
    <*> optional moduleExports

moduleFullName :: Parser ByteString
moduleFullName = ByteString.intercalate "." <$> sepBy moduleName (symbol ".")

moduleName :: Parser ByteString
moduleName = toName <$> upperChar <*> many alphaNumChar
  where
    toName pre rest = ByteString.pack (pre:rest)

moduleExports :: Parser (NonEmpty ModuleExportType)
moduleExports = lexeme $ between (symbol "(") (symbol ")") moduleExportList

moduleExportList :: Parser (NonEmpty ModuleExportType)
moduleExportList = do 
  exports <- sepBy moduleExport (symbol ",")
  if null exports then
    fail "Invalid syntax: export list"
  else
    return $ NonEmpty.fromList exports

moduleExport :: Parser ModuleExportType
moduleExport = choice
  [ exportTypeOp
  , exportClass
  , exportKind
  , exportModule
  , exportType
  , exportOp
  , exportValue
  ]

exportTypeOp :: Parser ModuleExportType
exportTypeOp = ExportTypeOp <$ symbol "type" <*> exportSymbol

exportClass :: Parser ModuleExportType
exportClass = ExportClass <$ symbol "class" <*> properName

exportKind :: Parser ModuleExportType
exportKind = ExportKind <$ symbol "kind" <*> properName

exportModule :: Parser ModuleExportType
exportModule = ExportModule <$ symbol "module" <*> moduleFullName

exportValue :: Parser ModuleExportType
exportValue = do
  pre  <- lowerChar <|> underscore
  rest <- many identChar

  if pre == 95 && null rest then
    fail "Unexpected token _"
  else
    return $ ExportValue (ByteString.pack (pre:rest))

exportOp :: Parser ModuleExportType
exportOp = ExportOp <$> exportSymbol

exportSymbol :: Parser ByteString
exportSymbol = ByteString.pack <$> between (string "(") (string ")") (many (satisfy isSymbolChar))
  where
    isSymbolChar c =  c `ByteString.elem` ":!#$%&*+./<=>?@\\^|-~"
                  || ((not . Char.isAscii) $ ByteString.w2c c) && (Char.isSymbol $ ByteString.w2c c)

exportType :: Parser ModuleExportType
exportType = ExportType <$> lexeme properName <*> optional dataMembers

properName :: Parser ByteString
properName = f <$> upperChar <*> many identChar
  where
    f pre rest = ByteString.pack (pre:rest)

dataMembers :: Parser DataMembers
dataMembers = choice
  [ DataAll           <$  string "(..)"
  , DataEnumerated [] <$  string "()"
  , DataEnumerated    <$> between (string "(") (string ")") (sepBy properName (symbol ","))
  ]

underscore :: Parser Word8
underscore = char 95

identChar :: Parser Word8
identChar = alphaNumChar <|> underscore <|> tick
  where
    tick = char 39  -- '

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
checkModuleNameMatches expectedModuleName content = case parse moduleDecl "" content of
  Left _                  -> False
  Right (PsModule name _) -> name == expectedModuleName
