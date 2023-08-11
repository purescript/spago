module PureScript.CST.Parser
  ( Recovered
  , parseModule
  , parseModuleHeader
  , parseModuleBody
  , parseImportDecl
  , parseDecl
  , parseType
  , parseExpr
  , parseBinder
  ) where

import Prelude
import Prim hiding (Type, Row)

import Control.Alt (alt)
import Control.Lazy (defer)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), uncurry)
import Prim as P
import PureScript.CST.Errors (ParseError(..), RecoveredError(..))
import PureScript.CST.Layout (currentIndent)
import PureScript.CST.Parser.Monad (Parser, eof, lookAhead, many, optional, recover, take, try)
import PureScript.CST.TokenStream (TokenStep(..), TokenStream, layoutStack)
import PureScript.CST.TokenStream as TokenStream
import PureScript.CST.Types (Binder(..), ClassFundep(..), DataCtor(..), DataMembers(..), Declaration(..), Delimited, DoStatement(..), Export(..), Expr(..), Fixity(..), FixityOp(..), Foreign(..), Guarded(..), GuardedExpr(..), Ident(..), Import(..), ImportDecl(..), Instance(..), InstanceBinding(..), IntValue(..), Label(..), Labeled(..), LetBinding(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), OneOrDelimited(..), Operator(..), PatternGuard(..), Proper(..), QualifiedName(..), RecordLabeled(..), RecordUpdate(..), Role(..), Row(..), Separated(..), SourceToken, Token(..), Type(..), TypeVarBinding(..), Where(..), Wrapped(..))

type Recovered :: (P.Type -> P.Type) -> P.Type
type Recovered f = f RecoveredError

type RecoveryStrategy f = Parser (Recovered f) -> Parser (Recovered f)

-- Right associated alts are more efficient for the parser interpreter since
-- it doesn't have to build and walk a stack for each chain greedily, but it
-- can expand them on demand.
infixr 3 alt as <|>

expectMap :: forall a. (SourceToken -> Maybe a) -> Parser a
expectMap k = take \tok ->
  case k tok of
    Just a ->
      Right a
    Nothing ->
      Left $ UnexpectedToken tok.value

expect :: (Token -> Boolean) -> Parser SourceToken
expect pred = expectMap \tok ->
  if pred tok.value then Just tok else Nothing

wrapped :: forall a. Parser SourceToken -> Parser SourceToken -> Parser a -> Parser (Wrapped a)
wrapped openTok closeTok valueParser = do
  open <- openTok
  value <- valueParser
  close <- closeTok
  pure $ Wrapped { open, value, close }

delimited :: forall a. Parser SourceToken -> Parser SourceToken -> Parser SourceToken -> Parser a -> Parser (Delimited a)
delimited openTok closeTok sepTok valueParser = do
  open <- openTok
  parseEmpty open
    <|> parseNonEmpty open
  where
  parseEmpty :: SourceToken -> Parser (Delimited a)
  parseEmpty open = ado
    close <- closeTok
    in Wrapped { open, value: Nothing, close }

  parseNonEmpty :: SourceToken -> Parser (Delimited a)
  parseNonEmpty open = ado
    value <- separated sepTok valueParser
    close <- closeTok
    in Wrapped { open, value: Just value, close }

separated :: forall a. Parser SourceToken -> Parser a -> Parser (Separated a)
separated sepParser valueParser = ado
  head <- valueParser
  tail <- many (Tuple <$> sepParser <*> valueParser)
  in Separated { head, tail }

parens :: forall a. Parser a -> Parser (Wrapped a)
parens = wrapped tokLeftParen tokRightParen

braces :: forall a. Parser a -> Parser (Wrapped a)
braces = wrapped tokLeftBrace tokRightBrace

layoutNonEmpty :: forall a. Parser a -> Parser (NonEmptyArray a)
layoutNonEmpty valueParser = ado
  head <- tokLayoutStart *> valueParser
  tail <- many (tokLayoutSep *> valueParser) <* tokLayoutEnd
  in NonEmptyArray.cons' head tail

parseModule :: Parser (Recovered Module)
parseModule = do
  header <- parseModuleHeader
  body <- parseModuleBody
  pure $ Module { header, body }

parseModuleHeader :: Parser (Recovered ModuleHeader)
parseModuleHeader = do
  keyword <- tokKeyword "module"
  name <- parseModuleName
  exports <- optional $ parens $ separated tokComma parseExport
  where_ <- tokKeyword "where"
  imports <- tokLayoutStart *> parseModuleImportDecls
  pure $ ModuleHeader { keyword, name, exports, where: where_, imports }

parseModuleBody :: Parser (Recovered ModuleBody)
parseModuleBody = do
  decls <- parseModuleDecls <* tokLayoutEnd
  Tuple end trailingComments <- eof
  pure $ ModuleBody { decls, trailingComments, end }

parseModuleImportDecls :: Parser (Array (Recovered ImportDecl))
parseModuleImportDecls = many (parseImportDecl <* (tokLayoutSep <|> lookAhead tokLayoutEnd))

parseModuleDecls :: Parser (Array (Recovered Declaration))
parseModuleDecls = many (recoverDecl parseDecl <* (tokLayoutSep <|> lookAhead tokLayoutEnd))

parseExport :: Parser (Recovered Export)
parseExport =
  ExportTypeOp <$> tokKeyword "type" <*> parseSymbol
    <|> ExportClass <$> tokKeyword "class" <*> parseProper
    <|> ExportModule <$> tokKeyword "module" <*> parseModuleName
    <|> ExportOp <$> parseSymbol
    <|> ExportValue <$> parseIdent
    <|> ExportType <$> parseProper <*> optional parseDataMembers

parseImportDecl :: Parser (Recovered ImportDecl)
parseImportDecl = do
  keyword <- tokKeyword "import"
  module_ <- parseModuleName
  names <- optional $ Tuple <$> optional (tokKeyword "hiding") <*> parens (separated tokComma parseImport)
  qualified <- optional $ Tuple <$> tokKeyword "as" <*> parseModuleName
  pure $ ImportDecl { keyword, "module": module_, names, qualified }

parseImport :: Parser (Recovered Import)
parseImport =
  ImportOp <$> parseSymbol
    <|> ImportType <$> parseProper <*> optional parseDataMembers
    <|> ImportTypeOp <$> tokKeyword "type" <*> parseSymbol
    <|> ImportClass <$> tokKeyword "class" <*> parseProper
    <|> ImportValue <$> parseIdent

parseDataMembers :: Parser DataMembers
parseDataMembers =
  DataAll <$> tokKeySymbol ".."
    <|> DataEnumerated <$> delimited tokLeftParen tokRightParen tokComma parseProper

parseDecl :: Parser (Recovered Declaration)
parseDecl = do
  parseDeclData
    <|> parseDeclNewtype
    <|> parseDeclType
    <|> parseDeclClass
    <|> parseDeclInstanceChain
    <|> parseDeclDerive
    <|> parseDeclValue
    <|> parseDeclForeign
    <|> parseDeclFixity

parseDeclKindSignature :: SourceToken -> Name Proper -> Parser (Recovered Declaration)
parseDeclKindSignature keyword label = do
  separator <- tokDoubleColon
  value <- parseType
  pure $ DeclKindSignature keyword $ Labeled { label, separator, value }

parseDeclData :: Parser (Recovered Declaration)
parseDeclData = do
  keyword <- tokKeyword "data"
  name <- parseProper
  parseDeclKindSignature keyword name
    <|> parseDeclData1 keyword name

parseDeclData1 :: SourceToken -> Name Proper -> Parser (Recovered Declaration)
parseDeclData1 keyword name = do
  vars <- many parseTypeVarBinding
  ctors <- optional (Tuple <$> tokEquals <*> separated tokPipe parseDataCtor)
  pure $ DeclData { keyword, name, vars } ctors

parseDataCtor :: Parser (Recovered DataCtor)
parseDataCtor = ado
  name <- parseProper
  fields <- many parseTypeAtom
  in DataCtor { name, fields }

parseDeclNewtype :: Parser (Recovered Declaration)
parseDeclNewtype = do
  keyword <- tokKeyword "newtype"
  name <- parseProper
  parseDeclKindSignature keyword name
    <|> parseDeclNewtype1 keyword name

parseDeclNewtype1 :: SourceToken -> Name Proper -> Parser (Recovered Declaration)
parseDeclNewtype1 keyword name = do
  vars <- many parseTypeVarBinding
  tok <- tokEquals
  wrapper <- parseProper
  body <- parseTypeAtom
  pure $ DeclNewtype { keyword, name, vars } tok wrapper body

parseDeclType :: Parser (Recovered Declaration)
parseDeclType = do
  keyword <- tokKeyword "type"
  parseDeclRole keyword
    <|> parseDeclType1 keyword

parseDeclType1 :: SourceToken -> Parser (Recovered Declaration)
parseDeclType1 keyword = do
  name <- parseProper
  parseDeclKindSignature keyword name
    <|> parseDeclType2 keyword name

parseDeclType2 :: SourceToken -> Name Proper -> Parser (Recovered Declaration)
parseDeclType2 keyword name = do
  vars <- many parseTypeVarBinding
  tok <- tokEquals
  body <- parseType
  pure $ DeclType { keyword, name, vars } tok body

parseDeclRole :: SourceToken -> Parser (Recovered Declaration)
parseDeclRole keyword1 = do
  keyword2 <- tokKeyword "role"
  name <- parseProper
  roles <- many1 parseRole
  pure $ DeclRole keyword1 keyword2 name roles

parseRole :: Parser (Tuple SourceToken Role)
parseRole =
  flip Tuple Representational <$> tokKeyword "representational"
    <|> flip Tuple Nominal <$> tokKeyword "nominal"
    <|> flip Tuple Phantom <$> tokKeyword "phantom"

parseDeclClass :: Parser (Recovered Declaration)
parseDeclClass = do
  keyword <- tokKeyword "class"
  parseDeclClassSignature keyword
    <|> parseDeclClass1 keyword

parseDeclClassSignature :: SourceToken -> Parser (Recovered Declaration)
parseDeclClassSignature keyword = do
  Tuple label separator <- try $ Tuple <$> parseProper <*> tokDoubleColon
  value <- parseType
  pure $ DeclKindSignature keyword $ Labeled { label, separator, value }

parseDeclClass1 :: SourceToken -> Parser (Recovered Declaration)
parseDeclClass1 keyword = do
  super <- optional $ try $ Tuple <$> parseClassConstraints parseType5 <*> tokLeftFatArrow
  name <- parseProper
  vars <- many parseTypeVarBinding
  fundeps <- optional $ Tuple <$> tokPipe <*> separated tokComma parseFundep
  members <- optional $ Tuple <$> tokKeyword "where" <*> layoutNonEmpty parseClassMember
  pure $ DeclClass { keyword, super, name, vars, fundeps } members

parseClassConstraints :: Parser (Recovered Type) -> Parser (OneOrDelimited (Recovered Type))
parseClassConstraints parseOneConstraint = do
  Many <$> parens (separated tokComma parseType)
    <|> One <$> parseOneConstraint

parseClassMember :: Parser (Labeled (Name Ident) (Recovered Type))
parseClassMember = do
  label <- parseIdent
  separator <- tokDoubleColon
  value <- parseType
  pure $ Labeled { label, separator, value }

parseFundep :: Parser ClassFundep
parseFundep =
  FundepDetermined <$> tokRightArrow <*> many1 parseIdent
    <|> FundepDetermines <$> many1 parseIdent <*> tokRightArrow <*> many1 parseIdent

parseDeclInstanceChain :: Parser (Recovered Declaration)
parseDeclInstanceChain = DeclInstanceChain <$> separated parseInstanceChainSeparator parseInstance

parseInstanceChainSeparator :: Parser SourceToken
parseInstanceChainSeparator =
  tokKeyword "else"
    <* optional tokLayoutSep

parseInstance :: Parser (Recovered Instance)
parseInstance = do
  keyword <- tokKeyword "instance"
  name <- optional parseInstanceName
  constraints <- optional $ try $ Tuple <$> parseClassConstraints parseType3 <*> tokRightFatArrow
  className <- parseQualifiedProper
  types <- many parseTypeAtom
  body <- optional $ Tuple <$> tokKeyword "where" <*> layoutNonEmpty parseInstanceBinding
  pure $ Instance
    { head: { keyword, name, constraints, className, types }
    , body
    }

parseInstanceName :: Parser (Tuple (Name Ident) SourceToken)
parseInstanceName = Tuple <$> parseIdent <*> tokDoubleColon

parseInstanceBinding :: Parser (Recovered InstanceBinding)
parseInstanceBinding = do
  ident <- parseIdent
  parseInstanceBindingSignature ident
    <|> parseInstanceBindingName ident

parseInstanceBindingSignature :: Name Ident -> Parser (Recovered InstanceBinding)
parseInstanceBindingSignature label = do
  separator <- tokDoubleColon
  value <- parseType
  pure $ InstanceBindingSignature $ Labeled { label, separator, value }

parseInstanceBindingName :: Name Ident -> Parser (Recovered InstanceBinding)
parseInstanceBindingName name = do
  binders <- many parseBinderAtom
  guarded <- parseGuarded (tokEquals)
  pure $ InstanceBindingName { name, binders, guarded }

parseDeclDerive :: Parser (Recovered Declaration)
parseDeclDerive = do
  derive_ <- tokKeyword "derive"
  newtype_ <- optional $ tokKeyword "newtype"
  keyword <- tokKeyword "instance"
  name <- optional parseInstanceName
  constraints <- optional $ try $ Tuple <$> parseClassConstraints parseType3 <*> tokRightFatArrow
  className <- parseQualifiedProper
  types <- many parseTypeAtom
  pure $ DeclDerive derive_ newtype_ { keyword, name, constraints, className, types }

parseDeclValue :: Parser (Recovered Declaration)
parseDeclValue = do
  ident <- parseIdent
  parseDeclSignature ident
    <|> parseDeclValue1 ident

parseDeclSignature :: Name Ident -> Parser (Recovered Declaration)
parseDeclSignature label = do
  separator <- tokDoubleColon
  value <- parseType
  pure $ DeclSignature $ Labeled { label, separator, value }

parseDeclValue1 :: Name Ident -> Parser (Recovered Declaration)
parseDeclValue1 name = do
  binders <- many parseBinderAtom
  guarded <- parseGuarded tokEquals
  pure $ DeclValue { name, binders, guarded }

parseDeclForeign :: Parser (Recovered Declaration)
parseDeclForeign = do
  keyword1 <- tokKeyword "foreign"
  keyword2 <- tokKeyword "import"
  foreign_ <- parseForeignData <|> parseForeignKind <|> parseForeignValue
  pure $ DeclForeign keyword1 keyword2 foreign_

parseForeignData :: Parser (Recovered Foreign)
parseForeignData = do
  keyword <- tokKeyword "data"
  label <- parseProper
  separator <- tokDoubleColon
  value <- parseType
  pure $ ForeignData keyword $ Labeled { label, separator, value }

parseForeignKind :: Parser (Recovered Foreign)
parseForeignKind = try $ ForeignKind <$> tokKeyword "kind" <*> parseProper

parseForeignValue :: Parser (Recovered Foreign)
parseForeignValue = do
  label <- parseIdent
  separator <- tokDoubleColon
  value <- parseType
  pure $ ForeignValue $ Labeled { label, separator, value }

parseDeclFixity :: Parser (Recovered Declaration)
parseDeclFixity = do
  keyword <- parseFixityKeyword
  prec <- parseSmallInt
  operator <- parseFixityOp
  pure $ DeclFixity { keyword, prec, operator }

parseFixityKeyword :: Parser (Tuple SourceToken Fixity)
parseFixityKeyword =
  flip Tuple Infix <$> tokKeyword "infix"
    <|> flip Tuple Infixl <$> tokKeyword "infixl"
    <|> flip Tuple Infixr <$> tokKeyword "infixr"

parseFixityOp :: Parser FixityOp
parseFixityOp =
  FixityType <$> tokKeyword "type" <*> parseQualifiedProper <*> tokKeyword "as" <*> parseOperator
    <|> FixityValue <$> parseQualifiedIdentOrProper <*> tokKeyword "as" <*> parseOperator

parseType :: Parser (Recovered Type)
parseType = defer \_ -> do
  ty <- parseType1
  TypeKinded ty <$> tokDoubleColon <*> parseType
    <|> pure ty

parseType1 :: Parser (Recovered Type)
parseType1 = defer \_ -> do
  parseForall
    <|> parseType2

parseType2 :: Parser (Recovered Type)
parseType2 = defer \_ -> do
  ty <- parseType3
  TypeArrow ty <$> tokRightArrow <*> parseType1
    <|> TypeConstrained ty <$> tokRightFatArrow <*> parseType1
    <|> pure ty

parseType3 :: Parser (Recovered Type)
parseType3 = defer \_ -> do
  ty <- parseType4
  ops <- many (Tuple <$> parseQualifiedOperator <*> parseType4)
  pure case NonEmptyArray.fromArray ops of
    Nothing -> ty
    Just os -> TypeOp ty os

parseType4 :: Parser (Recovered Type)
parseType4 = defer \_ -> do
  parseTypeNegative <|> parseType5

parseType5 :: Parser (Recovered Type)
parseType5 = defer \_ -> do
  ty <- parseTypeAtom
  args <- many parseTypeAtom
  pure case NonEmptyArray.fromArray args of
    Nothing -> ty
    Just as -> TypeApp ty as

parseTypeAtom :: Parser (Recovered Type)
parseTypeAtom = defer \_ ->
  TypeVar <$> parseIdent
    <|> TypeConstructor <$> parseQualifiedProper
    <|> uncurry TypeString <$> parseString
    <|> uncurry (TypeInt Nothing) <$> parseInt
    <|> parseTypeParens
    <|> TypeRecord <$> braces parseRow
    <|> TypeOpName <$> parseQualifiedSymbol
    <|> TypeHole <$> parseHole
    <|> TypeWildcard <$> tokUnderscore
    <|> TypeArrowName <$> tokSymbolArrow

parseTypeParens :: Parser (Recovered Type)
parseTypeParens = do
  open <- tokLeftParen
  parseRowParen open
    <|> parseRowTailParen open
    <|> parseKindedVar open
    <|> parseTypeParen open
    <|> parseEmptyRow open

parseTypeNegative :: Parser (Recovered Type)
parseTypeNegative = do
  negative <- tokKeyOperator "-"
  uncurry (TypeInt (Just negative)) <$> parseInt

parseRowParen :: SourceToken -> Parser (Recovered Type)
parseRowParen open = do
  Tuple label separator <- try $ Tuple <$> parseLabel <*> tokDoubleColon
  value <- parseType
  rest <- many (Tuple <$> tokComma <*> parseRowLabel)
  tail <- optional $ Tuple <$> tokPipe <*> parseType
  close <- tokRightParen
  pure $ TypeRow $ Wrapped
    { open
    , value: Row
        { labels: Just $ Separated
            { head: Labeled { label, separator, value }
            , tail: rest
            }
        , tail
        }
    , close
    }

parseRowTailParen :: SourceToken -> Parser (Recovered Type)
parseRowTailParen open = do
  tail <- Tuple <$> tokPipe <*> parseType
  close <- tokRightParen
  pure $ TypeRow $ Wrapped
    { open
    , value: Row { labels: Nothing, tail: Just tail }
    , close
    }

parseEmptyRow :: SourceToken -> Parser (Recovered Type)
parseEmptyRow open = do
  close <- tokRightParen
  pure $ TypeRow $ Wrapped
    { open
    , value: Row { labels: Nothing, tail: Nothing }
    , close
    }

parseKindedVar :: SourceToken -> Parser (Recovered Type)
parseKindedVar open = do
  Tuple var separator <- try $ Tuple <$> parens (TypeVar <$> parseIdent) <*> tokDoubleColon
  kind <- parseType
  close <- tokRightParen
  pure $ TypeParens $ Wrapped
    { open
    , value: TypeKinded (TypeParens var) separator kind
    , close
    }

parseTypeParen :: SourceToken -> Parser (Recovered Type)
parseTypeParen open = do
  value <- parseType
  close <- tokRightParen
  pure $ TypeParens $ Wrapped { open, value, close }

parseRow :: Parser (Recovered Row)
parseRow = defer \_ -> do
  labels <- optional $ separated tokComma parseRowLabel
  tail <- optional $ Tuple <$> tokPipe <*> parseType
  pure $ Row { labels, tail }

parseRowLabel :: Parser (Labeled (Name Label) (Recovered Type))
parseRowLabel = do
  label <- parseLabel
  separator <- tokDoubleColon
  value <- parseType
  pure $ Labeled { label, separator, value }

parseForall :: Parser (Recovered Type)
parseForall = defer \_ ->
  TypeForall
    <$> tokForall
    <*> many1 parseTypeVarBinding
    <*> tokDot
    <*> parseType1

parseTypeVarBinding :: Parser (Recovered TypeVarBinding)
parseTypeVarBinding = defer \_ ->
  parseTypeVarKinded
    <|> TypeVarName <$> parseIdent

parseTypeVarKinded :: Parser (Recovered TypeVarBinding)
parseTypeVarKinded = TypeVarKinded <$> parens do
  label <- parseIdent
  separator <- tokDoubleColon
  value <- parseType
  pure $ Labeled { label, separator, value }

parseExpr :: Parser (Recovered Expr)
parseExpr = defer \_ -> do
  expr <- parseExpr1
  ExprTyped expr <$> tokDoubleColon <*> parseType
    <|> pure expr

parseExpr1 :: Parser (Recovered Expr)
parseExpr1 = defer \_ -> do
  expr <- parseExpr2
  ops <- many (Tuple <$> parseQualifiedOperator <*> parseExpr2)
  pure case NonEmptyArray.fromArray ops of
    Nothing -> expr
    Just os -> ExprOp expr os

parseExpr2 :: Parser (Recovered Expr)
parseExpr2 = defer \_ -> do
  expr <- parseExpr3
  ops <- many (Tuple <$> parseTickExpr <*> parseExpr3)
  pure case NonEmptyArray.fromArray ops of
    Nothing -> expr
    Just os -> ExprInfix expr os

parseTickExpr :: Parser (Wrapped (Recovered Expr))
parseTickExpr = do
  open <- tokTick
  value <- parseTickExpr1
  close <- tokTick
  pure $ Wrapped { open, value, close }

parseTickExpr1 :: Parser (Recovered Expr)
parseTickExpr1 = defer \_ -> do
  expr <- parseExpr3
  ops <- many (Tuple <$> parseQualifiedOperator <*> parseExpr3)
  pure case NonEmptyArray.fromArray ops of
    Nothing -> expr
    Just os -> ExprOp expr os

parseExpr3 :: Parser (Recovered Expr)
parseExpr3 = defer \_ -> do
  ExprNegate <$> tokKeyOperator "-" <*> parseExpr3
    <|> parseExpr4

parseExpr4 :: Parser (Recovered Expr)
parseExpr4 = defer \_ -> do
  expr <- parseExpr5
  args <- many parseExpr5
  pure case NonEmptyArray.fromArray args of
    Nothing -> expr
    Just as -> ExprApp expr as

parseExpr5 :: Parser (Recovered Expr)
parseExpr5 = defer \_ ->
  parseIf
    <|> parseLetIn
    <|> parseLambda
    <|> parseCase
    <|> parseDo
    <|> parseAdo
    <|> parseExpr6

parseIf :: Parser (Recovered Expr)
parseIf = do
  keyword <- tokKeyword "if"
  cond <- parseExpr
  then_ <- tokKeyword "then"
  true_ <- parseExpr
  else_ <- tokKeyword "else"
  false_ <- parseExpr
  pure $ ExprIf { keyword, cond, then: then_, true: true_, else: else_, false: false_ }

parseLetIn :: Parser (Recovered Expr)
parseLetIn = do
  keyword <- tokKeyword "let"
  bindings <- layoutNonEmpty (recoverLetBinding parseLetBinding)
  in_ <- tokKeyword "in"
  body <- parseExpr
  pure $ ExprLet { keyword, bindings, in: in_, body }

parseLambda :: Parser (Recovered Expr)
parseLambda = do
  symbol <- tokBackslash
  binders <- many1 parseBinderAtom
  arrow <- tokRightArrow
  body <- parseExpr
  pure $ ExprLambda { symbol, binders, arrow, body }

parseCase :: Parser (Recovered Expr)
parseCase = do
  keyword <- tokKeyword "case"
  head <- separated tokComma parseExpr
  of_ <- tokKeyword "of"
  branches <- try parseBadSingleCaseBranch <|> parseCaseBranches
  pure $ ExprCase { keyword, head, of: of_, branches }

parseCaseBranches :: Parser (NonEmptyArray (Tuple (Separated (Recovered Binder)) (Recovered Guarded)))
parseCaseBranches = defer \_ ->
  layoutNonEmpty $ Tuple <$> separated tokComma parseBinder1 <*> parseGuarded tokRightArrow

parseBadSingleCaseBranch :: Parser (NonEmptyArray (Tuple (Separated (Recovered Binder)) (Recovered Guarded)))
parseBadSingleCaseBranch = do
  binder <- tokLayoutStart *> parseBinder1
  parseBadSingleCaseWhere binder
    <|> parseBadSingleCaseGuarded binder

parseBadSingleCaseWhere :: Recovered Binder -> Parser (NonEmptyArray (Tuple (Separated (Recovered Binder)) (Recovered Guarded)))
parseBadSingleCaseWhere binder = do
  arrow <- tokRightArrow
  body <- tokLayoutEnd *> parseWhere
  pure $ NonEmptyArray.singleton $ Tuple (Separated { head: binder, tail: [] }) $ Unconditional arrow body

parseBadSingleCaseGuarded :: Recovered Binder -> Parser (NonEmptyArray (Tuple (Separated (Recovered Binder)) (Recovered Guarded)))
parseBadSingleCaseGuarded binder = do
  body <- tokLayoutEnd *> parseGuarded tokRightArrow
  pure $ NonEmptyArray.singleton $ Tuple (Separated { head: binder, tail: [] }) body

parseDo :: Parser (Recovered Expr)
parseDo = do
  keyword <- tokQualifiedKeyword "do"
  statements <- layoutNonEmpty (recoverDoStatement parseDoStatement)
  pure $ ExprDo { keyword, statements }

parseAdo :: Parser (Recovered Expr)
parseAdo = do
  keyword <- tokQualifiedKeyword "ado"
  -- A possibly-empty version of `layoutNonEmpty` to handle empty `ado in`
  statements <- do
    let
      -- `recoverDoStatement` recovers too much if it is immediately
      -- confronted with `TokLayoutEnd`, since that is associated with a
      -- `layoutStack` _of the parent_ as opposed to the stuff we actually
      -- want to recover, which we would correctly guess if we saw a statement
      -- or two inside the block
      valueParser = recoverDoStatement parseDoStatement
      nonEmptyCase =
        Array.cons <$> valueParser <*> many (tokLayoutSep *> valueParser)
    _ <- tokLayoutStart
    -- So we explicitly handle `TokLayoutEnd` ahead of time:
    [] <$ tokLayoutEnd <|> nonEmptyCase <* tokLayoutEnd
  in_ <- tokKeyword "in"
  result <- parseExpr
  pure $ ExprAdo { keyword, statements, in: in_, result }

parseExpr6 :: Parser (Recovered Expr)
parseExpr6 = defer \_ -> do
  expr <- parseExpr7
  parseRecordUpdates expr
    <|> pure expr

parseRecordUpdates :: Recovered Expr -> Parser (Recovered Expr)
parseRecordUpdates expr = do
  open <- try $ tokLeftBrace <* lookAhead (parseLabel *> (tokEquals <|> tokLeftBrace))
  value <- separated tokComma parseRecordUpdate
  close <- tokRightBrace
  pure $ ExprRecordUpdate expr $ Wrapped { open, value, close }

parseRecordUpdate :: Parser (Recovered RecordUpdate)
parseRecordUpdate = do
  label <- parseLabel
  parseRecordUpdateLeaf label
    <|> parseRecordUpdateBranch label

parseRecordUpdateLeaf :: Name Label -> Parser (Recovered RecordUpdate)
parseRecordUpdateLeaf label =
  RecordUpdateLeaf label
    <$> tokEquals
    <*> parseExpr

parseRecordUpdateBranch :: Name Label -> Parser (Recovered RecordUpdate)
parseRecordUpdateBranch label =
  RecordUpdateBranch label
    <$> braces (separated tokComma parseRecordUpdate)

parseExpr7 :: Parser (Recovered Expr)
parseExpr7 = defer \_ -> do
  expr <- parseExprAtom
  parseRecordAccessor expr
    <|> pure expr

parseRecordAccessor :: Recovered Expr -> Parser (Recovered Expr)
parseRecordAccessor expr = do
  dot <- tokDot
  path <- separated tokDot parseLabel
  pure $ ExprRecordAccessor { expr, dot, path }

parseExprAtom :: Parser (Recovered Expr)
parseExprAtom = defer \_ ->
  ExprIdent <$> parseQualifiedIdent
    <|> ExprConstructor <$> parseQualifiedProper
    <|> ExprOpName <$> parseQualifiedSymbol
    <|> ExprSection <$> tokUnderscore
    <|> ExprHole <$> parseHole
    <|> uncurry ExprString <$> parseString
    <|> uncurry ExprChar <$> parseChar
    <|> uncurry ExprBoolean <$> parseBoolean
    <|> uncurry ExprInt <$> parseInt
    <|> uncurry ExprNumber <$> parseNumber
    <|> ExprArray <$> delimited tokLeftSquare tokRightSquare tokComma parseExpr
    <|> ExprRecord <$> delimited tokLeftBrace tokRightBrace tokComma (parseRecordLabeled parseExpr)
    <|> ExprParens <$> parens parseExpr

parseRecordLabeled :: forall a. Parser a -> Parser (RecordLabeled a)
parseRecordLabeled valueParser =
  parseRecordField
    <|> RecordPun <$> parseIdent
  where
  parseRecordField :: Parser (RecordLabeled a)
  parseRecordField =
    uncurry RecordField
      <$> try (Tuple <$> parseLabel <*> tokKeyOperator ":")
      <*> valueParser

parseDoStatement :: Parser (Recovered DoStatement)
parseDoStatement = defer \_ ->
  DoLet <$> tokKeyword "let" <*> layoutNonEmpty (recoverLetBinding parseLetBinding)
    <|> uncurry DoBind <$> try (Tuple <$> parseBinder <*> tokLeftArrow) <*> parseExpr
    <|> DoDiscard <$> parseExpr

parseLetBinding :: Parser (Recovered LetBinding)
parseLetBinding = defer \_ ->
  try parseIdentBinding
    <|> LetBindingPattern <$> parseBinder1 <*> tokEquals <*> parseWhere

parseIdentBinding :: Parser (Recovered LetBinding)
parseIdentBinding = do
  ident <- parseIdent
  parseLetBindingSignature ident
    <|> parseLetBindingName ident

parseLetBindingSignature :: Name Ident -> Parser (Recovered LetBinding)
parseLetBindingSignature label = do
  separator <- tokDoubleColon
  value <- parseType
  pure $ LetBindingSignature $ Labeled { label, separator, value }

parseLetBindingName :: Name Ident -> Parser (Recovered LetBinding)
parseLetBindingName name = do
  binders <- many parseBinderAtom
  guarded <- parseGuarded tokEquals
  pure $ LetBindingName { name, binders, guarded }

parseGuarded :: Parser SourceToken -> Parser (Recovered Guarded)
parseGuarded sepParser =
  Unconditional <$> sepParser <*> parseWhere
    <|> Guarded <$> many1 parseGuardedExpr
  where
  parseGuardedExpr :: Parser (Recovered GuardedExpr)
  parseGuardedExpr = ado
    bar <- tokPipe
    patterns <- separated tokComma parsePatternGuard
    separator <- sepParser
    where_ <- parseWhere
    in GuardedExpr { bar, patterns, separator, where: where_ }

  parsePatternGuard :: Parser (Recovered PatternGuard)
  parsePatternGuard = ado
    binder <- optional (try (Tuple <$> parseBinder <*> tokLeftArrow))
    expr <- parseExpr
    in PatternGuard { binder, expr }

parseWhere :: Parser (Recovered Where)
parseWhere = defer \_ -> do
  expr <- parseExpr
  bindings <- optional (Tuple <$> tokKeyword "where" <*> layoutNonEmpty (recoverLetBinding parseLetBinding))
  pure $ Where { expr, bindings }

parseBinder :: Parser (Recovered Binder)
parseBinder = defer \_ -> do
  binder <- parseBinder1
  BinderTyped binder <$> tokDoubleColon <*> parseType
    <|> pure binder

parseBinder1 :: Parser (Recovered Binder)
parseBinder1 = defer \_ -> do
  binder <- parseBinder2
  ops <- many (Tuple <$> parseQualifiedOperator <*> parseBinder2)
  pure case NonEmptyArray.fromArray ops of
    Nothing -> binder
    Just os -> BinderOp binder os

parseBinder2 :: Parser (Recovered Binder)
parseBinder2 = defer \_ ->
  parseBinderNegative
    <|> parseBinderConstructor
    <|> parseBinderAtom

parseBinderNegative :: Parser (Recovered Binder)
parseBinderNegative = do
  negative <- tokKeyOperator "-"
  uncurry (BinderInt (Just negative)) <$> parseInt
    <|> uncurry (BinderNumber (Just negative)) <$> parseNumber

parseBinderConstructor :: Parser (Recovered Binder)
parseBinderConstructor = defer \_ -> do
  name <- parseQualifiedProper
  apps <- many parseBinderAtom
  pure $ BinderConstructor name apps

parseBinderAtom :: Parser (Recovered Binder)
parseBinderAtom = defer \_ ->
  parseIdentBinder
    <|> flip BinderConstructor [] <$> parseQualifiedProper
    <|> BinderWildcard <$> tokUnderscore
    <|> uncurry BinderString <$> parseString
    <|> uncurry BinderChar <$> parseChar
    <|> uncurry BinderBoolean <$> parseBoolean
    <|> uncurry (BinderInt Nothing) <$> parseInt
    <|> uncurry (BinderNumber Nothing) <$> parseNumber
    <|> BinderArray <$> delimited tokLeftSquare tokRightSquare tokComma parseBinder
    <|> BinderRecord <$> delimited tokLeftBrace tokRightBrace tokComma (parseRecordLabeled parseBinder)
    <|> BinderParens <$> parens parseBinder

parseIdentBinder :: Parser (Recovered Binder)
parseIdentBinder = do
  ident <- parseIdent
  BinderNamed ident <$> tokAt <*> parseBinderAtom
    <|> pure (BinderVar ident)

parseLabel :: Parser (Name Label)
parseLabel = expectMap case _ of
  tok@{ value: TokRawString label } ->
    Just $ Name { token: tok, name: Label label }
  tok@{ value: TokString _ label } ->
    Just $ Name { token: tok, name: Label label }
  tok@{ value: TokLowerName Nothing label } ->
    Just $ Name { token: tok, name: Label label }
  _ -> Nothing

parseIdent :: Parser (Name Ident)
parseIdent = expectMap case _ of
  tok@{ value: TokLowerName Nothing ident } | not $ Set.member ident reservedKeywords ->
    Just $ Name { token: tok, name: Ident ident }
  _ -> Nothing

parseQualifiedIdent :: Parser (QualifiedName Ident)
parseQualifiedIdent = expectMap case _ of
  tok@{ value: TokLowerName mn ident } | not $ Set.member ident reservedKeywords ->
    Just $ QualifiedName { token: tok, "module": mn, name: Ident ident }
  _ -> Nothing

parseProper :: Parser (Name Proper)
parseProper = expectMap case _ of
  tok@{ value: TokUpperName Nothing proper } ->
    Just $ Name { token: tok, name: Proper proper }
  _ -> Nothing

parseQualifiedProper :: Parser (QualifiedName Proper)
parseQualifiedProper = expectMap case _ of
  tok@{ value: TokUpperName mn proper } ->
    Just $ QualifiedName { token: tok, "module": mn, name: Proper proper }
  _ -> Nothing

parseQualifiedIdentOrProper :: Parser (QualifiedName (Either Ident Proper))
parseQualifiedIdentOrProper = expectMap case _ of
  tok@{ value: TokLowerName mn ident } ->
    Just $ QualifiedName { token: tok, "module": mn, name: Left $ Ident ident }
  tok@{ value: TokUpperName mn proper } ->
    Just $ QualifiedName { token: tok, "module": mn, name: Right $ Proper proper }
  _ -> Nothing

parseModuleName :: Parser (Name ModuleName)
parseModuleName = expectMap case _ of
  tok@{ value: TokUpperName (Just (ModuleName mn)) proper } ->
    Just $ Name { token: tok, name: ModuleName $ mn <> "." <> proper }
  tok@{ value: TokUpperName Nothing proper } ->
    Just $ Name { token: tok, name: ModuleName proper }
  _ -> Nothing

parseOperator :: Parser (Name Operator)
parseOperator = expectMap case _ of
  tok@{ value: TokOperator Nothing operator } ->
    Just $ Name { token: tok, name: Operator operator }
  _ -> Nothing

parseQualifiedOperator :: Parser (QualifiedName Operator)
parseQualifiedOperator = expectMap case _ of
  tok@{ value: TokOperator mn operator } ->
    Just $ QualifiedName { token: tok, "module": mn, name: Operator operator }
  _ -> Nothing

parseSymbol :: Parser (Name Operator)
parseSymbol = expectMap case _ of
  tok@{ value: TokSymbolName Nothing operator } ->
    Just $ Name { token: tok, name: Operator operator }
  _ -> Nothing

parseQualifiedSymbol :: Parser (QualifiedName Operator)
parseQualifiedSymbol = expectMap case _ of
  tok@{ value: TokSymbolName mn operator } ->
    Just $ QualifiedName { token: tok, "module": mn, name: Operator operator }
  _ -> Nothing

parseHole :: Parser (Name Ident)
parseHole = expectMap case _ of
  tok@{ value: TokHole hole } ->
    Just $ Name { token: tok, name: Ident hole }
  _ -> Nothing

parseString :: Parser (Tuple SourceToken String)
parseString = expectMap case _ of
  tok@{ value: TokString _ str } ->
    Just $ Tuple tok str
  tok@{ value: TokRawString str } ->
    Just $ Tuple tok str
  _ -> Nothing

parseChar :: Parser (Tuple SourceToken Char)
parseChar = expectMap case _ of
  tok@{ value: TokChar _ ch } ->
    Just $ Tuple tok ch
  _ -> Nothing

parseInt :: Parser (Tuple SourceToken IntValue)
parseInt = expectMap case _ of
  tok@{ value: TokInt _ int } ->
    Just $ Tuple tok int
  _ -> Nothing

parseSmallInt :: Parser (Tuple SourceToken Int)
parseSmallInt = take case _ of
  tok@{ value: TokInt _ (SmallInt val) } ->
    Right $ Tuple tok val
  { value: TokInt raw _ } ->
    Left $ LexIntOutOfRange raw
  tok ->
    Left $ UnexpectedToken tok.value

parseNumber :: Parser (Tuple SourceToken Number)
parseNumber = expectMap case _ of
  tok@{ value: TokNumber _ number } ->
    Just $ Tuple tok number
  _ -> Nothing

parseBoolean :: Parser (Tuple SourceToken Boolean)
parseBoolean = expectMap case _ of
  tok@{ value: TokLowerName Nothing "true" } ->
    Just $ Tuple tok true
  tok@{ value: TokLowerName Nothing "false" } ->
    Just $ Tuple tok false
  _ -> Nothing

many1 :: forall a. Parser a -> Parser (NonEmptyArray a)
many1 parser =
  NonEmptyArray.cons'
    <$> parser
    <*> many parser

tokDoubleColon :: Parser SourceToken
tokDoubleColon = expect case _ of
  TokDoubleColon _ -> true
  _ -> false

tokForall :: Parser SourceToken
tokForall = expect case _ of
  TokForall _ -> true
  _ -> false

tokRightFatArrow :: Parser SourceToken
tokRightFatArrow = expect case _ of
  TokRightFatArrow _ -> true
  _ -> false

tokLeftFatArrow :: Parser SourceToken
tokLeftFatArrow = expect case _ of
  TokOperator Nothing name -> name == "<=" || name == "⇐"
  _ -> false

tokRightArrow :: Parser SourceToken
tokRightArrow = expect case _ of
  TokRightArrow _ -> true
  _ -> false

tokLeftArrow :: Parser SourceToken
tokLeftArrow = expect case _ of
  TokLeftArrow _ -> true
  _ -> false

tokSymbolArrow :: Parser SourceToken
tokSymbolArrow = expect case _ of
  TokSymbolArrow _ -> true
  _ -> false

tokKeyword :: String -> Parser SourceToken
tokKeyword kw = expect case _ of
  TokLowerName Nothing name -> kw == name
  _ -> false

tokQualifiedKeyword :: String -> Parser SourceToken
tokQualifiedKeyword kw = expect case _ of
  TokLowerName _ name -> kw == name
  _ -> false

tokKeyOperator :: String -> Parser SourceToken
tokKeyOperator sym = expect case _ of
  TokOperator Nothing name -> sym == name
  _ -> false

tokKeySymbol :: String -> Parser SourceToken
tokKeySymbol sym = expect case _ of
  TokSymbolName Nothing name -> sym == name
  _ -> false

tokLayoutStart :: Parser SourceToken
tokLayoutStart = expect case _ of
  TokLayoutStart _ -> true
  _ -> false

tokLayoutEnd :: Parser SourceToken
tokLayoutEnd = expect case _ of
  TokLayoutEnd _ -> true
  _ -> false

tokLayoutSep :: Parser SourceToken
tokLayoutSep = expect case _ of
  TokLayoutSep _ -> true
  _ -> false

tokLeftParen :: Parser SourceToken
tokLeftParen = expect case _ of
  TokLeftParen -> true
  _ -> false

tokRightParen :: Parser SourceToken
tokRightParen = expect case _ of
  TokRightParen -> true
  _ -> false

tokLeftBrace :: Parser SourceToken
tokLeftBrace = expect case _ of
  TokLeftBrace -> true
  _ -> false

tokRightBrace :: Parser SourceToken
tokRightBrace = expect case _ of
  TokRightBrace -> true
  _ -> false

tokLeftSquare :: Parser SourceToken
tokLeftSquare = expect case _ of
  TokLeftSquare -> true
  _ -> false

tokRightSquare :: Parser SourceToken
tokRightSquare = expect case _ of
  TokRightSquare -> true
  _ -> false

tokEquals :: Parser SourceToken
tokEquals = expect case _ of
  TokEquals -> true
  _ -> false

tokPipe :: Parser SourceToken
tokPipe = expect case _ of
  TokPipe -> true
  _ -> false

tokTick :: Parser SourceToken
tokTick = expect case _ of
  TokTick -> true
  _ -> false

tokDot :: Parser SourceToken
tokDot = expect case _ of
  TokDot -> true
  _ -> false

tokComma :: Parser SourceToken
tokComma = expect case _ of
  TokComma -> true
  _ -> false

tokUnderscore :: Parser SourceToken
tokUnderscore = expect case _ of
  TokUnderscore -> true
  _ -> false

tokBackslash :: Parser SourceToken
tokBackslash = expect case _ of
  TokBackslash -> true
  _ -> false

tokAt :: Parser SourceToken
tokAt = expect case _ of
  TokAt -> true
  _ -> false

reservedKeywords :: Set String
reservedKeywords = Set.fromFoldable
  [ "ado"
  , "case"
  , "class"
  , "data"
  , "derive"
  , "do"
  , "else"
  , "false"
  , "foreign"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "then"
  , "true"
  , "type"
  , "where"
  ]

recoverIndent :: forall a. (RecoveredError -> a) -> Parser a -> Parser a
recoverIndent mkNode = recover \{ position, error } stream -> do
  let
    Tuple tokens newStream = recoverTokensWhile
      ( \tok indent -> case tok.value of
          TokLayoutEnd col -> col > indent
          TokLayoutSep col -> col > indent
          _ -> true
      )
      stream
  if Array.null tokens then
    Nothing
  else
    Just (Tuple (mkNode (RecoveredError { position, error, tokens })) newStream)

recoverTokensWhile :: (SourceToken -> Int -> Boolean) -> TokenStream -> Tuple (Array SourceToken) TokenStream
recoverTokensWhile p initStream = go [] initStream
  where
  indent :: Int
  indent = maybe 0 _.column $ currentIndent $ layoutStack initStream

  go :: Array SourceToken -> TokenStream -> Tuple (Array SourceToken) TokenStream
  go acc stream = case TokenStream.step stream of
    TokenError _ _ _ _ ->
      Tuple acc stream
    TokenEOF _ _ ->
      Tuple acc stream
    TokenCons tok _ nextStream _ ->
      if p tok indent then
        go (Array.snoc acc tok) nextStream
      else
        Tuple acc stream

recoverDecl :: RecoveryStrategy Declaration
recoverDecl = recoverIndent DeclError

recoverLetBinding :: RecoveryStrategy LetBinding
recoverLetBinding = recoverIndent LetBindingError

recoverDoStatement :: RecoveryStrategy DoStatement
recoverDoStatement = recoverIndent DoError
