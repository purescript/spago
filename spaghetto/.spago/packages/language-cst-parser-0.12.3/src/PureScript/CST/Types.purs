module PureScript.CST.Types where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Prim hiding (Row, Type)

newtype ModuleName = ModuleName String

derive newtype instance eqModuleName :: Eq ModuleName
derive newtype instance ordModuleName :: Ord ModuleName
derive instance newtypeModuleName :: Newtype ModuleName _

type SourcePos =
  { line :: Int
  , column :: Int
  }

type SourceRange =
  { start :: SourcePos
  , end :: SourcePos
  }

data Comment l
  = Comment String
  | Space Int
  | Line l Int

data LineFeed
  = LF
  | CRLF

data SourceStyle
  = ASCII
  | Unicode

derive instance eqSourceStyle :: Eq SourceStyle

data IntValue
  = SmallInt Int
  | BigInt String
  | BigHex String

derive instance eqIntValue :: Eq IntValue

data Token
  = TokLeftParen
  | TokRightParen
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  | TokLeftArrow SourceStyle
  | TokRightArrow SourceStyle
  | TokRightFatArrow SourceStyle
  | TokDoubleColon SourceStyle
  | TokForall SourceStyle
  | TokEquals
  | TokPipe
  | TokTick
  | TokDot
  | TokComma
  | TokUnderscore
  | TokBackslash
  | TokAt
  | TokLowerName (Maybe ModuleName) String
  | TokUpperName (Maybe ModuleName) String
  | TokOperator (Maybe ModuleName) String
  | TokSymbolName (Maybe ModuleName) String
  | TokSymbolArrow SourceStyle
  | TokHole String
  | TokChar String Char
  | TokString String String
  | TokRawString String
  | TokInt String IntValue
  | TokNumber String Number
  | TokLayoutStart Int
  | TokLayoutSep Int
  | TokLayoutEnd Int

derive instance eqToken :: Eq Token

type SourceToken =
  { range :: SourceRange
  , leadingComments :: Array (Comment LineFeed)
  , trailingComments :: Array (Comment Void)
  , value :: Token
  }

newtype Ident = Ident String

derive newtype instance eqIdent :: Eq Ident
derive newtype instance ordIdent :: Ord Ident
derive instance newtypeIdent :: Newtype Ident _

newtype Proper = Proper String

derive newtype instance eqProper :: Eq Proper
derive newtype instance ordProper :: Ord Proper
derive instance newtypeProper :: Newtype Proper _

newtype Label = Label String

derive newtype instance eqLabel :: Eq Label
derive newtype instance ordLabel :: Ord Label
derive instance newtypeLabel :: Newtype Label _

newtype Operator = Operator String

derive newtype instance eqOperator :: Eq Operator
derive newtype instance ordOperator :: Ord Operator
derive instance newtypeOperator :: Newtype Operator _

newtype Name a = Name
  { token :: SourceToken
  , name :: a
  }

derive instance newtypeName :: Newtype (Name a) _

newtype QualifiedName a = QualifiedName
  { token :: SourceToken
  , module :: Maybe ModuleName
  , name :: a
  }

derive instance newtypeQualifiedName :: Newtype (QualifiedName a) _

newtype Wrapped a = Wrapped
  { open :: SourceToken
  , value :: a
  , close :: SourceToken
  }

derive instance newtypeWrapped :: Newtype (Wrapped a) _

newtype Separated a = Separated
  { head :: a
  , tail :: Array (Tuple SourceToken a)
  }

derive instance newtypeSeparated :: Newtype (Separated a) _

newtype Labeled a b = Labeled
  { label :: a
  , separator :: SourceToken
  , value :: b
  }

derive instance newtypeLabeled :: Newtype (Labeled a b) _

type Delimited a = Wrapped (Maybe (Separated a))
type DelimitedNonEmpty a = Wrapped (Separated a)

data OneOrDelimited a
  = One a
  | Many (DelimitedNonEmpty a)

data Type e
  = TypeVar (Name Ident)
  | TypeConstructor (QualifiedName Proper)
  | TypeWildcard SourceToken
  | TypeHole (Name Ident)
  | TypeString SourceToken String
  | TypeInt (Maybe SourceToken) SourceToken IntValue
  | TypeRow (Wrapped (Row e))
  | TypeRecord (Wrapped (Row e))
  | TypeForall SourceToken (NonEmptyArray (TypeVarBinding e)) SourceToken (Type e)
  | TypeKinded (Type e) SourceToken (Type e)
  | TypeApp (Type e) (NonEmptyArray (Type e))
  | TypeOp (Type e) (NonEmptyArray (Tuple (QualifiedName Operator) (Type e)))
  | TypeOpName (QualifiedName Operator)
  | TypeArrow (Type e) SourceToken (Type e)
  | TypeArrowName SourceToken
  | TypeConstrained (Type e) SourceToken (Type e)
  | TypeParens (Wrapped (Type e))
  | TypeError e

data TypeVarBinding e
  = TypeVarKinded (Wrapped (Labeled (Name Ident) (Type e)))
  | TypeVarName (Name Ident)

newtype Row e = Row
  { labels :: Maybe (Separated (Labeled (Name Label) (Type e)))
  , tail :: Maybe (Tuple SourceToken (Type e))
  }

derive instance newtypeRow :: Newtype (Row e) _

newtype Module e = Module
  { header :: ModuleHeader e
  , body :: ModuleBody e
  }

derive instance newtypeModule :: Newtype (Module e) _

newtype ModuleHeader e = ModuleHeader
  { keyword :: SourceToken
  , name :: Name ModuleName
  , exports :: Maybe (DelimitedNonEmpty (Export e))
  , where :: SourceToken
  , imports :: Array (ImportDecl e)
  }

derive instance newtypeModuleHeader :: Newtype (ModuleHeader e) _

newtype ModuleBody e = ModuleBody
  { decls :: Array (Declaration e)
  , trailingComments :: Array (Comment LineFeed)
  , end :: SourcePos
  }

derive instance newtypeModuleBody :: Newtype (ModuleBody e) _

data Export e
  = ExportValue (Name Ident)
  | ExportOp (Name Operator)
  | ExportType (Name Proper) (Maybe DataMembers)
  | ExportTypeOp SourceToken (Name Operator)
  | ExportClass SourceToken (Name Proper)
  | ExportModule SourceToken (Name ModuleName)
  | ExportError e

data DataMembers
  = DataAll SourceToken
  | DataEnumerated (Delimited (Name Proper))

data Declaration e
  = DeclData (DataHead e) (Maybe (Tuple SourceToken (Separated (DataCtor e))))
  | DeclType (DataHead e) SourceToken (Type e)
  | DeclNewtype (DataHead e) SourceToken (Name Proper) (Type e)
  | DeclClass (ClassHead e) (Maybe (Tuple SourceToken (NonEmptyArray (Labeled (Name Ident) (Type e)))))
  | DeclInstanceChain (Separated (Instance e))
  | DeclDerive SourceToken (Maybe SourceToken) (InstanceHead e)
  | DeclKindSignature SourceToken (Labeled (Name Proper) (Type e))
  | DeclSignature (Labeled (Name Ident) (Type e))
  | DeclValue (ValueBindingFields e)
  | DeclFixity FixityFields
  | DeclForeign SourceToken SourceToken (Foreign e)
  | DeclRole SourceToken SourceToken (Name Proper) (NonEmptyArray (Tuple SourceToken Role))
  | DeclError e

newtype Instance e = Instance
  { head :: InstanceHead e
  , body :: Maybe (Tuple SourceToken (NonEmptyArray (InstanceBinding e)))
  }

derive instance newtypeInstance :: Newtype (Instance e) _

data InstanceBinding e
  = InstanceBindingSignature (Labeled (Name Ident) (Type e))
  | InstanceBindingName (ValueBindingFields e)

newtype ImportDecl e = ImportDecl
  { keyword :: SourceToken
  , module :: Name ModuleName
  , names :: Maybe (Tuple (Maybe SourceToken) (DelimitedNonEmpty (Import e)))
  , qualified :: Maybe (Tuple SourceToken (Name ModuleName))
  }

derive instance newtypeImportDecl :: Newtype (ImportDecl e) _

data Import e
  = ImportValue (Name Ident)
  | ImportOp (Name Operator)
  | ImportType (Name Proper) (Maybe DataMembers)
  | ImportTypeOp SourceToken (Name Operator)
  | ImportClass SourceToken (Name Proper)
  | ImportError e

type DataHead e =
  { keyword :: SourceToken
  , name :: Name Proper
  , vars :: Array (TypeVarBinding e)
  }

newtype DataCtor e = DataCtor
  { name :: Name Proper
  , fields :: Array (Type e)
  }

derive instance newtypeDataCtor :: Newtype (DataCtor e) _

type ClassHead e =
  { keyword :: SourceToken
  , super :: Maybe (Tuple (OneOrDelimited (Type e)) SourceToken)
  , name :: Name Proper
  , vars :: Array (TypeVarBinding e)
  , fundeps :: Maybe (Tuple SourceToken (Separated ClassFundep))
  }

data ClassFundep
  = FundepDetermined SourceToken (NonEmptyArray (Name Ident))
  | FundepDetermines (NonEmptyArray (Name Ident)) SourceToken (NonEmptyArray (Name Ident))

type InstanceHead e =
  { keyword :: SourceToken
  , name :: Maybe (Tuple (Name Ident) SourceToken)
  , constraints :: Maybe (Tuple (OneOrDelimited (Type e)) SourceToken)
  , className :: QualifiedName Proper
  , types :: Array (Type e)
  }

data Fixity
  = Infix
  | Infixl
  | Infixr

data FixityOp
  = FixityValue (QualifiedName (Either Ident Proper)) SourceToken (Name Operator)
  | FixityType SourceToken (QualifiedName Proper) SourceToken (Name Operator)

type FixityFields =
  { keyword :: Tuple SourceToken Fixity
  , prec :: Tuple SourceToken Int
  , operator :: FixityOp
  }

type ValueBindingFields e =
  { name :: Name Ident
  , binders :: Array (Binder e)
  , guarded :: Guarded e
  }

data Guarded e
  = Unconditional SourceToken (Where e)
  | Guarded (NonEmptyArray (GuardedExpr e))

newtype GuardedExpr e = GuardedExpr
  { bar :: SourceToken
  , patterns :: Separated (PatternGuard e)
  , separator :: SourceToken
  , where :: Where e
  }

derive instance newtypeGuardedExpr :: Newtype (GuardedExpr e) _

newtype PatternGuard e = PatternGuard
  { binder :: Maybe (Tuple (Binder e) SourceToken)
  , expr :: Expr e
  }

derive instance newtypePatternGuard :: Newtype (PatternGuard e) _

data Foreign e
  = ForeignValue (Labeled (Name Ident) (Type e))
  | ForeignData SourceToken (Labeled (Name Proper) (Type e))
  | ForeignKind SourceToken (Name Proper)

data Role
  = Nominal
  | Representational
  | Phantom

data Expr e
  = ExprHole (Name Ident)
  | ExprSection SourceToken
  | ExprIdent (QualifiedName Ident)
  | ExprConstructor (QualifiedName Proper)
  | ExprBoolean SourceToken Boolean
  | ExprChar SourceToken Char
  | ExprString SourceToken String
  | ExprInt SourceToken IntValue
  | ExprNumber SourceToken Number
  | ExprArray (Delimited (Expr e))
  | ExprRecord (Delimited (RecordLabeled (Expr e)))
  | ExprParens (Wrapped (Expr e))
  | ExprTyped (Expr e) SourceToken (Type e)
  | ExprInfix (Expr e) (NonEmptyArray (Tuple (Wrapped (Expr e)) (Expr e)))
  | ExprOp (Expr e) (NonEmptyArray (Tuple (QualifiedName Operator) (Expr e)))
  | ExprOpName (QualifiedName Operator)
  | ExprNegate SourceToken (Expr e)
  | ExprRecordAccessor (RecordAccessor e)
  | ExprRecordUpdate (Expr e) (DelimitedNonEmpty (RecordUpdate e))
  | ExprApp (Expr e) (NonEmptyArray (Expr e))
  | ExprLambda (Lambda e)
  | ExprIf (IfThenElse e)
  | ExprCase (CaseOf e)
  | ExprLet (LetIn e)
  | ExprDo (DoBlock e)
  | ExprAdo (AdoBlock e)
  | ExprError e

data RecordLabeled a
  = RecordPun (Name Ident)
  | RecordField (Name Label) SourceToken a

data RecordUpdate e
  = RecordUpdateLeaf (Name Label) SourceToken (Expr e)
  | RecordUpdateBranch (Name Label) (DelimitedNonEmpty (RecordUpdate e))

type RecordAccessor e =
  { expr :: Expr e
  , dot :: SourceToken
  , path :: Separated (Name Label)
  }

type Lambda e =
  { symbol :: SourceToken
  , binders :: NonEmptyArray (Binder e)
  , arrow :: SourceToken
  , body :: Expr e
  }

type IfThenElse e =
  { keyword :: SourceToken
  , cond :: Expr e
  , then :: SourceToken
  , true :: Expr e
  , else :: SourceToken
  , false :: Expr e
  }

type CaseOf e =
  { keyword :: SourceToken
  , head :: Separated (Expr e)
  , of :: SourceToken
  , branches :: NonEmptyArray (Tuple (Separated (Binder e)) (Guarded e))
  }

type LetIn e =
  { keyword :: SourceToken
  , bindings :: NonEmptyArray (LetBinding e)
  , in :: SourceToken
  , body :: Expr e
  }

newtype Where e = Where
  { expr :: Expr e
  , bindings :: Maybe (Tuple SourceToken (NonEmptyArray (LetBinding e)))
  }

derive instance newtypeWhere :: Newtype (Where e) _

data LetBinding e
  = LetBindingSignature (Labeled (Name Ident) (Type e))
  | LetBindingName (ValueBindingFields e)
  | LetBindingPattern (Binder e) SourceToken (Where e)
  | LetBindingError e

type DoBlock e =
  { keyword :: SourceToken
  , statements :: NonEmptyArray (DoStatement e)
  }

data DoStatement e
  = DoLet SourceToken (NonEmptyArray (LetBinding e))
  | DoDiscard (Expr e)
  | DoBind (Binder e) SourceToken (Expr e)
  | DoError e

type AdoBlock e =
  { keyword :: SourceToken
  , statements :: Array (DoStatement e)
  , in :: SourceToken
  , result :: Expr e
  }

data Binder e
  = BinderWildcard SourceToken
  | BinderVar (Name Ident)
  | BinderNamed (Name Ident) SourceToken (Binder e)
  | BinderConstructor (QualifiedName Proper) (Array (Binder e))
  | BinderBoolean SourceToken Boolean
  | BinderChar SourceToken Char
  | BinderString SourceToken String
  | BinderInt (Maybe SourceToken) SourceToken IntValue
  | BinderNumber (Maybe SourceToken) SourceToken Number
  | BinderArray (Delimited (Binder e))
  | BinderRecord (Delimited (RecordLabeled (Binder e)))
  | BinderParens (Wrapped (Binder e))
  | BinderTyped (Binder e) SourceToken (Type e)
  | BinderOp (Binder e) (NonEmptyArray (Tuple (QualifiedName Operator) (Binder e)))
  | BinderError e
