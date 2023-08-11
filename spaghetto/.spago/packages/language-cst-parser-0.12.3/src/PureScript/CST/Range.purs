module PureScript.CST.Range
  ( class RangeOf
  , rangeOf
  , class TokensOf
  , tokensOf
  ) where

import Prelude

import Control.Lazy (defer)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Prim hiding (Row, Type)
import PureScript.CST.Errors (RecoveredError(..))
import PureScript.CST.Range.TokenList (TokenList, cons, singleton)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types (Binder(..), ClassFundep(..), DataCtor(..), DataMembers(..), Declaration(..), DoStatement(..), Export(..), Expr(..), FixityOp(..), Foreign(..), Guarded(..), GuardedExpr(..), Import(..), ImportDecl(..), Instance(..), InstanceBinding(..), Labeled(..), LetBinding(..), Module(..), ModuleBody(..), ModuleHeader(..), Name(..), OneOrDelimited(..), PatternGuard(..), QualifiedName(..), RecordLabeled(..), RecordUpdate(..), Row(..), Separated(..), SourceRange, Type(..), TypeVarBinding(..), Where(..), Wrapped(..))

class RangeOf a where
  rangeOf :: a -> SourceRange

class TokensOf a where
  tokensOf :: a -> TokenList

instance tokensOfTuple :: (TokensOf a, TokensOf b) => TokensOf (Tuple a b) where
  tokensOf (Tuple a b) = tokensOf a <> tokensOf b

instance tokensOfMaybe :: TokensOf a => TokensOf (Maybe a) where
  tokensOf = foldMap tokensOf

instance tokensOfArray :: TokensOf a => TokensOf (Array a) where
  tokensOf = foldMap (\a -> defer \_ -> tokensOf a)

instance tokensOfNonEmptyArray :: TokensOf a => TokensOf (NonEmptyArray a) where
  tokensOf = foldMap (\a -> defer \_ -> tokensOf a)

instance rangeOfVoid :: RangeOf Void where
  rangeOf = absurd

instance tokensOfVoid :: TokensOf Void where
  tokensOf = absurd

instance rangeOfRecoveredError :: RangeOf RecoveredError where
  rangeOf (RecoveredError { position, tokens }) =
    case NonEmptyArray.fromArray tokens of
      Just toks ->
        { start: (NonEmptyArray.head toks).range.start
        , end: (NonEmptyArray.last toks).range.end
        }
      Nothing ->
        { start: position
        , end: position
        }

instance tokensOfRecoveredError :: TokensOf RecoveredError where
  tokensOf (RecoveredError { tokens }) = TokenList.fromArray tokens

instance rangeOfModule :: RangeOf (Module e) where
  rangeOf (Module { header: ModuleHeader header, body: ModuleBody body }) =
    { start: header.keyword.range.start
    , end: body.end
    }

instance tokensOfModule :: TokensOf e => TokensOf (Module e) where
  tokensOf (Module { header: ModuleHeader header, body: ModuleBody body }) =
    cons header.keyword
      $ tokensOf header.name
          <> defer (\_ -> foldMap tokensOf header.exports)
          <> singleton header.where
          <> defer (\_ -> foldMap tokensOf header.imports)
          <> defer (\_ -> foldMap tokensOf body.decls)

instance rangeOfName :: RangeOf (Name a) where
  rangeOf (Name { token }) = token.range

instance tokensOfName :: TokensOf (Name a) where
  tokensOf (Name { token }) = singleton token

instance rangeOfQualifiedName :: RangeOf (QualifiedName a) where
  rangeOf (QualifiedName { token }) = token.range

instance tokensOfQualifiedName :: TokensOf (QualifiedName a) where
  tokensOf (QualifiedName { token }) = singleton token

instance rangeOfWrapped :: RangeOf (Wrapped a) where
  rangeOf (Wrapped { open, close }) =
    { start: open.range.start
    , end: close.range.end
    }

instance tokensOfWrapped :: TokensOf a => TokensOf (Wrapped a) where
  tokensOf (Wrapped { open, value, close }) =
    TokenList.wrap open (defer \_ -> tokensOf value) close

instance rangeOfSeparated :: RangeOf a => RangeOf (Separated a) where
  rangeOf (Separated { head, tail }) =
    case Array.last tail of
      Just (Tuple _ last) ->
        { start: (rangeOf head).start
        , end: (rangeOf last).end
        }
      Nothing ->
        rangeOf head

instance tokensOfSeparated :: TokensOf a => TokensOf (Separated a) where
  tokensOf (Separated { head, tail }) =
    tokensOf head
      <> defer \_ -> foldMap (\(Tuple a b) -> cons a $ defer (\_ -> tokensOf b)) tail

instance rangeOfLabeled :: (RangeOf a, RangeOf b) => RangeOf (Labeled a b) where
  rangeOf (Labeled { label, value }) =
    { start: (rangeOf label).start
    , end: (rangeOf value).end
    }

instance tokensOfLabeled :: (TokensOf a, TokensOf b) => TokensOf (Labeled a b) where
  tokensOf (Labeled { label, separator, value }) =
    tokensOf label <> singleton separator <> tokensOf value

instance rangeOfOneOrDelimited :: RangeOf a => RangeOf (OneOrDelimited a) where
  rangeOf = case _ of
    One a -> rangeOf a
    Many as -> rangeOf as

instance tokensOfOneOrDelimited :: TokensOf a => TokensOf (OneOrDelimited a) where
  tokensOf = case _ of
    One a -> tokensOf a
    Many as -> tokensOf as

instance rangeOfType :: RangeOf e => RangeOf (Type e) where
  rangeOf = case _ of
    TypeVar n ->
      rangeOf n
    TypeConstructor n ->
      rangeOf n
    TypeWildcard t ->
      t.range
    TypeHole n ->
      rangeOf n
    TypeString t _ ->
      t.range
    TypeInt neg t _ ->
      case neg of
        Nothing ->
          t.range
        Just n ->
          { start: n.range.start
          , end: t.range.end
          }
    TypeRow w ->
      rangeOf w
    TypeRecord w ->
      rangeOf w
    TypeForall t _ _ ty ->
      { start: t.range.start
      , end: (rangeOf ty).end
      }
    TypeKinded ty1 _ ty2 ->
      { start: (rangeOf ty1).start
      , end: (rangeOf ty2).end
      }
    TypeApp ty tys ->
      { start: (rangeOf ty).start
      , end: (rangeOf (NonEmptyArray.last tys)).end
      }
    TypeOp ty ops ->
      { start: (rangeOf ty).start
      , end: (rangeOf (snd (NonEmptyArray.last ops))).end
      }
    TypeOpName n ->
      rangeOf n
    TypeArrow ty1 _ ty2 ->
      { start: (rangeOf ty1).start
      , end: (rangeOf ty2).end
      }
    TypeArrowName t ->
      t.range
    TypeConstrained ty1 _ ty2 ->
      { start: (rangeOf ty1).start
      , end: (rangeOf ty2).end
      }
    TypeParens w ->
      rangeOf w
    TypeError e ->
      rangeOf e

instance tokensOfType :: TokensOf e => TokensOf (Type e) where
  tokensOf = case _ of
    TypeVar n ->
      tokensOf n
    TypeConstructor n ->
      tokensOf n
    TypeWildcard t ->
      singleton t
    TypeHole n ->
      tokensOf n
    TypeString t _ ->
      singleton t
    TypeInt neg t _ ->
      foldMap singleton neg <> singleton t
    TypeRow w ->
      tokensOf w
    TypeRecord w ->
      tokensOf w
    TypeForall t vars dot ty ->
      cons t $ defer \_ ->
        tokensOf vars
          <> singleton dot
          <> tokensOf ty
    TypeKinded ty1 t ty2 ->
      tokensOf ty1
        <> defer \_ -> singleton t <> tokensOf ty2
    TypeApp ty tys ->
      tokensOf ty
        <> defer \_ -> tokensOf tys
    TypeOp ty ops ->
      tokensOf ty
        <> defer \_ -> foldMap (\(Tuple op arg) -> tokensOf op <> tokensOf arg) ops
    TypeOpName n ->
      tokensOf n
    TypeArrow ty1 t ty2 ->
      tokensOf ty1
        <> defer \_ -> singleton t <> tokensOf ty2
    TypeArrowName t ->
      singleton t
    TypeConstrained ty1 t ty2 ->
      tokensOf ty1
        <> defer \_ -> singleton t <> tokensOf ty2
    TypeParens w ->
      tokensOf w
    TypeError e ->
      tokensOf e

instance tokensOfRow :: TokensOf e => TokensOf (Row e) where
  tokensOf (Row { labels, tail }) =
    foldMap tokensOf labels
      <> foldMap (\(Tuple t ty) -> cons t $ tokensOf ty) tail

instance rangeOfTypeVarBinding :: RangeOf (TypeVarBinding e) where
  rangeOf = case _ of
    TypeVarKinded w ->
      rangeOf w
    TypeVarName n ->
      rangeOf n

instance tokensOfTypeVarBinding :: TokensOf e => TokensOf (TypeVarBinding e) where
  tokensOf = case _ of
    TypeVarKinded w ->
      tokensOf w
    TypeVarName n ->
      tokensOf n

instance rangeOfExport :: RangeOf e => RangeOf (Export e) where
  rangeOf = case _ of
    ExportValue n ->
      rangeOf n
    ExportOp n ->
      rangeOf n
    ExportType n dms ->
      case dms of
        Nothing ->
          rangeOf n
        Just dms' ->
          { start: (rangeOf n).start
          , end: (rangeOf dms').end
          }
    ExportTypeOp t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }
    ExportClass t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }
    ExportModule t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }
    ExportError e ->
      rangeOf e

instance tokensOfExport :: TokensOf e => TokensOf (Export e) where
  tokensOf = case _ of
    ExportValue n ->
      tokensOf n
    ExportOp n ->
      tokensOf n
    ExportType n dms ->
      tokensOf n <> foldMap tokensOf dms
    ExportTypeOp t n ->
      cons t $ tokensOf n
    ExportClass t n ->
      cons t $ tokensOf n
    ExportModule t n ->
      cons t $ tokensOf n
    ExportError e ->
      tokensOf e

instance rangeOfDataMembers :: RangeOf DataMembers where
  rangeOf = case _ of
    DataAll t ->
      t.range
    DataEnumerated w ->
      rangeOf w

instance tokensOfDataMembers :: TokensOf DataMembers where
  tokensOf = case _ of
    DataAll t ->
      singleton t
    DataEnumerated w ->
      tokensOf w

instance rangeOfImportDecl :: RangeOf (ImportDecl e) where
  rangeOf (ImportDecl { keyword, "module": mod, names, qualified }) = do
    let
      { end } = case qualified of
        Nothing ->
          case names of
            Nothing ->
              rangeOf mod
            Just (Tuple _ imports) ->
              rangeOf imports
        Just (Tuple _ n) ->
          rangeOf n
    { start: keyword.range.start
    , end
    }

instance tokensOfImportDecl :: TokensOf e => TokensOf (ImportDecl e) where
  tokensOf (ImportDecl { keyword, "module": mod, names, qualified }) =
    cons keyword $ defer \_ ->
      tokensOf mod
        <> foldMap (\(Tuple hiding imports) -> foldMap singleton hiding <> defer (\_ -> tokensOf imports)) names
        <> foldMap (\(Tuple as mn) -> singleton as <> tokensOf mn) qualified

instance rangeOfImport :: RangeOf e => RangeOf (Import e) where
  rangeOf = case _ of
    ImportValue n ->
      rangeOf n
    ImportOp n ->
      rangeOf n
    ImportType n dms ->
      case dms of
        Nothing ->
          rangeOf n
        Just dms' ->
          { start: (rangeOf n).start
          , end: (rangeOf dms').end
          }
    ImportTypeOp t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }
    ImportClass t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }
    ImportError e ->
      rangeOf e

instance tokensOfImport :: TokensOf e => TokensOf (Import e) where
  tokensOf = case _ of
    ImportValue n ->
      tokensOf n
    ImportOp n ->
      tokensOf n
    ImportType n dms ->
      tokensOf n <> foldMap tokensOf dms
    ImportTypeOp t n ->
      cons t $ tokensOf n
    ImportClass t n ->
      cons t $ tokensOf n
    ImportError e ->
      tokensOf e

instance rangeOfDataCtor :: RangeOf e => RangeOf (DataCtor e) where
  rangeOf (DataCtor { name, fields }) = do
    let
      { end } = case Array.last fields of
        Nothing ->
          rangeOf name
        Just ty ->
          rangeOf ty
    { start: (rangeOf name).start
    , end
    }

instance tokensOfDataCtor :: TokensOf e => TokensOf (DataCtor e) where
  tokensOf (DataCtor { name, fields }) =
    tokensOf name <> tokensOf fields

instance rangeOfDecl :: RangeOf e => RangeOf (Declaration e) where
  rangeOf = case _ of
    DeclData { keyword, name, vars } ctors -> do
      let
        { end } = case ctors of
          Nothing ->
            case Array.last vars of
              Nothing ->
                rangeOf name
              Just var ->
                rangeOf var
          Just (Tuple _ (Separated { head, tail })) ->
            rangeOf $ maybe head snd $ Array.last tail
      { start: keyword.range.start
      , end
      }
    DeclType { keyword } _ ty ->
      { start: keyword.range.start
      , end: (rangeOf ty).end
      }
    DeclNewtype { keyword } _ _ ty ->
      { start: keyword.range.start
      , end: (rangeOf ty).end
      }
    DeclClass { keyword, name, vars, fundeps } members -> do
      let
        { end } = case members of
          Nothing ->
            case fundeps of
              Nothing ->
                case Array.last vars of
                  Nothing ->
                    rangeOf name
                  Just var ->
                    rangeOf var
              Just (Tuple _ fundeps) ->
                rangeOf fundeps
          Just (Tuple _ ms) ->
            rangeOf (NonEmptyArray.last ms)
      { start: keyword.range.start
      , end
      }
    DeclInstanceChain insts ->
      rangeOf insts
    DeclDerive keyword _ { className, types } -> do
      let
        { end } = case Array.last types of
          Nothing ->
            rangeOf className
          Just ty ->
            rangeOf ty
      { start: keyword.range.start
      , end
      }
    DeclKindSignature keyword lbl ->
      { start: keyword.range.start
      , end: (rangeOf lbl).end
      }
    DeclSignature sig ->
      rangeOf sig
    DeclValue { name, guarded } ->
      { start: (rangeOf name).start
      , end: (rangeOf guarded).end
      }
    DeclFixity { keyword: Tuple keyword _, operator } ->
      { start: keyword.range.start
      , end: (rangeOf operator).end
      }
    DeclForeign keyword _ frn ->
      { start: keyword.range.start
      , end: (rangeOf frn).end
      }
    DeclRole keyword _ _ roles ->
      { start: keyword.range.start
      , end: (fst (NonEmptyArray.last roles)).range.end
      }
    DeclError e ->
      rangeOf e

instance tokensOfDecl :: TokensOf e => TokensOf (Declaration e) where
  tokensOf = case _ of
    DeclData { keyword, name, vars } ctors ->
      cons keyword $ defer \_ ->
        tokensOf name
          <> tokensOf vars
          <> foldMap (\(Tuple t cs) -> cons t $ tokensOf cs) ctors
    DeclType { keyword, name, vars } tok ty ->
      cons keyword $ defer \_ ->
        tokensOf name
          <> tokensOf vars
          <> singleton tok
          <> tokensOf ty
    DeclNewtype { keyword, name, vars } tok n ty ->
      cons keyword $ defer \_ ->
        tokensOf name
          <> tokensOf vars
          <> singleton tok
          <> tokensOf n
          <> tokensOf ty
    DeclClass { keyword, super, name, vars, fundeps } members ->
      cons keyword $ defer \_ ->
        foldMap (\(Tuple cs t) -> tokensOf cs <> singleton t) super
          <> tokensOf name
          <> tokensOf vars
          <> foldMap (\(Tuple t fs) -> cons t $ tokensOf fs) fundeps
          <> foldMap (\(Tuple t ls) -> cons t $ tokensOf ls) members
    DeclInstanceChain insts ->
      tokensOf insts
    DeclDerive keyword tok inst ->
      cons keyword $ defer \_ ->
        foldMap singleton tok
          <> singleton inst.keyword
          <> foldMap (\(Tuple cs t) -> tokensOf cs <> singleton t) inst.name
          <> foldMap (\(Tuple cs t) -> tokensOf cs <> singleton t) inst.constraints
          <> tokensOf inst.className
          <> tokensOf inst.types
    DeclKindSignature keyword lbl ->
      cons keyword $ defer \_ ->
        tokensOf lbl
    DeclSignature sig ->
      tokensOf sig
    DeclValue { name, binders, guarded } ->
      tokensOf name <> defer \_ ->
        tokensOf binders <> tokensOf guarded
    DeclFixity { keyword: Tuple keyword _, prec: Tuple prec _, operator } ->
      cons keyword $ defer \_ ->
        cons prec $ tokensOf operator
    DeclForeign keyword imp frn ->
      cons keyword $ defer \_ ->
        cons imp $ tokensOf frn
    DeclRole keyword rl n roles ->
      cons keyword $ defer \_ ->
        singleton rl
          <> tokensOf n
          <> foldMap (\(Tuple t _) -> singleton t) roles
    DeclError e ->
      tokensOf e

instance rangeOfClassFundep :: RangeOf ClassFundep where
  rangeOf = case _ of
    FundepDetermined t ns ->
      { start: t.range.start
      , end: (rangeOf (NonEmptyArray.last ns)).end
      }
    FundepDetermines ns1 _ ns2 ->
      { start: (rangeOf (NonEmptyArray.head ns1)).start
      , end: (rangeOf (NonEmptyArray.last ns2)).end
      }

instance tokensOfClassFundep :: TokensOf ClassFundep where
  tokensOf = case _ of
    FundepDetermined t ns ->
      cons t $ tokensOf ns
    FundepDetermines ns1 t ns2 ->
      tokensOf ns1 <> singleton t <> tokensOf ns2

instance rangeOfInstance :: RangeOf e => RangeOf (Instance e) where
  rangeOf (Instance { head: { keyword, className, types }, body }) = do
    let
      { end } = case body of
        Nothing ->
          case Array.last types of
            Nothing ->
              rangeOf className
            Just ty ->
              rangeOf ty
        Just (Tuple _ bs) ->
          rangeOf (NonEmptyArray.last bs)
    { start: keyword.range.start
    , end
    }

instance tokensOfInstance :: TokensOf e => TokensOf (Instance e) where
  tokensOf (Instance { head, body }) =
    cons head.keyword $ defer \_ ->
      foldMap (\(Tuple cs t) -> tokensOf cs <> singleton t) head.name
        <> foldMap (\(Tuple cs t) -> tokensOf cs <> singleton t) head.constraints
        <> tokensOf head.className
        <> tokensOf head.types
        <> foldMap (\(Tuple t bs) -> cons t $ tokensOf bs) body

instance rangeOfGuarded :: RangeOf e => RangeOf (Guarded e) where
  rangeOf = case _ of
    Unconditional t wh ->
      { start: t.range.start
      , end: (rangeOf wh).end
      }
    Guarded gs ->
      { start: (rangeOf (NonEmptyArray.head gs)).start
      , end: (rangeOf (NonEmptyArray.last gs)).end
      }

instance tokensOfGuarded :: TokensOf e => TokensOf (Guarded e) where
  tokensOf = case _ of
    Unconditional t wh ->
      cons t $ tokensOf wh
    Guarded gs ->
      tokensOf gs

instance rangeOfGuardedExpr :: RangeOf e => RangeOf (GuardedExpr e) where
  rangeOf (GuardedExpr ge) =
    { start: ge.bar.range.start
    , end: (rangeOf ge.where).end
    }

instance tokensOfGuardedExpr :: TokensOf e => TokensOf (GuardedExpr e) where
  tokensOf (GuardedExpr ge) =
    cons ge.bar $ defer \_ ->
      tokensOf ge.patterns
        <> singleton ge.separator
        <> tokensOf ge.where

instance tokensOfPatternGuard :: TokensOf e => TokensOf (PatternGuard e) where
  tokensOf (PatternGuard { binder, expr }) =
    foldMap (\(Tuple b t) -> tokensOf b <> singleton t) binder
      <> tokensOf expr

instance rangeOfFixityOp :: RangeOf FixityOp where
  rangeOf = case _ of
    FixityValue n1 _ n2 ->
      { start: (rangeOf n1).start
      , end: (rangeOf n2).end
      }
    FixityType t _ _ n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }

instance tokensOfFixityOp :: TokensOf FixityOp where
  tokensOf = case _ of
    FixityValue n1 t n2 ->
      tokensOf n1 <> singleton t <> tokensOf n2
    FixityType t1 n1 t2 n2 ->
      cons t1 $ tokensOf n1 <> singleton t2 <> tokensOf n2

instance rangeOfForeign :: RangeOf e => RangeOf (Foreign e) where
  rangeOf = case _ of
    ForeignValue lbl ->
      rangeOf lbl
    ForeignData t lbl ->
      { start: t.range.start
      , end: (rangeOf lbl).end
      }
    ForeignKind t n ->
      { start: t.range.start
      , end: (rangeOf n).end
      }

instance tokensOfForeign :: TokensOf e => TokensOf (Foreign e) where
  tokensOf = case _ of
    ForeignValue lbl ->
      tokensOf lbl
    ForeignData t lbl ->
      cons t $ tokensOf lbl
    ForeignKind t n ->
      cons t $ tokensOf n

instance rangeOfInstanceBinding :: RangeOf e => RangeOf (InstanceBinding e) where
  rangeOf = case _ of
    InstanceBindingSignature lbl ->
      rangeOf lbl
    InstanceBindingName { name, guarded } ->
      { start: (rangeOf name).start
      , end: (rangeOf guarded).end
      }

instance tokensOfInstanceBinding :: TokensOf e => TokensOf (InstanceBinding e) where
  tokensOf = case _ of
    InstanceBindingSignature lbl ->
      tokensOf lbl
    InstanceBindingName { name, binders, guarded } ->
      tokensOf name
        <> tokensOf binders
        <> tokensOf guarded

instance rangeOfExpr :: RangeOf e => RangeOf (Expr e) where
  rangeOf = case _ of
    ExprHole n ->
      rangeOf n
    ExprSection t ->
      t.range
    ExprIdent n ->
      rangeOf n
    ExprConstructor n ->
      rangeOf n
    ExprBoolean t _ ->
      t.range
    ExprChar t _ ->
      t.range
    ExprString t _ ->
      t.range
    ExprInt t _ ->
      t.range
    ExprNumber t _ ->
      t.range
    ExprArray exprs ->
      rangeOf exprs
    ExprRecord exprs ->
      rangeOf exprs
    ExprParens w ->
      rangeOf w
    ExprTyped expr _ ty ->
      { start: (rangeOf expr).start
      , end: (rangeOf ty).end
      }
    ExprInfix expr ops ->
      { start: (rangeOf expr).start
      , end: (rangeOf (snd (NonEmptyArray.last ops))).end
      }
    ExprOp expr ops ->
      { start: (rangeOf expr).start
      , end: (rangeOf (snd (NonEmptyArray.last ops))).end
      }
    ExprOpName n ->
      rangeOf n
    ExprNegate t expr ->
      { start: t.range.start
      , end: (rangeOf expr).end
      }
    ExprRecordAccessor { expr, path } ->
      { start: (rangeOf expr).start
      , end: (rangeOf path).end
      }
    ExprRecordUpdate expr upds ->
      { start: (rangeOf expr).start
      , end: (rangeOf upds).end
      }
    ExprApp expr exprs ->
      { start: (rangeOf expr).start
      , end: (rangeOf (NonEmptyArray.last exprs)).end
      }
    ExprLambda { symbol, body } ->
      { start: symbol.range.start
      , end: (rangeOf body).end
      }
    ExprIf ifte ->
      { start: ifte.keyword.range.start
      , end: (rangeOf ifte.false).end
      }
    ExprCase { keyword, branches } ->
      { start: keyword.range.start
      , end: (rangeOf (snd (NonEmptyArray.last branches))).end
      }
    ExprLet { keyword, body } ->
      { start: keyword.range.start
      , end: (rangeOf body).end
      }
    ExprDo { keyword, statements } ->
      { start: keyword.range.start
      , end: (rangeOf (NonEmptyArray.last statements)).end
      }
    ExprAdo { keyword, result } ->
      { start: keyword.range.start
      , end: (rangeOf result).end
      }
    ExprError e ->
      rangeOf e

instance tokensOfExpr :: TokensOf e => TokensOf (Expr e) where
  tokensOf = case _ of
    ExprHole n ->
      tokensOf n
    ExprSection t ->
      singleton t
    ExprIdent n ->
      tokensOf n
    ExprConstructor n ->
      tokensOf n
    ExprBoolean t _ ->
      singleton t
    ExprChar t _ ->
      singleton t
    ExprString t _ ->
      singleton t
    ExprInt t _ ->
      singleton t
    ExprNumber t _ ->
      singleton t
    ExprArray exprs ->
      tokensOf exprs
    ExprRecord exprs ->
      tokensOf exprs
    ExprParens w ->
      tokensOf w
    ExprTyped expr t ty ->
      tokensOf expr <> defer \_ -> cons t $ tokensOf ty
    ExprInfix expr ops ->
      tokensOf expr <> defer \_ -> tokensOf ops
    ExprOp expr ops ->
      tokensOf expr <> defer \_ -> tokensOf ops
    ExprOpName n ->
      tokensOf n
    ExprNegate t expr ->
      cons t $ tokensOf expr
    ExprRecordAccessor { expr, dot, path } ->
      tokensOf expr <> defer \_ -> cons dot $ tokensOf path
    ExprRecordUpdate expr upds ->
      tokensOf expr <> defer \_ -> tokensOf upds
    ExprApp expr exprs ->
      tokensOf expr <> defer \_ -> tokensOf exprs
    ExprLambda { symbol, binders, arrow, body } ->
      cons symbol $ defer \_ ->
        tokensOf binders
          <> singleton arrow
          <> tokensOf body
    ExprIf ifte ->
      cons ifte.keyword $ defer \_ ->
        tokensOf ifte.cond
          <> singleton ifte.then
          <> tokensOf ifte.true
          <> singleton ifte.else
          <> tokensOf ifte.false
    ExprCase cs ->
      cons cs.keyword $ defer \_ ->
        tokensOf cs.head
          <> singleton cs.of
          <> tokensOf cs.branches
    ExprLet lt ->
      cons lt.keyword $ defer \_ ->
        tokensOf lt.bindings
          <> singleton lt.in
          <> tokensOf lt.body
    ExprDo { keyword, statements } ->
      cons keyword $ defer \_ -> tokensOf statements
    ExprAdo block ->
      cons block.keyword $ defer \_ ->
        tokensOf block.statements
          <> singleton block.in
          <> tokensOf block.result
    ExprError e ->
      tokensOf e

instance tokensOfRecordUpdate :: TokensOf e => TokensOf (RecordUpdate e) where
  tokensOf = case _ of
    RecordUpdateLeaf n t e ->
      tokensOf n <> singleton t <> tokensOf e
    RecordUpdateBranch n us ->
      tokensOf n <> tokensOf us

instance rangeOfDoStatement :: RangeOf e => RangeOf (DoStatement e) where
  rangeOf = case _ of
    DoLet t bindings ->
      { start: t.range.start
      , end: (rangeOf (NonEmptyArray.last bindings)).end
      }
    DoDiscard expr ->
      rangeOf expr
    DoBind b _ expr ->
      { start: (rangeOf b).start
      , end: (rangeOf expr).end
      }
    DoError e ->
      rangeOf e

instance tokensOfDoStatement :: TokensOf e => TokensOf (DoStatement e) where
  tokensOf = case _ of
    DoLet t bindings ->
      cons t $ defer \_ -> tokensOf bindings
    DoDiscard expr ->
      tokensOf expr
    DoBind b t expr ->
      tokensOf b <> defer \_ -> cons t $ tokensOf expr
    DoError e ->
      tokensOf e

instance rangeOfLetBinding :: RangeOf e => RangeOf (LetBinding e) where
  rangeOf = case _ of
    LetBindingSignature lbl ->
      rangeOf lbl
    LetBindingName { name, guarded } ->
      { start: (rangeOf name).start
      , end: (rangeOf guarded).end
      }
    LetBindingPattern b _ wh ->
      { start: (rangeOf b).start
      , end: (rangeOf wh).end
      }
    LetBindingError e ->
      rangeOf e

instance tokensOfLetBinding :: TokensOf e => TokensOf (LetBinding e) where
  tokensOf = case _ of
    LetBindingSignature lbl ->
      tokensOf lbl
    LetBindingName { name, binders, guarded } ->
      tokensOf name <> defer \_ -> tokensOf binders <> tokensOf guarded
    LetBindingPattern b t wh ->
      tokensOf b <> defer \_ -> cons t $ tokensOf wh
    LetBindingError e ->
      tokensOf e

instance rangeOfBinder :: RangeOf e => RangeOf (Binder e) where
  rangeOf = case _ of
    BinderWildcard t ->
      t.range
    BinderVar n ->
      rangeOf n
    BinderNamed n _ b ->
      { start: (rangeOf n).start
      , end: (rangeOf b).end
      }
    BinderConstructor n bs ->
      case Array.last bs of
        Nothing ->
          rangeOf n
        Just b ->
          { start: (rangeOf n).start
          , end: (rangeOf b).end
          }
    BinderBoolean t _ ->
      t.range
    BinderChar t _ ->
      t.range
    BinderString t _ ->
      t.range
    BinderInt neg t _ ->
      case neg of
        Nothing ->
          t.range
        Just n ->
          { start: n.range.start
          , end: t.range.end
          }
    BinderNumber neg t _ ->
      case neg of
        Nothing ->
          t.range
        Just n ->
          { start: n.range.start
          , end: t.range.end
          }
    BinderArray bs ->
      rangeOf bs
    BinderRecord bs ->
      rangeOf bs
    BinderParens b ->
      rangeOf b
    BinderTyped b _ ty ->
      { start: (rangeOf b).start
      , end: (rangeOf ty).end
      }
    BinderOp b ops ->
      { start: (rangeOf b).start
      , end: (rangeOf (snd (NonEmptyArray.last ops))).end
      }
    BinderError e ->
      rangeOf e

instance tokensOfBinder :: TokensOf e => TokensOf (Binder e) where
  tokensOf = case _ of
    BinderWildcard t ->
      singleton t
    BinderVar n ->
      tokensOf n
    BinderNamed n t b ->
      tokensOf n <> defer \_ -> cons t $ tokensOf b
    BinderConstructor n bs ->
      tokensOf n <> defer \_ -> tokensOf bs
    BinderBoolean t _ ->
      singleton t
    BinderChar t _ ->
      singleton t
    BinderString t _ ->
      singleton t
    BinderInt neg t _ ->
      foldMap singleton neg <> singleton t
    BinderNumber neg t _ ->
      foldMap singleton neg <> singleton t
    BinderArray bs ->
      tokensOf bs
    BinderRecord bs ->
      tokensOf bs
    BinderParens b ->
      tokensOf b
    BinderTyped b t ty ->
      tokensOf b <> defer \_ -> cons t $ tokensOf ty
    BinderOp b ops ->
      tokensOf b <> defer \_ -> tokensOf ops
    BinderError e ->
      tokensOf e

instance tokensOfRecordLabeled :: TokensOf a => TokensOf (RecordLabeled a) where
  tokensOf = case _ of
    RecordPun n ->
      tokensOf n
    RecordField n t a ->
      tokensOf n <> defer \_ -> cons t $ tokensOf a

instance rangeOfWhere :: RangeOf e => RangeOf (Where e) where
  rangeOf (Where { expr, bindings }) = case bindings of
    Nothing ->
      rangeOf expr
    Just (Tuple _ lb) ->
      { start: (rangeOf expr).start
      , end: (rangeOf (NonEmptyArray.last lb)).end
      }

instance tokensOfWhere :: TokensOf e => TokensOf (Where e) where
  tokensOf (Where { expr, bindings }) =
    tokensOf expr <> defer \_ ->
      foldMap (\(Tuple t bs) -> cons t $ tokensOf bs) bindings
