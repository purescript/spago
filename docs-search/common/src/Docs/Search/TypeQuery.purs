-- | `TypeQuery` is a representation of a user-provided type.
module Docs.Search.TypeQuery
  ( TypeQuery(..)
  , Substitution(..)
  , parseTypeQuery
  , typeQueryParser
  , getFreeVariables
  , typeVarPenalty
  , penalty
  , joinConstraints
  , joinForAlls
  , joinRows
  ) where

import Docs.Search.Config as Config
import Docs.Search.Extra (foldl1, foldr1)
import Docs.Search.TypeDecoder (Type(..), Type', Qualified(..), QualifiedBy(..), ModuleName(..), ProperName(..), Label(..), Constraint(..), TypeArgument)
import Docs.Search.Types (Identifier(..))

import Prelude
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List(..), many, some, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Ord (abs)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray)
import Data.String.Common (trim) as String
import Data.Tuple (Tuple(..), fst, snd)
import Language.PureScript.PSString as PSString
import StringParser (ParseError, Parser, runParser, try)
import StringParser.CodePoints (alphaNum, anyLetter, char, eof, lowerCaseChar, skipSpaces, string, upperCaseChar)
import StringParser.Combinators (fix, sepBy, sepBy1, sepEndBy, sepEndBy1)
import Safe.Coerce (coerce)

-- | We need type queries because we don't have a full-featured type parser
-- | available.
data TypeQuery
  = QVar Identifier
  | QConst Identifier
  | QFun TypeQuery TypeQuery
  | QApp TypeQuery TypeQuery
  | QForAll (NonEmptyList Identifier) TypeQuery
  | QConstraint Identifier (List TypeQuery) TypeQuery
  | QRow (List (Tuple Identifier TypeQuery))

derive instance eqTypeQuery :: Eq TypeQuery
derive instance genericTypeQuery :: Generic TypeQuery _

instance showTypeQuery :: Show TypeQuery where
  show x = genericShow x

parseTypeQuery :: String -> Either ParseError TypeQuery
parseTypeQuery = String.trim >>> runParser (typeQueryParser <* eof)

typeQueryParser :: Parser TypeQuery
typeQueryParser = fix \typeQuery ->
  let
    rowFields =
      QRow <$> sepBy
        ( Tuple <$> (skipSpaces *> ident <* skipSpaces <* string "::") <*>
            (skipSpaces *> typeQuery <* skipSpaces)
        )
        (string "," *> skipSpaces)

    row = string "(" *> rowFields <* string ")"

    record = QApp (QConst $ Identifier "Record") <$>
      (string "{" *> rowFields <* string "}")

    binders =
      string "forall" *> some space *> sepEndBy1 ident skipSpaces <* string "." <* skipSpaces

    for_all = QForAll <$> binders <*> typeQuery

    parens =
      string "(" *> skipSpaces *> typeQuery <* skipSpaces <* string ")"

    atom = skipSpaces *>
      ( for_all
          <|> try parens
          <|> row
          <|> record
          <|> concrete
          <|>
            any
      )

    apps =
      foldl1 QApp <$> sepEndBy1 atom (some space)

    funs =
      foldr1 QFun <$> sepBy1 apps (string "->" *> skipSpaces)

    constrained =
      QConstraint <$> (upperCaseIdent <* skipSpaces)
        <*>
          ( sepEndBy ((QVar <$> ident) <|> parens)
              (many space) <* string "=>" <* skipSpaces
          )
        <*>
          typeQuery
  in
    try constrained <|> funs

any :: Parser TypeQuery
any = do
  QVar <$> lowerCaseIdent

concrete :: Parser TypeQuery
concrete =
  QConst <$> upperCaseIdent

ident :: Parser Identifier
ident = do
  head <- anyLetter
  rest <- Array.many (alphaNum <|> char '\'')
  pure $ Identifier <$> fromCharArray $ pure head <> rest

upperCaseIdent :: Parser Identifier
upperCaseIdent = do
  head <- upperCaseChar
  rest <- Array.many (alphaNum <|> char '\'')
  pure $ Identifier $ fromCharArray $ pure head <> rest

lowerCaseIdent :: Parser Identifier
lowerCaseIdent = do
  head <- lowerCaseChar
  rest <- Array.many (alphaNum <|> char '\'')
  pure $ Identifier $ fromCharArray $ pure head <> rest

space :: Parser Char
space = char ' '

-- | Used only in `getFreeVariables`.
data FreeVarCounterQueueEntry = Unbind (Set.Set Identifier) | Next TypeQuery

getFreeVariables :: TypeQuery -> Set.Set Identifier
getFreeVariables query = go Set.empty Set.empty (List.singleton $ Next query)
  where
  insertIfUnbound bound var free =
    if Set.member var bound then free
    else Set.insert var free

  go bound free Nil = free
  go bound free (Unbind vars : rest) =
    go (Set.difference bound vars) free rest

  go bound free (Next (QVar var) : rest) =
    go bound (insertIfUnbound bound var free) rest

  go bound free (Next (QConst str) : rest) =
    go bound free rest
  go bound free (Next (QFun q1 q2) : rest) =
    go bound free (Next q1 : Next q2 : rest)
  go bound free (Next (QApp q1 q2) : rest) =
    go bound free (Next q1 : Next q2 : rest)

  go bound free (Next (QForAll nl q) : rest) =
    go (Set.union bound newBound) free queue
    where
    newBound = NonEmptyList.foldr Set.insert mempty nl
    queue = (Next q : Unbind (Set.difference newBound bound) : rest)

  go bound free (Next (QConstraint _ vars q) : rest) =
    go bound free ((Next <$> vars) <> (Next q : rest))

  go bound free (Next (QRow lst) : rest) =
    go bound free ((lst <#> snd >>> Next) <> rest)

data Substitution
  = Instantiate Identifier Type'
  | Match Identifier Identifier
  | Generalize TypeQuery Identifier
  | Substitute Identifier Identifier
  | MatchConstraints (Set Identifier) (Set Identifier)
  | MissingConstraint
  | ExcessiveConstraint
  | RowsMismatch Int Int
  -- Type and type query significantly differ.
  | Mismatch TypeQuery Type'
  -- A query of size 1 corresponds to some type.
  | TypeMismatch Type'
  -- A type of size 1 corresponds to some query.
  | QueryMismatch TypeQuery

derive instance genericSubstitution :: Generic Substitution _

instance showSubstitution :: Show Substitution where
  show x = genericShow x

-- | A mock-up of unification algorithm, that does not unify anything, actually.
-- | We use it to estimate how far a type is from a type query, by looking into
-- | the resulting list.
unify :: TypeQuery -> Type' -> List Substitution
unify query type_ = go Nil (List.singleton { q: query, t: type_ })
  where
  go :: List Substitution -> List { q :: TypeQuery, t :: Type' } -> List Substitution
  go acc Nil = acc
  go acc ({ q, t: ParensInType _ t } : rest) =
    go acc ({ q, t } : rest)

  -- * ForAll
  go acc ({ q, t: ForAll _ _ _ _ t _ } : rest) =
    go acc ({ q, t } : rest)
  go acc ({ q: (QForAll _ q), t } : rest) =
    go acc ({ q, t } : rest)

  -- * Constraints
  go
    acc
    ( { q: q@(QConstraint _ _ _)
      , t: t@(ConstrainedType _ _ _)
      } : rest
    ) =
    let
      qcs = Set.fromFoldable (joinQueryConstraints q).constraints
      tcs = Set.fromFoldable (joinConstraints t).constraints
    in
      -- TODO: use edit distance instead
      go (MatchConstraints qcs tcs : acc) rest
  go acc ({ q: QConstraint _ _ q, t } : rest) =
    go (ExcessiveConstraint : acc) ({ q, t } : rest)
  go acc ({ q, t: ConstrainedType _ _ t } : rest) =
    go (MissingConstraint : acc) ({ q, t } : rest)

  -- * Type variables
  go acc ({ q: QVar q, t: TypeVar _ v } : rest) =
    go (Substitute q (Identifier v) : acc) rest
  go acc ({ q, t: TypeVar _ v } : rest) =
    go (Generalize q (Identifier v) : acc) rest
  go acc ({ q: QVar v, t } : rest) =
    go (Instantiate v t : acc) rest

  -- * Names
  go acc ({ q: QConst qname, t: TypeConstructor _ (Qualified _ name) } : rest) =
    go (Match qname (coerce name) : acc) rest
  go acc ({ q: QConst _, t } : rest) =
    go (TypeMismatch t : acc) rest
  go acc ({ q, t: TypeConstructor _ _ } : rest) =
    go (QueryMismatch q : acc) rest

  -- type operators can't appear in type queries: this is always a mismatch
  go acc ({ q, t: TypeOp _ _ } : rest) =
    go (QueryMismatch q : acc) rest
  go acc ({ q, t: t@(BinaryNoParensType _ _ _ _) } : rest) =
    go (Mismatch q t : acc) rest

  -- * Functions
  go
    acc
    ( { q: QFun q1 q2
      , t: TypeApp _
          ( TypeApp _
              ( TypeConstructor _
                  ( Qualified
                      (ByModuleName (ModuleName "Prim"))
                      (ProperName "Function")
                  )
              )
              t1
          )
          t2
      } : rest
    ) =
    go acc ({ q: q1, t: t1 } : { q: q2, t: t2 } : rest)
  go acc ({ q: q@(QFun _ _), t } : rest) =
    go (Mismatch q t : acc) rest

  -- * Rows
  go
    acc
    ( { q: QApp (QConst (Identifier "Record")) (QRow qRows)
      , t: TypeApp _
          ( TypeConstructor _
              ( Qualified
                  (ByModuleName (ModuleName "Prim"))
                  (ProperName "Record")
              )
          )
          row
      } : rest
    ) =
    let
      { rows } = joinRows row
      qRowsLength = List.length qRows
      rowsLength = List.length rows
    in
      if rowsLength == qRowsLength then
        let
          sortedQRows = List.sortBy (\x y -> compare (fst x) (fst y)) qRows
          sortedRows = List.sortBy (\x y -> compare x.row y.row) rows
        in
          go
            -- match row names
            ( List.zipWith
                ( \(Tuple qRowName _) { row: rowName } ->
                    Match qRowName rowName
                )
                sortedQRows
                sortedRows
                <> acc
            )
            -- match row types
            ( List.zipWith
                ( \(Tuple _ q) { ty: t } ->
                    { q, t }
                )
                sortedQRows
                sortedRows
                <> rest
            )
      else
        go (RowsMismatch qRowsLength rowsLength : acc) rest

  go acc ({ q: q@(QRow _), t } : rest) =
    go (Mismatch q t : acc) rest

  -- * Type application
  go acc ({ q: QApp q1 q2, t: TypeApp _ t1 t2 } : rest) =
    go acc ({ q: q1, t: t1 } : { q: q2, t: t2 } : rest)

  go acc ({ q, t: TypeLevelString _ _ } : rest) =
    go (QueryMismatch q : acc) rest

  go acc ({ q, t: TypeLevelInt _ _ } : rest) =
    go (QueryMismatch q : acc) rest

  go acc ({ q, t: TypeLevelString _ _ } : rest) =
    go (QueryMismatch q : acc) rest

  go acc ({ q, t: TypeWildcard _ _ } : rest) =
    go (QueryMismatch q : acc) rest

  go acc ({ q, t: t@(RCons _ _ _ _) } : rest) =
    go (Mismatch q t : acc) rest

  go acc ({ q, t: REmpty _ } : rest) =
    go (QueryMismatch q : acc) rest

  go acc ({ q, t: KindedType _ _ _ } : rest) =
    go (QueryMismatch q : acc) rest

  go acc ({ t: t@(KindApp _ _ _) } : rest) =
    go (TypeMismatch t : acc) rest

  -- FIXME(new-ast)
  go acc ({ t: t@(TUnknown _ _) } : rest) =
    go (TypeMismatch t : acc) rest

  -- FIXME(new-ast)
  go acc ({ t: t@(Skolem _ _ _ _ _) } : rest) =
    go (TypeMismatch t : acc) rest

-- | Sum various penalties.
penalty :: TypeQuery -> Type' -> Int
penalty typeQuery ty =
  let
    substs = unify typeQuery ty
  in
    typeVarPenalty substs * Config.penalties.typeVars
      + namesPenalty substs
      +
        mismatchPenalty substs

-- | Penalty for type variables mismatch.
-- | Congruent types should receive zero penalty points.
typeVarPenalty :: List Substitution -> Int
typeVarPenalty substs =
  penaltyFor (varSubstMapWith (flip insertion)) +
    penaltyFor (varSubstMapWith insertion)
  where
  penaltyFor varSubstMap =
    abs $
      List.length (List.foldMap List.fromFoldable varSubstMap) - Map.size varSubstMap

  insertion v1 v2 = Map.insertWith append v1 (Set.singleton v2)

  varSubstMapWith
    :: ( Identifier
         -> Identifier
         -> Map Identifier (Set Identifier)
         -> Map Identifier (Set Identifier)
       )
    -> Map Identifier (Set Identifier)
  varSubstMapWith f =
    List.foldr
      ( case _ of
          Substitute v1 v2 ->
            f v1 v2
          _ -> identity
      )
      Map.empty
      substs

-- | Penalty for name mismatches.
namesPenalty :: List Substitution -> Int
namesPenalty = go 0
  where
  go p Nil = p
  go p (Match a b : rest)
    | a == b = go p rest
    | otherwise = go (p + Config.penalties.match) rest
  go p (MatchConstraints qcs tcs : rest) =
    let
      p' = Set.size (Set.union qcs tcs) -
        Set.size (Set.intersection qcs tcs)
    in
      go (p + Config.penalties.matchConstraint * p') rest
  go p (RowsMismatch n m : rest) = go (Config.penalties.rowsMismatch * abs (n - m)) rest
  go p (_ : rest) = go p rest

-- | Penalty for generalization and instantiation.
mismatchPenalty :: List Substitution -> Int
mismatchPenalty = go 0
  where
  go n Nil = n
  go n (Instantiate q t : rest) = go
    ( n + typeSize t *
        Config.penalties.instantiate
    )
    rest
  go n (Generalize q t : rest) = go
    ( n + typeQuerySize q *
        Config.penalties.generalize
    )
    rest
  go n (ExcessiveConstraint : rest) = go (n + Config.penalties.excessiveConstraint) rest
  go n (MissingConstraint : rest) = go (n + Config.penalties.missingConstraint) rest
  go n (Mismatch q t : rest) = go (n + typeQuerySize q + typeSize t) rest
  go n (TypeMismatch t : rest) = go (n + typeSize t) rest
  go n (QueryMismatch q : rest) = go (n + typeQuerySize q) rest
  go n (_ : rest) = go n rest

-- | Only returns a list of type class names (lists of arguments are omitted).
joinQueryConstraints
  :: TypeQuery
  -> { constraints :: List Identifier
     , ty :: TypeQuery
     }
joinQueryConstraints = go Nil
  where
  go acc (QConstraint name _ query) =
    go (name : acc) query
  go acc ty = { constraints: List.sort acc, ty }

typeQuerySize :: TypeQuery -> Int
typeQuerySize = go 0 <<< List.singleton
  where
  go n Nil = n
  go n (QVar _ : rest) =
    go (n + 1) rest
  go n (QConst _ : rest) =
    go (n + 1) rest
  go n (QFun q1 q2 : rest) =
    go (n + 1) (q1 : q2 : rest)
  go n (QApp q1 q2 : rest) =
    go (n + 1) (q1 : q2 : rest)
  go n (QForAll _ q : rest) =
    go (n + 1) (q : rest)
  go n (QConstraint _ _ q : rest) =
    go (n + 1) (q : rest)
  go n (QRow qs : rest) =
    go n ((qs <#> snd) <> rest)

typeSize :: Type' -> Int
typeSize = go 0 <<< List.singleton
  where
  go n Nil = n
  go n (TypeVar _ _ : rest) =
    go (n + 1) rest
  go n (TypeLevelString _ _ : rest) =
    go (n + 1) rest
  go n (TypeLevelInt _ _ : rest) =
    go (n + 1) rest
  go n (TypeWildcard _ _ : rest) =
    go (n + 1) rest
  go n (TypeConstructor _ _ : rest) =
    go (n + 1) rest
  go n (TypeOp _ _ : rest) =
    go (n + 1) rest
  go n (KindApp _ t1 t2 : res) = go n (t1 : t2 : res)
  go
    n
    ( TypeApp _
        ( TypeApp _
            ( TypeConstructor _
                ( Qualified
                    (ByModuleName (ModuleName "Prim"))
                    name
                )
            )
            t1
        )
        t2 : rest
    ) =
    go (n + 1) (t1 : t2 : rest)
  go n (TypeApp _ q1 q2 : rest) =
    go (n + 1) (q1 : q2 : rest)
  go n (ForAll _ _ _ _ t _ : rest) =
    go (n + 1) (t : rest)
  go n (ConstrainedType _ _ t : rest) =
    go (n + 1) (t : rest)
  go n (RCons _ _ t1 t2 : rest) =
    go (n + 1) (t1 : t2 : rest)
  go n (REmpty _ : rest) =
    go (n + 1) rest
  go n (KindedType _ t1 t2 : rest) =
    go n (t1 : t2 : rest)
  go n (BinaryNoParensType _ op t1 t2 : rest) =
    go (n + 1) (t1 : t2 : rest)
  go n (ParensInType _ t : rest) =
    go n (t : rest)
  go n (Skolem _ _ _ _ _ : rest) =
    go n rest -- FIXME(ast)
  go n (TUnknown _ _ : rest) =
    go n rest -- FIXME(ast)

joinForAlls
  :: Type'
  -> { binders :: List TypeArgument
     , ty :: Type'
     }
joinForAlls ty = go Nil ty
  where
  go acc (ForAll _ _ name kind ty' _) =
    go ({ name, kind } : acc) ty'
  go acc ty' = { binders: acc, ty: ty' }

type Row = { row :: Identifier, ty :: Type' }
type Rows = { rows :: List Row, ty :: Maybe Type' }

joinRows
  :: Type'
  -> Rows
joinRows = go Nil
  where
  go :: List Row -> Type' -> Rows
  go acc (RCons _ row ty rest) =
    go ({ row: labelToIdentifier row, ty } : acc) rest
  go acc ty =
    { rows: List.reverse acc
    , ty:
        case ty of
          REmpty _ -> Nothing
          ty' -> Just ty'
    }

-- | Only returns a list of type class names (lists of arguments are omitted).
joinConstraints
  :: Type'
  -> { constraints :: List Identifier
     , ty :: Type'
     }
joinConstraints = go Nil
  where
  go acc (ConstrainedType _ (Constraint { "class": Qualified _ (ProperName name) }) ty) =
    --: Qualified _ { name } }) ty) =
    go (wrap name : acc) ty
  go acc ty = { constraints: List.sort acc, ty }

labelToIdentifier :: Label -> Identifier
labelToIdentifier = coerce PSString.decodeStringWithReplacement
