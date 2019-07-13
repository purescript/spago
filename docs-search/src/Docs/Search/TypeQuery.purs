module Docs.Search.TypeQuery
       ( TypeQuery(..)
       , Substitution(..)
       , parseTypeQuery
       , typeQueryParser
       , getFreeVariables
       , typeVarPenalty
       , penalty
       )
where

import Prelude

import Docs.Search.TypeDecoder
import Docs.Search.Config

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), many, some, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList, cons', uncons)
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Ord (abs)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray)
import Data.String.Common (trim) as String
import Data.Tuple (Tuple(..), fst, snd)
import Text.Parsing.StringParser (ParseError, Parser, runParser, try)
import Text.Parsing.StringParser.CodePoints (alphaNum, anyLetter, char, eof, lowerCaseChar, skipSpaces, string, upperCaseChar)
import Text.Parsing.StringParser.Combinators (fix, sepBy, sepBy1, sepEndBy, sepEndBy1)

data TypeQuery
  = QVar String
  | QConst String
  | QFun TypeQuery TypeQuery
  | QApp TypeQuery TypeQuery
  | QForAll (NonEmptyList String) TypeQuery
  | QConstraint String (List TypeQuery) TypeQuery
  | QRow (List (Tuple String TypeQuery))

derive instance eqTypeQuery :: Eq TypeQuery
derive instance genericTypeQuery :: Generic TypeQuery _

instance showTypeQuery :: Show TypeQuery where
  show x = genericShow x

parseTypeQuery :: String -> Either ParseError TypeQuery
parseTypeQuery = String.trim >>> runParser (typeQueryParser <* eof)

typeQueryParser :: Parser TypeQuery
typeQueryParser = fix \typeQuery ->
  let rowFields =
        QRow <$> sepBy (Tuple <$> (skipSpaces *> ident <* skipSpaces <* string "::") <*>
                                  (skipSpaces *> typeQuery <* skipSpaces))
                       (string "," *> skipSpaces)

      row = string "(" *> rowFields <* string ")"

      record = QApp (QConst "Record") <$> (string "{" *> rowFields <* string "}")

      binders =
        string "forall" *> some space *> sepEndBy1 ident skipSpaces <* string "." <* skipSpaces

      for_all = QForAll <$> binders <*> typeQuery

      parens =
        string "(" *> skipSpaces *> typeQuery <* skipSpaces <* string ")"

      atom = skipSpaces *> (
        for_all    <|>
        try parens <|>
        row        <|>
        record     <|>
        concrete   <|>
        any
      )

      apps =
        foldl1 QApp <$> sepEndBy1 atom (some space)

      funs =
        foldr1 QFun <$> sepBy1 apps (string "->" *> skipSpaces)

      constrained =
        QConstraint <$> (upperCaseIdent <* skipSpaces) <*>
                        (sepEndBy ((QVar <$> ident) <|> parens) (many space) <* string "=>" <* skipSpaces) <*>
                        typeQuery
  in
    try constrained <|> funs

foldl1 :: forall a. (a -> a -> a) -> NonEmptyList a -> a
foldl1 f as =
  case uncons as of
    { head, tail } -> foldl f head tail

foldr1 :: forall a. (a -> a -> a) -> NonEmptyList a -> a
foldr1 f = go List.Nil
  where
    go acc x = case uncons x of
      { head, tail } -> case List.uncons tail of
        Nothing -> List.foldl (flip f) head acc
        Just { head: head1, tail: tail1 } ->
          go (head : acc) (cons' head1 tail1)

any :: Parser TypeQuery
any = do
  QVar <$> lowerCaseIdent

concrete :: Parser TypeQuery
concrete =
  QConst <$> upperCaseIdent

ident :: Parser String
ident = do
  head <- anyLetter
  rest <- Array.many (alphaNum <|> char '\'')
  pure $ fromCharArray $ pure head <> rest

upperCaseIdent :: Parser String
upperCaseIdent = do
  head <- upperCaseChar
  rest <- Array.many (alphaNum <|> char '\'')
  pure $ fromCharArray $ pure head <> rest

lowerCaseIdent :: Parser String
lowerCaseIdent = do
  head <- lowerCaseChar
  rest <- Array.many (alphaNum <|> char '\'')
  pure $ fromCharArray $ pure head <> rest

space :: Parser Char
space = char ' '


-- | Used only in `getFreeVariables`.
data FreeVarCounterQueueEntry = Unbind (Set.Set String) | Next TypeQuery

getFreeVariables :: TypeQuery -> Set.Set String
getFreeVariables query = go Set.empty Set.empty (List.singleton $ Next query)
  where
    insertIfUnbound bound var free =
      if Set.member var bound
      then free
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
  = Instantiate String Type
  | Match String String
  | Generalize TypeQuery String
  | Substitute String String
  | MatchConstraints String String
  | MissingConstraint
  | ExcessiveConstraint

derive instance genericSubstitution :: Generic Substitution _

instance showSubstitution :: Show Substitution where
  show x = genericShow x

unify :: TypeQuery -> Type -> Maybe (List Substitution)
unify query type_ = go Nil (List.singleton { q: query, t: type_ })
  where
    go :: List Substitution -> List { q :: TypeQuery, t :: Type } -> Maybe (List Substitution)
    go acc Nil = Just acc

    -- * ForAll
    go acc ({ q: (QForAll queryBinders q), t:type_1@(ForAll _ _ _) } : rest) =
      let { binders, ty } = joinForAlls type_1 in
        go acc ({ q, t: ty } : rest)
    go acc ({ q, t: ForAll _ t _ } : rest) =
        go acc ({ q, t } : rest)
    go acc ({ q: (QForAll queryBinders q), t } : rest) =
        go acc ({ q, t } : rest)

    -- * Type variables
    go acc ({ q: QVar q, t: TypeVar v } : rest) =
      go (Substitute q v : acc) rest
    go acc ({ q, t: TypeVar v } : rest ) =
      go (Generalize q v : acc) rest
    go acc ({ q: QVar v, t } : rest) =
      go (Instantiate v t : acc) rest

    -- * Names
    go acc ({ q: QConst qname, t: TypeConstructor (QualifiedName { name }) } : rest) =
      go (Match qname name : acc) rest

    -- * Functions
    go acc ({ q: QFun q1 q2
            , t: TypeApp (TypeApp (TypeConstructor
                                    (QualifiedName { moduleName: [ "Prim" ]
                                                   , name: "Function" })) t1) t2 } : rest) =
      go acc ({ q: q1, t: t1 } : { q: q2, t: t2 } : rest)

    -- * Constraints
    go acc ({ q: QConstraint className args q
            , t: ConstrainedType cnstr t } : rest) =
      go (MatchConstraints className typeClassName  : acc) ({ q, t } : rest)
       where typeClassName = (unwrap (unwrap cnstr).constraintClass).name
    go acc ({ q: QConstraint _ _ q
            , t } : rest) =
      go (ExcessiveConstraint : acc) ({ q, t } : rest)
    go acc ({ q
            , t: ConstrainedType cnstr t } : rest) =
      go (MissingConstraint : acc) ({ q, t } : rest)

    -- * Rows
    go acc ({ q: QApp (QConst "Record") (QRow qRows)
            , t: TypeApp (TypeConstructor
                           (QualifiedName { moduleName: [ "Prim" ]
                                          , name: "Record" })) row } : rest) =
      let { rows, ty } = joinRows row in
        if List.length rows == List.length qRows then
          let
            sortedQRows = List.sortBy (\x y -> compare (fst x) (fst y)) qRows
            sortedRows  = List.sortBy (\x y -> compare x.row y.row) rows in
            go acc $
            (List.zipWith (\(Tuple _ q) { ty: t } -> { q, t }) sortedQRows sortedRows <> rest)
        else
          Nothing

    -- * Type application
    go acc ({ q: QApp q1 q2, t: TypeApp t1 t2 } : rest) =
      go acc ({ q: q1, t: t1 } : { q: q2, t: t2 } : rest)

    go acc _ = Nothing

penalty :: TypeQuery -> Type -> Maybe Int
penalty typeQuery ty =
  unify typeQuery ty <#> \substs ->
  typeVarPenalty substs * config.penalties.typeVars +
  namesPenalty substs +
  mismatchPenalty substs

-- | Penalty for type variables mismatch.
-- | Congruent types should receive zero penalty points.
typeVarPenalty :: List Substitution -> Int
typeVarPenalty substs =
  penaltyFor (varSubstMapWith (flip insertion)) +
  penaltyFor (varSubstMapWith       insertion)
    where
      penaltyFor varSubstMap =
        abs $
        List.length (List.foldMap List.fromFoldable varSubstMap) - Map.size varSubstMap

      insertion v1 v2 = Map.insertWith append v1 (Set.singleton v2)

      varSubstMapWith
        :: (String -> String -> Map String (Set String) -> Map String (Set String))
        -> Map String (Set String)
      varSubstMapWith f =
        List.foldr (case _ of
                      Substitute v1 v2 ->
                        f v1 v2
                      _ -> identity
                   ) mempty substs

-- | Penalty for name mismatches.
namesPenalty :: List Substitution -> Int
namesPenalty = go 0
  where
    go n Nil = n
    go n (Match a b : rest)
      | a == b = go n rest
      | otherwise = go (n + config.penalties.match) rest
    go n (MatchConstraints a b : rest)
      | a == b = go n rest
      | otherwise = go (n + config.penalties.matchConstraint) rest
    go n (_ : rest) = go n rest

-- | Penalty for generalization and instantiation.
mismatchPenalty :: List Substitution -> Int
mismatchPenalty = go 0
  where
    go n Nil = n
    go n (Instantiate _ _     : rest) = go (n + config.penalties.instantiate)         rest
    go n (Generalize _ _      : rest) = go (n + config.penalties.generalize)          rest
    go n (ExcessiveConstraint : rest) = go (n + config.penalties.excessiveConstraint) rest
    go n (MissingConstraint   : rest) = go (n + config.penalties.missingConstraint)   rest
    go n (_ : rest) = go n rest
