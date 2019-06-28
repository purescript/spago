module Spago.Search.TypeQuery where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String.Common (trim) as String
import Data.List (List(..), many, some, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList, cons', uncons)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..), snd)
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
        string "forall" *> some space *> sepEndBy1 ident (some space) <* string "." <* skipSpaces

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
        QConstraint <$> (upperCaseIdent <* some space) <*>
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
