module Spago.Search.TypeQuery where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List, some, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList, cons', uncons)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (ParseError, Parser, runParser, try)
import Text.Parsing.StringParser.CodePoints (alphaNum, anyLetter, char, eof, lowerCaseChar, skipSpaces, string, upperCaseChar)
import Text.Parsing.StringParser.Combinators (fix, sepBy, sepBy1, sepEndBy, sepEndBy1)

data TypeQuery
  = QAny String
  | QConcrete String
  | QFun TypeQuery TypeQuery
  | QApp TypeQuery TypeQuery
  | QForAll (NonEmptyList String) TypeQuery
  | QConstraint String (List String) TypeQuery
  | QRow (List (Tuple String TypeQuery))
  | QEmpty

derive instance eqTypeQuery :: Eq TypeQuery
derive instance genericTypeQuery :: Generic TypeQuery _

instance showTypeQuery :: Show TypeQuery where
  show x = genericShow x

parseTypeQuery :: String -> Either ParseError TypeQuery
parseTypeQuery = runParser (typeQueryParser <* eof)

typeQueryParser :: Parser TypeQuery
typeQueryParser = fix \typeQuery ->
  let rowFields =
        QRow <$> sepBy (Tuple <$> (skipSpaces *> ident <* skipSpaces <* string "::") <*>
                                  (skipSpaces *> typeQuery <* skipSpaces))
                       (string "," *> skipSpaces)

      row = string "{" *> rowFields <* string "}"

      binders =
        string "forall" *> some space *> sepEndBy1 ident (some space) <* string "." <* skipSpaces

      for_all = QForAll <$> binders <*> typeQuery

      parens =
        string "(" *> skipSpaces *> typeQuery <* skipSpaces <* string ")"

      atom = skipSpaces *> (
        for_all    <|>
        parens     <|>
        row        <|>
        concrete   <|>
        any
      )

      apps =
        foldl1 QApp <$> sepEndBy1 atom (some space)

      funs =
        foldr1 QFun <$> sepBy1 apps (string "->" *> skipSpaces)

      constrained =
        QConstraint <$> (upperCaseIdent <* some space) <*>
                        (sepEndBy ident (some space) <* string "=>") <*>
                        funs
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
  QAny <$> lowerCaseIdent

concrete :: Parser TypeQuery
concrete =
  QConcrete <$> upperCaseIdent

ident :: Parser String
ident = do
  head <- anyLetter
  rest <- Array.many alphaNum
  pure $ fromCharArray $ pure head <> rest

upperCaseIdent :: Parser String
upperCaseIdent = do
  head <- upperCaseChar
  rest <- Array.many alphaNum
  pure $ fromCharArray $ pure head <> rest

lowerCaseIdent :: Parser String
lowerCaseIdent = do
  head <- lowerCaseChar
  rest <- Array.many alphaNum
  pure $ fromCharArray $ pure head <> rest

space :: Parser Char
space = char ' '
