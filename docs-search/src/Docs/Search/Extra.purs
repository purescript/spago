module Docs.Search.Extra where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldl)
import Data.List (List, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList, cons', uncons)
import Data.String.CodeUnits as String
import Data.Newtype (wrap)
import Data.Maybe (Maybe(..), fromMaybe)

whenJust :: forall a m. Monad m => Maybe a -> (a -> m Unit) -> m Unit
whenJust (Just a) f = f a
whenJust _ _ = pure unit

foldMapFlipped :: forall a m f. Foldable f => Monoid m =>  f a -> (a -> m) -> m
foldMapFlipped = flip foldMap

infixr 7 foldMapFlipped as >#>

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

-- | Try to guess repository main page on github from git URL.
homePageFromRepository :: String -> String
homePageFromRepository repo =
  fromMaybe repo $ String.stripSuffix (wrap ".git") $
  fromMaybe repo $ String.stripPrefix (wrap "git:") repo <#> ("https:" <> _)

stringToList :: String -> List Char
stringToList = List.fromFoldable <<< String.toCharArray

listToString :: List Char -> String
listToString = String.fromCharArray <<< Array.fromFoldable
