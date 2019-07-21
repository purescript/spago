module Docs.Search.Index where

import Prelude

import Docs.Search.Config (config)
import Docs.Search.SearchResult (SearchResult)

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array as Array
import Data.Char as Char
import Data.Either (hush)
import Data.List (List, (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Search.Trie (Trie)
import Data.Search.Trie as Trie
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, try)

newtype Index
  = Index (Map Int (Trie Char (List SearchResult)))

derive instance newtypeIndex :: Newtype Index _
derive newtype instance semigroupIndex :: Semigroup Index
derive newtype instance monoidIndex :: Monoid Index

query
  :: Index
  -> String
  -> Aff { index :: Index, results :: Array SearchResult }
query index@(Index indexMap) input = do
  let
    path :: List Char
    path =
      List.fromFoldable $
      String.toCharArray $
      input

    partId :: Int
    partId = getPartId path

  case Map.lookup partId indexMap of
    Just trie ->
      pure { index, results: flatten $ Trie.queryValues path trie }
    Nothing -> do


      eiPartJson <-
        try $ toAffE $ loadIndex_ partId $ config.mkIndexPartLoadPath partId

      let
        mbNewTrie :: Maybe (Trie Char (List SearchResult))
        mbNewTrie = do
          json <- hush eiPartJson
          results <- hush (decodeJson json) :: Maybe (Array (Tuple String (Array SearchResult)))
          pure $ Array.foldr insertResults mempty results

      case mbNewTrie of
        Just newTrie -> do
          pure { index: Index $ Map.insert partId newTrie indexMap
               , results: flatten $ Trie.queryValues path newTrie
               }
        Nothing -> do
          pure { index, results: mempty }

  where
    flatten = Array.concat <<< Array.fromFoldable <<< map Array.fromFoldable

insertResults
  :: Tuple String (Array SearchResult)
  -> Trie Char (List SearchResult)
  -> Trie Char (List SearchResult)
insertResults (Tuple path newResults) =
  Trie.alter pathList insert
  where
    pathList = List.fromFoldable $ String.toCharArray path

    insert
      :: Maybe (List SearchResult)
      -> Maybe (List SearchResult)
    insert mbOldResults =
      case mbOldResults of
        Nothing  -> Just $ List.fromFoldable newResults
        Just old -> Just $ List.fromFoldable newResults <> old

-- | Find in which part of the index this path can be found.
getPartId :: List Char -> Int
getPartId (a : b : _) =
  (Char.toCharCode a + Char.toCharCode b) `mod` config.numberOfIndexParts
getPartId (a : _) =
  Char.toCharCode a `mod` config.numberOfIndexParts
getPartId _ = 0

-- | Load a part of the index by injecting a <script> tag into the DOM.
foreign import loadIndex_
 :: Int
 -> String
 -> Effect (Promise Json)
