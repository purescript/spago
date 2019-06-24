module Spago.Search.Index where

import Prelude

import Spago.Search.Declarations (Declarations(..), IndexEntry(..))

import Data.Foldable (foldr)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Search.Trie (Trie, alter)
import Data.String.CodeUnits (toCharArray)
import Data.String.Common (toLower)

newtype SearchIndex = SearchIndex (Trie Char (List SearchResult))

derive instance newtypeSearchIndex :: Newtype SearchIndex _

newtype SearchResult
  = SearchResult { name :: String
                 }

derive instance newtypeSearchResult :: Newtype SearchResult _

mkSearchIndex :: Array Declarations -> SearchIndex
mkSearchIndex = SearchIndex <<< foldr insertDeclarations mempty
  where
    insertDeclarations (Declarations { name, declarations }) trie
      = foldr insertIndexEntry trie declarations

    insertIndexEntry entry@(IndexEntry { title }) trie
      = foldr insertSearchResult trie (resultsForEntry entry)

    insertSearchResult :: { path :: String
                          , result :: SearchResult
                          } -> Trie Char (List SearchResult) -> Trie Char (List SearchResult)
    insertSearchResult { path, result } trie =
      let path' = List.fromFoldable $ toCharArray $ toLower path in
        alter path' (updateResults result) trie

    updateResults result mbOldResults =
      case mbOldResults of
        Just oldResults ->
          Just $ List.singleton result <> oldResults
        Nothing ->
          Just $ List.singleton result

    resultsForEntry :: IndexEntry -> List { path :: String
                                          , result :: SearchResult
                                          }
    resultsForEntry (IndexEntry { title })
      = List.fromFoldable
        [ { path: title
          , result : SearchResult { name: title }
          }
        ]
