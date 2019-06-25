module Spago.Search.Index where

import Prelude

import Spago.Search.Declarations (Declarations(..), IndexEntry(..))

import Data.Foldable (foldr)
import Data.Array ((!!))
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common as String
import Data.String.Pattern (Pattern(..))
import Data.Newtype (class Newtype)
import Data.Search.Trie (Trie, alter)
import Data.String.CodeUnits (toCharArray)
import Data.String.Common (toLower)

newtype SearchIndex = SearchIndex (Trie Char (List SearchResult))

derive instance newtypeSearchIndex :: Newtype SearchIndex _

newtype SearchResult
  = SearchResult { name :: String
                 , comments :: Maybe String
                 , declType :: String
                 , moduleName :: String
                 , packageName :: String
                 , sourceSpan :: { start :: Array Int
                                 , end :: Array Int
                                 , name :: String
                                 }
                 }

derive instance newtypeSearchResult :: Newtype SearchResult _

mkSearchIndex :: Array Declarations -> SearchIndex
mkSearchIndex = SearchIndex <<< foldr insertDeclarations mempty
  where
    insertDeclarations (Declarations { name, declarations }) trie
      = foldr (insertIndexEntry name) trie declarations

    insertIndexEntry moduleName entry@(IndexEntry { title }) trie
      = foldr insertSearchResult trie
        (resultsForEntry moduleName entry)

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

    resultsForEntry
      :: String
      -> IndexEntry
      -> List { path :: String
              , result :: SearchResult
              }
    resultsForEntry moduleName (IndexEntry { title
                                           , comments
                                           , info
                                           , sourceSpan }) =
      List.fromFoldable
      [ { path: title
        , result: SearchResult { name: title
                               , comments
                               , declType: info.declType
                               , moduleName
                               , sourceSpan
                               , packageName:
                                 extractPackageName sourceSpan
                               }
        }
      ]

    extractPackageName { name } =
      let chunks = String.split (Pattern "/") name in
      fromMaybe "<unknown>" $
      chunks !! 0 >>= \dir ->
      -- TODO: is it safe to assume that directory name is ".spago"?
      if dir == ".spago" then
        chunks !! 1
      else
        Just "<local package>"
