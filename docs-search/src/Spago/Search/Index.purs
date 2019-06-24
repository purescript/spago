module Spago.Search.Index where

import Prelude

import Spago.Search.Declarations (Declarations(..), IndexEntry(..))

import Data.Foldable (foldr)
import Data.List as List
import Data.List ((:))
import Data.Newtype (class Newtype)
import Data.Search.Trie (Trie, alter)
import Data.String.CodeUnits (toCharArray)
import Data.String.Common (toLower)
import Data.Maybe

newtype SearchIndex = SearchIndex (Trie Char (List.List IndexEntry))

derive instance newtypeEmailAddress :: Newtype SearchIndex _

mkSearchIndex :: Array Declarations -> SearchIndex
mkSearchIndex = SearchIndex <<< foldr insertDeclarations mempty
  where
    insertDeclarations (Declarations { name, declarations }) trie
      = foldr insertIndexEntry trie declarations

    insertIndexEntry entry@(IndexEntry { title })
      = alter (List.fromFoldable $ toCharArray $ toLower title) updateEntries
        where
          updateEntries mbOldEntries =
            case mbOldEntries of
              Just oldEntries ->
                Just $ entry : oldEntries
              Nothing ->
                Just $ List.singleton entry
