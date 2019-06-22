module Spago.Search.Index where

import Prelude

import Spago.Search.Declarations (Declarations(..), IndexEntry(..))

import Data.Foldable (foldr)
import Data.List as List
import Data.Newtype (class Newtype)
import Data.Search.Trie (Trie, insert)
import Data.String.CodeUnits (toCharArray)

newtype SearchIndex = SearchIndex (Trie Char IndexEntry)

derive instance newtypeEmailAddress :: Newtype SearchIndex _

mkSearchIndex :: Array Declarations -> SearchIndex
mkSearchIndex = SearchIndex <<< foldr insertDeclarations mempty
  where
    insertDeclarations :: (Declarations -> Trie Char IndexEntry -> Trie Char IndexEntry)
    insertDeclarations (Declarations { name, declarations }) trie
      = foldr insertIndexEntry trie declarations
    insertIndexEntry :: (IndexEntry -> Trie Char IndexEntry -> Trie Char IndexEntry)
    insertIndexEntry entry@(IndexEntry { title })
      = insert (List.fromFoldable $ toCharArray title) entry
