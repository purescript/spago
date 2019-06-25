module Spago.Search.Index where

import Prelude

import Spago.Search.Declarations (ChildDeclType(..), ChildIndexEntry, DeclType(..), Declarations(..), IndexEntry(..))

import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (foldr)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Search.Trie (Trie, alter)
import Data.String.CodeUnits (stripPrefix, stripSuffix, toCharArray)
import Data.String.Common (toLower)
import Data.String.Common as String
import Data.String.Pattern (Pattern(..))

newtype SearchIndex = SearchIndex (Trie Char (List SearchResult))

derive instance newtypeSearchIndex :: Newtype SearchIndex _

newtype SearchResult
  = SearchResult { name :: String
                 , comments :: Maybe String
                 , hashAnchor :: String
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

insertDeclarations
  :: Declarations
     -> Trie Char (List SearchResult)
     -> Trie Char (List SearchResult)
insertDeclarations (Declarations { name, declarations }) trie
  = foldr (insertIndexEntry name) trie declarations

insertIndexEntry
  :: String
     -> IndexEntry
     -> Trie Char (List SearchResult)
     -> Trie Char (List SearchResult)
insertIndexEntry moduleName entry@(IndexEntry { title }) trie
  = foldr insertSearchResult trie (resultsForEntry moduleName entry)

insertSearchResult
  :: { path :: String
     , result :: SearchResult
     }
  -> Trie Char (List SearchResult)
  -> Trie Char (List SearchResult)
insertSearchResult { path, result } trie =
  let path' = List.fromFoldable $ toCharArray $ toLower path in
    alter path' updateResults trie
    where
      updateResults mbOldResults =
        case mbOldResults of
          Just oldResults ->
            Just $ result : oldResults
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
                                       , sourceSpan
                                       , children }) =
  let { name, hashAnchor } = extractHashAnchor info.declType title
      packageName = extractPackageName sourceSpan.name
  in
    { path: name
    , result: SearchResult { name: title
                           , comments
                           , hashAnchor
                           , moduleName
                           , sourceSpan
                           , packageName
                           }
    } : (
      List.fromFoldable $
      extractChildren sourceSpan packageName moduleName children
    )

extractHashAnchor
  :: DeclType
  -> String
  -> { hashAnchor :: String
     , name :: String
     }
extractHashAnchor DeclValue       name = { name, hashAnchor: "v" }
extractHashAnchor DeclData        name = { name, hashAnchor: "t" }
extractHashAnchor DeclTypeSynonym name = { name, hashAnchor: "t" }
extractHashAnchor DeclTypeClass   name = { name, hashAnchor: "v" }
extractHashAnchor DeclAlias       title =
  case stripPrefix (Pattern "(") title of
    Just rest ->
      case stripSuffix (Pattern ")") rest of
        -- An infix operator
        Just name -> { name, hashAnchor: "v" }
        -- Impossible
        Nothing -> { name: title, hashAnchor: "v" }
    Nothing ->
      case stripPrefix (Pattern "type (") title of
        Nothing ->
           { name: title, hashAnchor: "v" }
        -- An infix type operator
        Just rest ->
          case stripSuffix (Pattern ")") rest of
            Just name ->
              { name, hashAnchor: "t" }
            Nothing ->
              { name: title, hashAnchor: "v" }
extractHashAnchor DeclExternData  name = { name, hashAnchor: "t" }
extractHashAnchor DeclExternKind  name = { name, hashAnchor: "k" }


extractPackageName :: String -> String
extractPackageName name =
  let chunks = String.split (Pattern "/") name in
  fromMaybe "<unknown>" $
  chunks !! 0 >>= \dir ->
  -- TODO: is it safe to assume that directory name is ".spago"?
  if dir == ".spago" then
    chunks !! 1
  else
    Just "<local package>"

extractChildren
  :: { end :: Array Int
     , name :: String
     , start :: Array Int
     }
  -> String
  -> String
  -> Array ChildIndexEntry
  -> Array { path :: String
           , result :: SearchResult
           }
extractChildren sourceSpan' packageName moduleName =
  Array.concatMap $ unwrap >>>
  \ { title, comments, info, mbSourceSpan } ->

  let sourceSpan = fromMaybe sourceSpan' mbSourceSpan in
  if info.declType == ChildDeclInstance
  then mempty
  else
    -- `info.declType` is either `ChildDeclDataConstructor` or
    -- `ChildDeclTypeClassMember`.
    [ { path: title
      , result: SearchResult { name: title
                             , comments
                             , hashAnchor: "v"
                             , moduleName
                             , packageName
                             , sourceSpan }
      }
    ]
