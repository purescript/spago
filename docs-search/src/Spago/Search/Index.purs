module Spago.Search.Index where

import Prelude

import Spago.Search.TypeParser (Constraint, FunDeps, Kind, Type, TypeArgument)
import Spago.Search.Declarations (ChildDeclType(..), ChildIndexEntry(..), DeclType(..), Declarations(..), IndexEntry(..))

import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.Foldable (foldr)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Search.Trie (Trie, alter)
import Data.String.CodeUnits (stripPrefix, stripSuffix, toCharArray)
import Data.String.Common (toLower)
import Data.String.Common as String
import Data.String.Pattern (Pattern(..))

newtype SearchIndex = SearchIndex (Trie Char (List SearchResult))

derive instance newtypeSearchIndex :: Newtype SearchIndex _

data ResultInfo
  = DataResult            { typeArguments :: Array TypeArgument }
  | ExternDataResult      { kind :: Kind }
  | TypeSynonymResult     { type :: Type }
  | DataConstructorResult { arguments :: Array Type }
  | TypeClassMemberResult { type :: Type }
  | TypeClassResult       { fundeps :: FunDeps
                          , arguments :: Array TypeArgument
                          , superclasses :: Array Constraint }
  | ValueResult           { type :: Type }
  | ValueAliasResult
  | TypeAliasResult
  | ExternKindResult

newtype SearchResult
  = SearchResult { name :: String
                 , comments :: Maybe String
                 , hashAnchor :: String
                 , moduleName :: String
                 , packageName :: String
                 , sourceSpan :: Maybe { start :: Array Int
                                       , end :: Array Int
                                       , name :: String
                                       }
                 , info :: ResultInfo
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
resultsForEntry moduleName ie@(IndexEntry entry@{info, title, sourceSpan, comments, children}) =
  let { name, declLevel } = getLevelAndName info.declType title
      packageName = extractPackageName sourceSpan.name
  in case mkInfo declLevel ie of
       Nothing -> mempty
       Just info' -> (
         List.singleton $
         { path: name
         , result: SearchResult { name: title
                                , comments
                                , hashAnchor: declLevelToHashAnchor declLevel
                                , moduleName
                                , sourceSpan: Just sourceSpan
                                , packageName
                                , info: info'
                                }
         }
         ) <>
         ( List.fromFoldable children >>=
           resultsForChildIndexEntry packageName moduleName
         )

mkInfo :: DeclLevel -> IndexEntry -> Maybe ResultInfo
mkInfo declLevel (IndexEntry { info, title }) =
  case info.declType of

    DeclValue ->
      info.type <#>
      \ty -> ValueResult { type: ty }

    DeclData ->
      info.typeArguments <#>
      \typeArguments -> DataResult { typeArguments }

    DeclExternData ->
      info.kind <#>
      \kind -> ExternDataResult { kind }

    DeclTypeSynonym ->
      info.type <#>
      \ty -> TypeSynonymResult { type: ty }

    DeclTypeClass ->
      case info.fundeps, info.arguments, info.superclasses of
        Just fundeps, Just arguments, Just superclasses ->
          Just $ TypeClassResult { fundeps, arguments, superclasses }
        _, _, _ -> Nothing

    DeclAlias ->
      case declLevel of
        TypeLevel  -> Just TypeAliasResult
        ValueLevel -> Just ValueAliasResult
        _          -> Nothing

    DeclExternKind ->
      Just ExternKindResult

-- | Level of a declaration, used to determine which URI hash anchor to use in
-- | links ("v", "t" or "k").
data DeclLevel = ValueLevel | TypeLevel | KindLevel

declLevelToHashAnchor :: DeclLevel -> String
declLevelToHashAnchor = case _ of
  ValueLevel -> "v"
  TypeLevel  -> "t"
  KindLevel  -> "k"

getLevelAndName
  :: DeclType
  -> String
  -> { declLevel :: DeclLevel
     , name :: String
     }
getLevelAndName DeclValue       name = { name, declLevel: ValueLevel }
getLevelAndName DeclData        name = { name, declLevel: TypeLevel }
getLevelAndName DeclTypeSynonym name = { name, declLevel: TypeLevel }
getLevelAndName DeclTypeClass   name = { name, declLevel: ValueLevel }
-- "declType": "alias" does not specify the level of the declaration.
-- But for type aliases, name of the declaration is always wrapped into
-- "type (" and ")".
getLevelAndName DeclAlias       title =
  fromMaybe (withAnchor ValueLevel title) $
  (withAnchor ValueLevel <$>
   (stripPrefix (Pattern "(") >=>
    stripSuffix (Pattern ")")) title) <|>
  (withAnchor TypeLevel <$>
    (stripPrefix (Pattern "type (") >=>
     stripSuffix (Pattern ")")) title)
  where
    withAnchor declLevel name = { declLevel, name }
getLevelAndName DeclExternData  name = { name, declLevel: TypeLevel }
getLevelAndName DeclExternKind  name = { name, declLevel: KindLevel }

-- | Extract package name from `sourceSpan.name`, which contains path to
-- | the source file.
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

resultsForChildIndexEntry
  :: String
  -> String
  -> ChildIndexEntry
  -> List { path :: String, result :: SearchResult }
resultsForChildIndexEntry packageName moduleName
  cie@(ChildIndexEntry { title, info, comments, mbSourceSpan }) =
    case mkChildInfo cie of
      Nothing -> mempty
      Just resultInfo ->
        { path: title
        , result: SearchResult { name: title
                               , comments
                               , hashAnchor: "v"
                               , moduleName
                               , sourceSpan: mbSourceSpan
                               , packageName
                               , info: resultInfo
                               }
        } # List.singleton

mkChildInfo :: ChildIndexEntry -> Maybe ResultInfo
mkChildInfo (ChildIndexEntry { info } ) =
  case info.declType of
    ChildDeclDataConstructor ->
      info.arguments <#>
      \arguments -> DataConstructorResult { arguments }
    ChildDeclTypeClassMember ->
      info.type <#>
      \ty -> TypeClassMemberResult { type: ty }
    ChildDeclInstance -> Nothing
