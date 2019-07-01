module Spago.Search.Declarations where

import Prelude

import Spago.Search.SearchResult (ResultInfo(..), SearchResult(..))
import Spago.Search.DocsJson (ChildDeclType(..), ChildDeclaration(..), DeclType(..), Declaration(..), DocsJson(..))
import Spago.Search.TypeDecoder (Constraint(..), QualifiedName(..), Type(..), joinForAlls)

import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.Foldable (foldr)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Search.Trie (Trie, alter)
import Data.String.CodeUnits (stripPrefix, stripSuffix, toCharArray)
import Data.String.Common (toLower)
import Data.String.Common (split) as String
import Data.String.Pattern (Pattern(..))

type ModuleName = String
type PackageName = String

newtype Declarations
  = Declarations (Trie Char (List SearchResult))

derive instance newtypeDeclarations :: Newtype Declarations _
derive newtype instance semigroupDeclarations :: Semigroup Declarations
derive newtype instance monoidDeclarations :: Monoid Declarations

mkDeclarations :: Array DocsJson -> Declarations
mkDeclarations docsJson = Declarations trie
  where
    trie = foldr insertDocsJson mempty docsJson

insertDocsJson
  :: DocsJson
  -> Trie Char (List SearchResult)
  -> Trie Char (List SearchResult)
insertDocsJson (DocsJson { name, declarations }) trie
  = foldr (insertDeclaration name) trie declarations

insertDeclaration
  :: ModuleName
  -> Declaration
  -> Trie Char (List SearchResult)
  -> Trie Char (List SearchResult)
insertDeclaration moduleName entry@(Declaration { title }) trie
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
  :: ModuleName
  -> Declaration
  -> List { path :: String
          , result :: SearchResult
          }
resultsForEntry moduleName indexEntry@(Declaration entry) =
  let { info, title, sourceSpan, comments, children } = entry
      { name, declLevel } = getLevelAndName info.declType title
      packageName = extractPackageName sourceSpan.name
  in case mkInfo declLevel indexEntry of
       Nothing -> mempty
       Just info' ->
         let result = SearchResult { name: title
                                   , comments
                                   , hashAnchor: declLevelToHashAnchor declLevel
                                   , moduleName
                                   , sourceSpan: Just sourceSpan
                                   , packageName
                                   , info: info'
                                   }
         in
           ( List.singleton $
               { path: name
               , result
               }
           ) <>
           ( List.fromFoldable children >>=
             resultsForChildDeclaration packageName moduleName result
           )

mkInfo :: DeclLevel -> Declaration -> Maybe ResultInfo
mkInfo declLevel (Declaration { info, title }) =
  case info.declType of

    DeclValue ->
      info.type <#>
      \ty -> ValueResult { type: ty }

    DeclData ->
       make <$> info.typeArguments <*> info.dataDeclType
        where
          make typeArguments dataDeclType =
            DataResult { typeArguments, dataDeclType }

    DeclExternData ->
      info.kind <#>
      \kind -> ExternDataResult { kind }

    DeclTypeSynonym ->
      make <$> info.type <*> info.arguments
        where
          make ty args = TypeSynonymResult { type: ty, arguments: args }

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
getLevelAndName DeclTypeClass   name = { name, declLevel: TypeLevel }
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

resultsForChildDeclaration
  :: PackageName
  -> ModuleName
  -> SearchResult
  -> ChildDeclaration
  -> List { path :: String, result :: SearchResult }
resultsForChildDeclaration packageName moduleName parentResult
  child@(ChildDeclaration { title, info, comments, mbSourceSpan }) =
    case mkChildInfo parentResult child of
      Nothing -> mempty
      Just resultInfo ->
        { path: title
        , result: SearchResult { name: title
                               , comments
                                 -- `ChildDeclaration`s are always either data
                                 -- constructors, type class members or instances.
                                 -- The former two are both value-level, and
                                 -- the latter are not included in the index.
                               , hashAnchor: "v"
                               , moduleName
                               , sourceSpan: mbSourceSpan
                               , packageName
                               , info: resultInfo
                               }
        } # List.singleton

mkChildInfo
  :: SearchResult
  -> ChildDeclaration
  -> Maybe ResultInfo
mkChildInfo parentResult (ChildDeclaration { info } ) =
  case info.declType of
    ChildDeclDataConstructor ->
      info.arguments <#>
      \arguments -> DataConstructorResult { arguments }
    ChildDeclTypeClassMember ->
      case (unwrap parentResult).info of
        TypeClassResult { arguments } ->
          -- We need to reconstruct a "real" type of a type class member.
          -- For example, if `unconstrainedType` is the type of `pure`, i.e. `forall a. a -> m a`,
          -- `restoredType` should be `forall m a. Control.Applicative.Applicative m => a -> m a`.
          info.type <#>
            \(unconstrainedType :: Type) ->
            let
              -- First, we get a list of nested `forall` quantifiers for `unconstrainedType`
              -- and a version of `unconstrainedType` without them (`ty`).
              { ty, binders } = joinForAlls unconstrainedType

              -- Then we construct a qualified name of the type class.
              constraintClass =
                QualifiedName { moduleName:
                                String.split (wrap ".")
                                (unwrap parentResult).moduleName
                              , name: (unwrap parentResult).name }

              typeClassArguments = arguments <#> unwrap >>> _.name

              -- We concatenate two lists:
              -- * list of type parameters of the type class, and
              -- * list of quantified variables of the unconstrained type
              allArguments :: Array String
              allArguments =
                typeClassArguments <> (List.toUnfoldable binders <#> (_.var))

              restoreType :: Type -> Type
              restoreType =
                foldr
                  (\arg -> compose (\type'' -> ForAll arg type'' Nothing))
                  identity allArguments

              restoredType = restoreType $
                ConstrainedType (Constraint { constraintClass
                                            , constraintArgs: typeClassArguments <#> TypeVar
                                            }) ty

            in TypeClassMemberResult
               { type: restoredType
               , typeClass: constraintClass
               , typeClassArguments
               }

        _ -> Nothing
    ChildDeclInstance -> Nothing
