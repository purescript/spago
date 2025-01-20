module Docs.Search.Declarations
  ( Declarations(..)
  , DeclLevel(..)
  , declLevelToHashAnchor
  , extractPackageName
  , mkDeclarations
  , resultsForDeclaration
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (foldl, foldr)
import Data.List (List, (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Search.Trie (Trie, alter)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (stripPrefix, stripSuffix, toCharArray)
import Data.String.Common (toLower)
import Data.String.Common as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Docs.Search.DocTypes (ChildDeclaration(..), ChildDeclarationInfo(..), Declaration(..), DeclarationInfo(..), DocModule(..), ModuleName(..), ProperName(..), QualifiedBy(..), Type')
import Docs.Search.Score (Scores, getPackageScore, getPackageScoreForPackageName)
import Docs.Search.SearchResult (ResultInfo(..), SearchResult(..))
import Docs.Search.TypeDecoder (Constraint(..), Qualified(..), Type(..), TypeArgument, TypeVarVisibility(..))
import Docs.Search.TypeQuery as TypeQuery
import Docs.Search.Types (PackageInfo(..), Identifier(..))
import Registry.PackageName (PackageName)
import Safe.Coerce (coerce)
import Spago.Purs.Types as Graph

newtype Declarations = Declarations (Trie Char (List SearchResult))

derive instance newtypeDeclarations :: Newtype Declarations _
derive newtype instance semigroupDeclarations :: Semigroup Declarations
derive newtype instance monoidDeclarations :: Monoid Declarations

mkDeclarations :: Graph.ModuleGraphWithPackage -> Set PackageName -> Scores -> Array DocModule -> Declarations
mkDeclarations moduleGraph workspacePackages scores = Declarations <<< foldr (insertDocModule scores) mempty
  where
  insertDocModule
    :: Scores
    -> DocModule
    -> Trie Char (List SearchResult)
    -> Trie Char (List SearchResult)

  insertDocModule _scores (DocModule { name, declarations }) trie =
    foldr (insertDeclaration moduleGraph workspacePackages scores name) trie declarations

insertDeclaration
  :: Graph.ModuleGraphWithPackage
  -> Set PackageName
  -> Scores
  -> ModuleName
  -> Declaration
  -> Trie Char (List SearchResult)
  -> Trie Char (List SearchResult)
insertDeclaration moduleGraph workspacePackages scores moduleName entry@(Declaration _) trie =
  foldr insertSearchResult trie (resultsForDeclaration moduleGraph workspacePackages scores moduleName entry)

insertSearchResult
  :: { path :: String
     , result :: SearchResult
     }
  -> Trie Char (List SearchResult)
  -> Trie Char (List SearchResult)
insertSearchResult { path, result } trie =
  let
    path' = List.fromFoldable $ toCharArray $ toLower path
  in
    alter path' (Just <<< updateResults) trie
  where
  updateResults mbOldResults
    | Just oldResults <- mbOldResults =
        result : oldResults
    | otherwise =
        List.singleton result

-- | For each declaration, extract its own `SearchResult` and `SearchResult`s
-- | corresponding to its children (e.g. a class declaration contains class members).
resultsForDeclaration
  :: Graph.ModuleGraphWithPackage
  -> Set PackageName
  -> Scores
  -> ModuleName
  -> Declaration
  -> List
       { path :: String
       , result :: SearchResult
       }
resultsForDeclaration moduleGraph workspacePackages scores moduleName indexEntry@(Declaration entry) =
  case mkInfo declLevel indexEntry of
    Nothing -> mempty
    Just info' ->
      let
        result = SearchResult
          { name: Identifier title
          , comments
          , hashAnchor: declLevelToHashAnchor declLevel
          , moduleName
          , sourceSpan
          , packageInfo
          , score:
              fromMaybe zero $ getPackageScoreForPackageName scores <$> mbPackageName
          , info: info'
          }
      in
        ( List.singleton $
            { path: name
            , result
            }
        ) <>
          ( List.fromFoldable children >>=
              resultsForChildDeclaration scores packageInfo moduleName result
          )
  where
  { title, sourceSpan, comments, children } = entry
  { name, declLevel } = getLevelAndName indexEntry
  packageInfo = extractPackageName moduleGraph workspacePackages moduleName
  mbPackageName =
    case packageInfo of
      Package packageName -> Just packageName
      LocalPackage packageName -> Just packageName
      _ -> Nothing

mkInfo :: DeclLevel -> Declaration -> Maybe ResultInfo
mkInfo declLevel (Declaration { info, title: _ }) =
  case info of
    ValueDeclaration ty ->
      Just $ ValueResult { type: ty }

    DataDeclaration dataDeclType typeArguments _ ->
      Just $ DataResult
        { dataDeclType
        , typeArguments: typeArguments <#> toTypeArgument
        }

    ExternDataDeclaration kind _ ->
      Just $ ExternDataResult { kind }

    TypeSynonymDeclaration arguments ty ->
      Just $ TypeSynonymResult
        { type: ty
        , arguments: arguments <#> toTypeArgument
        }

    TypeClassDeclaration arguments superclasses fundeps ->
      Just $ TypeClassResult
        { fundeps
        , arguments: arguments <#> toTypeArgument
        , superclasses
        }

    AliasDeclaration _ _ ->
      case declLevel of
        TypeLevel -> Just TypeAliasResult
        ValueLevel -> Just ValueAliasResult

  where
  toTypeArgument :: Tuple _ _ -> TypeArgument
  toTypeArgument (Tuple name kind) = { name, kind }

-- | Level of a declaration, used to determine which URI hash anchor to use in
-- | links ("v" or "t" ).
data DeclLevel = ValueLevel | TypeLevel

declLevelToHashAnchor :: DeclLevel -> String
declLevelToHashAnchor = case _ of
  ValueLevel -> "v"
  TypeLevel -> "t"

getLevelAndName
  :: Declaration
  -> { declLevel :: DeclLevel
     , name :: String
     }
getLevelAndName (Declaration { info, title }) =
  case info of
    ValueDeclaration _ -> { name: title, declLevel: ValueLevel }
    DataDeclaration _ _ _ -> { name: title, declLevel: TypeLevel }
    TypeSynonymDeclaration _ _ -> { name: title, declLevel: TypeLevel }
    TypeClassDeclaration _ _ _ -> { name: title, declLevel: TypeLevel }
    AliasDeclaration _ _ ->
      -- "declType": "alias" does not specify the level of the declaration.
      -- But for type aliases, name of the declaration is always wrapped into
      -- "type (" and ")".
      let
        withAnchor declLevel name = { declLevel, name }
      in
        fromMaybe (withAnchor ValueLevel title) $
          ( withAnchor ValueLevel <$>
              ( stripPrefix (Pattern "(") >=>
                  stripSuffix (Pattern ")")
              ) title
          ) <|>
            ( withAnchor TypeLevel <$>
                ( stripPrefix (Pattern "type (") >=>
                    stripSuffix (Pattern ")")
                ) title
            )

    ExternDataDeclaration _ _kind ->
      { name: title, declLevel: TypeLevel }

-- | Extract package name from `sourceSpan.name`, which contains path to
-- | the source file. If `ModuleName` string starts with `Prim.`, it's a
-- | built-in (guaranteed by the compiler).
extractPackageName :: Graph.ModuleGraphWithPackage -> Set PackageName -> ModuleName -> PackageInfo
extractPackageName moduleGraph workspacePackages (ModuleName moduleName) =
  case Array.index (String.split (Pattern ".") moduleName) 0 == Just "Prim" of
    true -> Builtin
    false -> case Map.lookup moduleName moduleGraph of
      Nothing -> UnknownPackage
      Just { package } -> case Set.member package workspacePackages of
        true -> LocalPackage package
        false -> Package package

-- | Extract `SearchResults` from a `ChildDeclaration`.
resultsForChildDeclaration
  :: Scores
  -> PackageInfo
  -> ModuleName
  -> SearchResult
  -> ChildDeclaration
  -> List { path :: String, result :: SearchResult }
resultsForChildDeclaration
  scores
  packageInfo
  moduleName
  parentResult
  child@(ChildDeclaration { title, info: _, comments, sourceSpan })
  | Just resultInfo <- mkChildInfo parentResult child =
      { path: title
      , result: SearchResult
          { name: Identifier title
          , comments
          -- `ChildDeclaration`s are always either data
          -- constructors, type class members or instances.
          -- The former two are both value-level, and
          -- the latter are not included in the index.
          , hashAnchor: "v"
          , moduleName
          , sourceSpan
          , packageInfo
          , score: getPackageScore scores packageInfo
          , info: resultInfo
          }
      } # List.singleton
  | otherwise = mempty

mkChildInfo
  :: SearchResult
  -> ChildDeclaration
  -> Maybe ResultInfo
mkChildInfo
  (SearchResult { info: parentInfo, moduleName, name: resultName })
  (ChildDeclaration { info })

  | ChildDataConstructor childTypeArguments <- info
  , DataResult { dataDeclType, typeArguments } <- parentInfo =
      let
        parentTypeCtor :: Type'
        parentTypeCtor =
          TypeConstructor unit $
            Qualified
              (ByModuleName moduleName)
              (coerce $ resultName)

        parentTypeArgs :: Array Type'
        parentTypeArgs = typeArguments <#> \{ name } -> TypeVar unit name

        parentType :: Type'
        parentType = foldl (TypeApp unit) parentTypeCtor parentTypeArgs

        typeArrow :: Type' -> Type'
        typeArrow =
          TypeApp
            unit
            ( TypeConstructor
                unit
                ( Qualified
                    (ByModuleName (ModuleName "Prim"))
                    (ProperName "Function")
                )
            )

        makeType :: Array Type' -> Type'
        makeType = foldr (\a b -> TypeApp unit (typeArrow a) b) parentType
      in
        Just $ DataConstructorResult
          { dataDeclType
          , "type": makeType childTypeArguments
          }
  | ChildTypeClassMember unconstrainedType <- info
  , TypeClassResult { arguments } <- parentInfo =
      -- We need to reconstruct a "real" type of a type class member.
      -- For example, if `unconstrainedType` is the type of `pure`, i.e.
      -- `forall a. a -> m a`, then `restoredType` should be:
      -- `forall m a. Control.Applicative.Applicative m => a -> m a`.

      let
        -- First, we get a list of nested `forall` quantifiers for
        -- `unconstrainedType`  and a version of `unconstrainedType` without
        -- them (`ty`).
        ({ ty, binders }) = TypeQuery.joinForAlls unconstrainedType

        -- Then we construct a qualified name of the type class.
        constraintClass =
          Qualified
            (ByModuleName moduleName)
            (coerce resultName)

        -- We concatenate two lists:
        -- * a list of type parameters of the type class, and
        -- * a list of quantified variables of the unconstrained type
        allArguments :: Array TypeArgument
        allArguments =
          arguments <> List.toUnfoldable binders

        restoreType :: Type' -> Type'
        restoreType =
          foldr
            ( \({ name, kind }) -> compose
                \ty -> ForAll unit TypeVarInvisible name kind ty Nothing
            )
            identity
            allArguments

        -- Finally, we have a restored type. It allows us to search for
        -- type members the same way we search for functions. And types
        -- of class member results appear with the correct
        -- class constraints.
        restoredType =
          restoreType $
            ConstrainedType
              unit
              ( Constraint
                  { ann: unit
                  , args: toTypeVars arguments
                  , class: constraintClass
                  , data: Nothing
                  , kindArgs: []
                  }
              )
              ty

      in
        Just $ TypeClassMemberResult
          { type: restoredType
          , typeClass: constraintClass
          , typeClassArguments: arguments
          }
  | otherwise = Nothing

toTypeVars :: Array TypeArgument -> Array Type'
toTypeVars = map \{ name } -> TypeVar unit name
