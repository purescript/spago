module Docs.Search.Declarations where

import Docs.Search.DocsJson (ChildDeclType(..), ChildDeclaration(..), DeclType(..), Declaration(..), DocsJson(..), SourceSpan)
import Docs.Search.Score (Scores, getPackageScore, getPackageScoreForPackageName)
import Docs.Search.SearchResult (ResultInfo(..), SearchResult(..))
import Docs.Search.TypeDecoder (Constraint(..), QualifiedName(..), Type(..), joinForAlls)
import Docs.Search.Types (ModuleName(..), PackageName(..), PackageInfo(..), Identifier(..))

import Prelude
import Prim hiding (Type)
import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (foldl, foldr)
import Data.List (List, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Search.Trie (Trie, alter)
import Data.String.CodeUnits (stripPrefix, stripSuffix, toCharArray)
import Data.String.Common (split) as String
import Data.String.Common (toLower)
import Data.String.Pattern (Pattern(..))


newtype Declarations = Declarations (Trie Char (List SearchResult))

derive instance newtypeDeclarations :: Newtype Declarations _
derive newtype instance semigroupDeclarations :: Semigroup Declarations
derive newtype instance monoidDeclarations :: Monoid Declarations


mkDeclarations :: Scores -> Array DocsJson -> Declarations
mkDeclarations scores = Declarations <<< foldr (insertDocsJson scores) mempty


insertDocsJson
  :: Scores
  -> DocsJson
  -> Trie Char (List SearchResult)
  -> Trie Char (List SearchResult)
insertDocsJson scores (DocsJson { name, declarations }) trie
  = foldr (insertDeclaration scores $ ModuleName name) trie declarations


insertDeclaration
  :: Scores
  -> ModuleName
  -> Declaration
  -> Trie Char (List SearchResult)
  -> Trie Char (List SearchResult)
insertDeclaration scores moduleName entry@(Declaration { title }) trie
  = foldr insertSearchResult trie (resultsForDeclaration scores moduleName entry)


insertSearchResult
  :: { path :: String
     , result :: SearchResult
     }
  -> Trie Char (List SearchResult)
  -> Trie Char (List SearchResult)
insertSearchResult { path, result } trie =
  let path' = List.fromFoldable $ toCharArray $ toLower path in
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
  :: Scores
  -> ModuleName
  -> Declaration
  -> List { path :: String
          , result :: SearchResult
          }
resultsForDeclaration scores moduleName indexEntry@(Declaration entry) =
  case mkInfo declLevel indexEntry of
    Nothing -> mempty
    Just info' ->
         let result = SearchResult { name: Identifier title
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
    { info, title, sourceSpan, comments, children } = entry
    { name, declLevel } = getLevelAndName info.declType title
    packageInfo = extractPackageName moduleName sourceSpan
    mbPackageName =
      case packageInfo of
        Package packageName -> Just packageName
        _                   -> Nothing


mkInfo :: DeclLevel -> Declaration -> Maybe ResultInfo
mkInfo declLevel (Declaration { info, title }) =
  case info.declType of

    DeclValue ->
      info."type" <#> \ty -> ValueResult { type: ty }

    DeclData ->
      make <$> info.typeArguments <*> info.dataDeclType
      where
        make typeArguments dataDeclType =
          DataResult { typeArguments, dataDeclType }

    DeclExternData ->
      info.kind <#> \kind -> ExternDataResult { kind }

    DeclTypeSynonym ->
      make <$> info."type" <*> info.arguments
        where
          make ty args = TypeSynonymResult { "type": ty, arguments: args }

    DeclTypeClass
      | Just fundeps      <- info.fundeps
      , Just arguments    <- info.arguments
      , Just superclasses <- info.superclasses ->
        Just $ TypeClassResult { fundeps, arguments, superclasses }
      | otherwise -> Nothing

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
-- | the source file. If `ModuleName` string starts with `Prim.`, it's a
-- | built-in (guaranteed by the compiler).
extractPackageName :: ModuleName -> Maybe SourceSpan -> PackageInfo
extractPackageName (ModuleName moduleName) _
  | String.split (Pattern ".") moduleName !! 0 == Just "Prim" = Builtin
extractPackageName _ Nothing = UnknownPackage
extractPackageName _ (Just { name }) =
  fromMaybe LocalPackage do
    topLevelDir <- dirs !! 0
    if topLevelDir == ".spago"
    then Package <<< PackageName <$> dirs !! 1
    else do
      bowerDirIx <- Array.findIndex (_ == "bower_components") dirs
      Package <<< PackageName <$> dirs !! (bowerDirIx + 1)
  where dirs = String.split (Pattern "/") name


-- | Extract `SearchResults` from a `ChildDeclaration`.
resultsForChildDeclaration
  :: Scores
  -> PackageInfo
  -> ModuleName
  -> SearchResult
  -> ChildDeclaration
  -> List { path :: String, result :: SearchResult }
resultsForChildDeclaration scores packageInfo moduleName parentResult
  child@(ChildDeclaration { title, info, comments, mbSourceSpan })
  | Just resultInfo <- mkChildInfo parentResult child =
        { path: title
        , result: SearchResult { name: Identifier title
                               , comments
                                 -- `ChildDeclaration`s are always either data
                                 -- constructors, type class members or instances.
                                 -- The former two are both value-level, and
                                 -- the latter are not included in the index.
                               , hashAnchor: "v"
                               , moduleName
                               , sourceSpan: mbSourceSpan
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
  (ChildDeclaration { info } )

  | ChildDeclDataConstructor <- info.declType
  , DataResult { dataDeclType, typeArguments } <- parentInfo =
      let
        parentTypeCtor :: Type
        parentTypeCtor = TypeConstructor
          $ QualifiedName { moduleNameParts:
                            String.split (wrap ".") (unwrap moduleName)
                          , name: resultName }

        parentTypeArgs :: Array Type
        parentTypeArgs = typeArguments <#> unwrap >>> \{ name } -> TypeVar name

        parentType :: Type
        parentType = foldl TypeApp parentTypeCtor parentTypeArgs

        typeArrow :: Type -> Type
        typeArrow =
          TypeApp (TypeConstructor (QualifiedName { moduleNameParts: [ "Prim" ]
                                                  , name: Identifier "Function" }))

        makeType :: Array Type -> Type
        makeType = foldr (\a b -> TypeApp (typeArrow a) b) parentType
      in
       info.arguments <#> \arguments ->
         DataConstructorResult { dataDeclType
                               , "type": makeType arguments
                               }

  | ChildDeclTypeClassMember <- info.declType
  , TypeClassResult { arguments } <- parentInfo =
    -- We need to reconstruct a "real" type of a type class member.
    -- For example, if `unconstrainedType` is the type of `pure`, i.e.
    -- `forall a. a -> m a`, then `restoredType` should be:
    -- `forall m a. Control.Applicative.Applicative m => a -> m a`.

    info."type" <#>
    \(unconstrainedType :: Type) ->
      let
        -- First, we get a list of nested `forall` quantifiers for
        -- `unconstrainedType`  and a version of `unconstrainedType` without
        -- them (`ty`).
        ({ ty, binders }) = joinForAlls unconstrainedType

        -- Then we construct a qualified name of the type class.
        constraintClass =
          QualifiedName { moduleNameParts:
                          String.split (wrap ".") $ unwrap moduleName
                        , name: resultName }

        -- We concatenate two lists:
        -- * a list of type parameters of the type class, and
        -- * a list of quantified variables of the unconstrained type
        allArguments :: Array { name :: String, mbKind :: Maybe Type }
        allArguments =
          (arguments <#> unwrap) <> List.toUnfoldable binders

        restoreType :: Type -> Type
        restoreType =
          foldr
            (\({ name, mbKind }) -> compose (\type'' -> ForAll name mbKind type''))
            identity
            allArguments

        -- Finally, we have a restored type. It allows us to search for
        -- type members the same way we search for functions. And types
        -- of class member results appear with the correct
        -- class constraints.
        restoredType =
          restoreType $
          ConstrainedType
          (Constraint { constraintClass
                      , constraintArgs: arguments <#> unwrap >>> (_.name) >>> TypeVar
                      }) ty

      in TypeClassMemberResult
         { type: restoredType
         , typeClass: constraintClass
         , typeClassArguments: arguments
         }

  | otherwise = Nothing
