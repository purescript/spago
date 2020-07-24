-- | Definitions for the "search REPL".
module Docs.Search.Interactive where

import Prelude

import Data.Array as Array
import Data.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Newtype (un, unwrap, wrap)
import Data.Search.Trie as Trie
import Data.String (length) as String
import Data.String.Common (split, trim) as String
import Data.Tuple (fst)
import Docs.Search.Declarations (Declarations, mkDeclarations)
import Docs.Search.DocsJson (DataDeclType(..))
import Docs.Search.Engine (mkEngineState, Result(..))
import Docs.Search.Engine as Engine
import Docs.Search.Extra (listToString, stringToList, (>#>))
import Docs.Search.IndexBuilder as IndexBuilder
import Docs.Search.ModuleIndex (ModuleResult, mkPackedModuleIndex, unpackModuleIndex)
import Docs.Search.NodeEngine (nodeEngine)
import Docs.Search.PackageIndex (PackageResult, mkPackageIndex, mkPackageInfo)
import Docs.Search.Score (mkScores)
import Docs.Search.SearchResult (ResultInfo(..), SearchResult(..))
import Docs.Search.Terminal (bold, cyan, green, yellow)
import Docs.Search.TypeDecoder (Constraint, FunDeps, Kind, QualifiedName, Type, TypeArgument)
import Docs.Search.TypeIndex (resultsWithTypes)
import Docs.Search.TypePrinter (keyword, showConstraint, showFunDeps, showKind, showType, showTypeArgument, space, syntax)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log, clear) as Console
import Node.ReadLine (createConsoleInterface, question)


type Config =
  { docsFiles :: Array String
  , bowerFiles :: Array String
  }


run :: Config -> Effect Unit
run cfg = launchAff_ $ do

  liftEffect do
    Console.log "Loading search index..."

  docsJsons    <- IndexBuilder.decodeDocsJsons cfg
  packageMetas <- IndexBuilder.decodeBowerJsons cfg

  let scores       = mkScores packageMetas
      index        = mkDeclarations scores docsJsons
      typeIndex    = docsJsons >>= resultsWithTypes scores
      packageIndex = mkPackageIndex $ mkPackageInfo packageMetas
      moduleIndex  = unpackModuleIndex $ mkPackedModuleIndex index
      engineState  = mkEngineState (unwrap index) typeIndex packageIndex moduleIndex scores

  let countOfDefinitions     = Trie.size $ unwrap index
      countOfTypeDefinitions = Array.length typeIndex
      countOfPackages        = Array.length packageMetas

  liftEffect do
    Console.log $
      "Loaded " <>
      show countOfDefinitions <>
      " definitions and " <>
      show countOfTypeDefinitions <>
      " type definitions from " <>
      show countOfPackages <> " packages."


  liftEffect do
    let
      call handler interface = do
        question "> " (handler interface) interface

      inputHandler interface input = do

        let results =
              Engine.query nodeEngine engineState input <#> (_.results) # un Identity

        let total = Array.length results

        Console.clear

        Console.log $
          if total > 0 then do
            Array.intercalate "\n\n\n" $
              showResult <$> Array.reverse results
          else
            "Your search for " <> bold input <> " did not yield any results."

        call inputHandler interface

    interface <- createConsoleInterface (mkCompleter index)

    call inputHandler interface


mkCompleter
  :: Declarations
  -> String
  -> Effect { completions :: Array String
            , matched :: String }
mkCompleter index input = do
  let path = stringToList input
  let paths =
        Array.fromFoldable $
        listToString <$>
        (fst <$> Trie.query path (unwrap index))

  pure { completions: paths
       , matched: input }


showResult :: Result -> String
showResult = case _ of
  DeclResult r -> showSearchResult r
  TypeResult r -> showSearchResult r
  PackResult r -> showPackageResult r
  MdlResult r -> showModuleResult r


showSearchResult :: SearchResult -> String
showSearchResult (SearchResult result@{ name, comments, moduleName, packageName }) =
  showSignature result <> "\n" <>

  (fromMaybe "\n" $
   comments <#> \comment ->
   "\n" <> leftShift 3 (String.trim comment) <> "\n\n") <>

  bold (cyan (rightPad 40 packageName)) <> space <> bold (green moduleName)


showPackageResult :: PackageResult -> String
showPackageResult { name, description } =
  bold (cyan "package") <> " " <> bold (yellow name) <>

  (description >#> \text -> "\n\n" <> leftShift 3 text <> "\n")


showModuleResult :: ModuleResult -> String
showModuleResult { name, package } =
  bold (cyan "module") <> " " <> bold (green name)


showSignature ::
  forall rest.
  { name :: String
  , moduleName :: String
  , packageName :: String
  , info :: ResultInfo
  | rest
  }
  -> String
showSignature result@{ name, info } =
  case info of
    ValueResult { type: ty } ->
      yellow name <> syntax " :: " <> showType ty

    TypeClassResult info' ->
      showTypeClassSignature info' result

    TypeClassMemberResult info' ->
      showTypeClassMemberSignature info' result

    DataResult info' ->
      showDataSignature info' result

    TypeSynonymResult info' ->
      showTypeSynonymSignature info' result

    ExternDataResult info' ->
      showExternDataSignature info' result

    ValueAliasResult ->
      yellow ("(" <> name <> ")")

    _ -> yellow name


showTypeClassSignature
  :: forall rest
  .  { fundeps :: FunDeps
     , arguments :: Array TypeArgument
     , superclasses :: Array Constraint
     }
  -> { name :: String, moduleName :: String | rest }
  -> String
showTypeClassSignature { fundeps, arguments, superclasses } { name, moduleName } =

  keyword "class" <>
  ( if Array.null superclasses
    then
      ""
    else
      syntax " (" <> (
        Array.intercalate (syntax ", " ) (
          superclasses <#> showConstraint
        )
      ) <>
      syntax ")" <>
      space <>
      syntax "<="
  ) <>
  space <>
  yellow name <>
  space <> (
    Array.intercalate space $
      arguments <#> showTypeArgument
  ) <> (
    showFunDeps fundeps
  )


showTypeClassMemberSignature
  :: forall rest
  .  { type :: Type
     , typeClass :: QualifiedName
     , typeClassArguments :: Array TypeArgument
     }
  -> { name :: String | rest }
  -> String
showTypeClassMemberSignature { type: ty, typeClass, typeClassArguments } result =
  yellow result.name <>
  syntax " :: " <>
  showType ty


showDataSignature
  :: forall rest
  .  { typeArguments :: Array TypeArgument
     , dataDeclType :: DataDeclType }
  -> { name :: String | rest }
  -> String
showDataSignature { typeArguments, dataDeclType } { name } =
  ( keyword
    case dataDeclType of
      NewtypeDataDecl -> "newtype"
      DataDataDecl    -> "data"
  ) <>
  space <>
  yellow name <>
  space <> (
    Array.intercalate space $
      typeArguments <#> showTypeArgument
  )


showTypeSynonymSignature
  :: forall rest
  .  { type :: Type
     , arguments :: Array TypeArgument
     }
  -> { name :: String | rest }
  -> String
showTypeSynonymSignature { type: ty, arguments } { name } =
  keyword "type" <>
  space <>
  yellow name <>
  space <> (
    Array.intercalate space $
      arguments <#> showTypeArgument
  ) <>
  space <>
  syntax "=" <>
  space <>
  showType ty


showExternDataSignature
  :: forall rest
  .  { kind :: Kind }
  -> { name :: String | rest }
  -> String
showExternDataSignature { kind } { name } =
  keyword "foreign data" <>
  space <>
  yellow name <>
  space <>
  syntax " :: " <>
  showKind kind


leftShift :: Int -> String -> String
leftShift shift str =
  Array.intercalate "\n" $
  leftPad shift <$>
  String.trim <$>
  String.split (wrap "\n") str


leftPad :: Int -> String -> String
leftPad w str = Array.fold (Array.replicate w " ") <> str


rightPad :: Int -> String -> String
rightPad w str = str <> Array.fold (Array.replicate (w - String.length str) " ")
