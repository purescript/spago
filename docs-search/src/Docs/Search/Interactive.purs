-- | Definitions for the "search REPL".
module Docs.Search.Interactive where

import Prelude

import Docs.Search.Declarations (Declarations, mkDeclarations)
import Docs.Search.DocsJson (DataDeclType(..))
import Docs.Search.Engine (isValuableTypeQuery)
import Docs.Search.Engine as SearchEngine
import Docs.Search.Extra (homePageFromRepository)
import Docs.Search.IndexBuilder as IndexBuilder
import Docs.Search.SearchResult (ResultInfo(..), SearchResult(..))
import Docs.Search.Terminal (bold, cyan, green, yellow)
import Docs.Search.TypeDecoder (Constraint, FunDeps, Kind, QualifiedName, Type, TypeArgument)
import Docs.Search.TypeIndex (resultsWithTypes)
import Docs.Search.TypePrinter (keyword, showConstraint, showFunDeps, showKind, showType, showTypeArgument, space, syntax)
import Docs.Search.TypeQuery (parseTypeQuery)

import Data.Array as Array
import Data.Either (hush)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Search.Trie as Trie
import Data.String (length) as String
import Data.String.CodeUnits (fromCharArray, toCharArray) as String
import Data.String.Common (split, toLower, trim) as String
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.ReadLine (createConsoleInterface, question)

type Config =
  { docsFiles :: Array String
  , bowerFiles :: Array String
  }

run :: Config -> Effect Unit
run cfg = launchAff_ $ do

  liftEffect do
    log "Loading search index..."

  docsJsons    <- IndexBuilder.decodeDocsJsons cfg
  packageMetas <- IndexBuilder.decodeBowerJsons cfg

  let index =
        IndexBuilder.insertPackages packageMetas $
        mkDeclarations docsJsons
      typeIndex = docsJsons >>= resultsWithTypes

  let countOfDefinitions = Trie.size $ unwrap index
      countOfTypeDefinitions =
        Array.length typeIndex

  liftEffect do
    log $
      "Loaded " <>
      show countOfDefinitions <>
      " definitions and " <>
      show countOfTypeDefinitions <>
      " type definitions."

  liftEffect do
    let
      call handler interface = do
        question "> " (handler interface) interface

      inputHandler interface input = do
        let results =
              case hush (parseTypeQuery input) >>= isValuableTypeQuery of
                Nothing ->
                  Array.fromFoldable $
                  List.concat $
                  Trie.queryValues (List.fromFoldable $
                                    String.toCharArray $
                                    String.toLower $
                                    input) (unwrap index)

                Just typeQuery ->
                  Array.take 100 $
                  SearchEngine.sortByDistance typeQuery typeIndex

        let total = Array.length results

        consoleClear

        if total > 0 then do
          log $
            Array.intercalate "\n\n\n" $
            showResult <$> Array.reverse results
        else do
          log $
            "Your search for " <> bold input <> " did not yield any results."
        call inputHandler interface

    interface <- do
      interface <- createConsoleInterface (mkCompleter index)
      pure interface

    call inputHandler interface

mkCompleter
  :: Declarations
  -> String
  -> Effect { completions :: Array String
            , matched :: String }
mkCompleter index input = do
  let path = List.fromFoldable $ String.toCharArray input
  let paths =
        Array.fromFoldable $
        (String.fromCharArray <<< Array.fromFoldable) <$>
        (fst <$> Trie.query path (unwrap index))

  pure { completions: paths
       , matched: input }

showResult :: SearchResult -> String
showResult (SearchResult result@{ name, comments, moduleName, packageName }) =
  showSignature result <> "\n" <>

  (fromMaybe "\n" $
   comments <#> \comment ->
   "\n" <> leftShift 3 (String.trim comment) <> "\n\n") <>

  bold (cyan (rightPad 40 packageName)) <> space <> bold (green moduleName)
showResult (PackageResult { name, description, repository }) =
  cyan "package" <> " " <> yellow name <> "\n" <>
  (fromMaybe "\n" $
   description <#> \text ->
   "\n" <> leftShift 3 text <> "\n\n") <>
  leftShift 3 (homePageFromRepository repository)


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

foreign import consoleClear :: Effect Unit
