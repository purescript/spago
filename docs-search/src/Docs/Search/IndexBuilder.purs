module Docs.Search.IndexBuilder where

import Docs.Search.BrowserEngine (getPartId)
import Docs.Search.Config as Config
import Docs.Search.Declarations (Declarations(..), mkDeclarations)
import Docs.Search.DocsJson (DocsJson)
import Docs.Search.Extra ((>#>))
import Docs.Search.Meta (Meta)
import Docs.Search.ModuleIndex (PackedModuleIndex, mkPackedModuleIndex)
import Docs.Search.ModuleParser (parseModuleName)
import Docs.Search.PackageIndex (PackageInfo, mkPackageInfo)
import Docs.Search.Score (mkScores)
import Docs.Search.SearchResult (SearchResult)
import Docs.Search.TypeIndex (TypeIndex, mkTypeIndex)
import Docs.Search.Types (ModuleName, PackageName, PartId)

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concat)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (sum)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Profunctor.Choice (left)
import Data.Search.Trie as Trie
import Data.Set as Set
import Data.String.CodePoints (contains) as String
import Data.String.CodeUnits (singleton) as String
import Data.String.Common (replace) as String
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (mkdir, readFile, readTextFile, readdir, stat, writeFile, writeTextFile)
import Node.FS.Stats (isDirectory, isFile)
import Node.FS.Sync (exists)
import Node.Process as Process
import Web.Bower.PackageMeta (PackageMeta(..))
import Web.Bower.PackageMeta as Bower
import Codec.Json.Unidirectional.Value as JsonCodec

type Config =
  { docsFiles :: Array String
  , bowerFiles :: Array String
  , generatedDocs :: String
  , noPatch :: Boolean
  , packageName :: PackageName
  , sourceFiles :: Array String
  }

run :: Config -> Effect Unit
run = launchAff_ <<< run'

run' :: Config -> Aff Unit
run' cfg = do

  checkDirectories cfg

  liftEffect do
    log "Building the search index..."

  docsJsons /\ moduleNames /\ packageMetas <- sequential $
    (\d h m -> d /\ h /\ m)
      <$> parallel (decodeDocsJsons cfg)
      <*> parallel (parseModuleHeaders cfg.sourceFiles)
      <*> parallel (decodeBowerJsons cfg)

  let
    countOfPackages = Array.length packageMetas
    countOfModules = Array.length docsJsons

  liftEffect do
    log $
      "Indexing "
        <> show countOfModules
        <> " modules from "
        <> show countOfPackages
        <>
          " packages..."

  let
    scores = mkScores packageMetas
    index = mkDeclarations scores docsJsons
    typeIndex = mkTypeIndex scores docsJsons
    packageInfo = mkPackageInfo scores packageMetas
    moduleIndex = mkPackedModuleIndex index moduleNames
    meta = { localPackageName: cfg.packageName }

  createDirectories cfg

  void $ sequential do
    ignore <$> parallel (writeIndex cfg index)
      <*> parallel (writeTypeIndex typeIndex)
      <*> parallel (writePackageInfo packageInfo)
      <*> parallel (writeModuleIndex moduleIndex)
      <*> parallel (writeMeta meta)
      <*> parallel (when (not cfg.noPatch) $ patchDocs cfg)
      <*> parallel (copyAppFile cfg)

  let
    countOfDefinitions = Trie.size $ unwrap index
    countOfTypeDefinitions =
      sum $ fromMaybe 0 <$> map Array.length <$> Map.values (unwrap typeIndex)

  liftEffect do
    log $
      "Added "
        <> show countOfDefinitions
        <> " definitions and "
        <> show countOfTypeDefinitions
        <> " type definitions from "
        <> show countOfPackages
        <>
          " packages to the search index."

  where
  ignore _ _ _ _ _ _ _ = unit

-- | Exit early if something is missing.
checkDirectories :: Config -> Aff Unit
checkDirectories cfg = do

  let
    dirs =
      [ cfg.generatedDocs
      , cfg.generatedDocs <> "/html"
      ]

  for_ dirs \dir -> do
    whenM (not <$> directoryExists dir) $
      liftEffect do
        logAndExit "Build the documentation first!"

-- | Read and decode given `docs.json` files.
decodeDocsJsons
  :: forall rest
   . { docsFiles :: Array String | rest }
  -> Aff (Array DocsJson)
decodeDocsJsons cfg@{ docsFiles } = do

  paths <- getPathsByGlobs docsFiles

  when (Array.null paths) do
    liftEffect do
      logAndExit $
        "The following globs do not match any files: " <> showGlobs cfg.docsFiles <>
          ".\nBuild the documentation first!"

  docsJsons <- Array.catMaybes <$> for paths \jsonFile -> do
    doesExist <- fileExists jsonFile

    if doesExist then do

      contents <- readTextFile UTF8 jsonFile
      let eiResult = jsonParser contents >>= left printJsonDecodeError <<< decodeJson

      case eiResult of
        Left error -> do
          liftEffect $ log $
            "\"docs.json\" decoding failed failed for " <> jsonFile <> ": " <> error
          pure Nothing
        Right result -> pure result

    else do
      liftEffect $ do
        log $
          "File does not exist: " <> jsonFile
      pure Nothing

  when (Array.null docsJsons) do
    liftEffect $ logAndExit $
      "Couldn't decode any of the files matched by the following globs: " <> showGlobs cfg.docsFiles

  pure docsJsons

-- | This function accepts an array of globs pointing to project sources
-- | and returns a list of module names extracted from these files.
-- | Unfortunately, we can't get all module names from `docs.json`s, because
-- | reexport-only modules do not have a single declaration in them, so we can't
-- | count them normally in `mkPackedModuleIndex.extract`, where we only look at
-- | exported declarations.
parseModuleHeaders :: Array String -> Aff (Array ModuleName)
parseModuleHeaders globs = do
  files <- getPathsByGlobs globs

  -- we are not checking if the globs match at least one file, because
  -- we want to support scenarios where there is no "current project"

  concat <$> for files \filePath -> do
    fileContents <- readTextFile UTF8 filePath
    case parseModuleName fileContents of
      Nothing -> do
        liftEffect $ log $
          "Module header decoding failed for " <> filePath <>
            ", unable to extract module name"
        pure []
      Just res -> pure [ res ]

decodeBowerJsons
  :: forall rest
   . { bowerFiles :: Array String | rest }
  -> Aff (Array PackageMeta)
decodeBowerJsons { bowerFiles } = do
  paths <- getPathsByGlobs bowerFiles

  when (Array.null paths) do
    liftEffect do
      logAndExit $
        "The following globs do not match any files: " <> showGlobs bowerFiles <>
          ".\nAre you in a project directory?"

  Array.nubBy compareNames
    <$> Array.catMaybes
    <$>
      for paths \jsonFileName ->
        join <$> withExisting jsonFileName
          \contents ->
            either (logError jsonFileName) pure
              (jsonParser contents >>= left JsonCodec.printDecodeError <<< decodePackageMeta)

  where
  decodePackageMeta :: Json -> Either JsonCodec.DecodeError (Maybe PackageMeta)
  decodePackageMeta = JsonCodec.toNullNothingOrJust Bower.toPackageMeta

  compareNames
    (PackageMeta { name: name1 })
    (PackageMeta { name: name2 }) = compare name1 name2

  logError fileName error = do
    liftEffect $ log $
      "\"bower.json\" decoding failed failed for " <> fileName <> ": " <> error
    pure Nothing

-- | Write type index parts to files.
writeTypeIndex :: TypeIndex -> Aff Unit
writeTypeIndex typeIndex =
  for_ entries \(Tuple typeShape results) -> do
    writeTextFile UTF8 (unwrap Config.typeIndexDirectory <> "/" <> typeShape <> ".js")
      (mkHeader typeShape <> stringify (encodeJson results))
  where
  mkHeader typeShape =
    "// This file was generated by purescript-docs-search\n"
      <> "window.DocsSearchTypeIndex[\""
      <> typeShape
      <> "\"] = "

  entries :: Array _
  entries = Map.toUnfoldableUnordered (unwrap typeIndex)

writePackageInfo :: PackageInfo -> Aff Unit
writePackageInfo packageInfo = do

  writeTextFile UTF8 (unwrap Config.packageInfoPath) $
    header <> stringify (encodeJson packageInfo)

  where
  header = "window.DocsSearchPackageIndex = "

writeModuleIndex :: PackedModuleIndex -> Aff Unit
writeModuleIndex moduleIndex = do
  writeTextFile UTF8 (unwrap Config.moduleIndexPath) $
    header <> stringify (encodeJson moduleIndex)
  where
  header = "window.DocsSearchModuleIndex = "

writeMeta :: Meta -> Aff Unit
writeMeta meta = do
  writeTextFile UTF8 (unwrap Config.metaPath) $
    header <> stringify (encodeJson meta)
  where
  header = "window." <> unwrap Config.metaItem <> " = "

-- | Get a mapping from index parts to index contents.
getIndex :: Declarations -> Map PartId (Array (Tuple String (Array SearchResult)))
getIndex (Declarations trie) =
  Array.foldr insert Map.empty parts
  where
  insert part = Map.insertWith append (getPartId part.prefix) part.results

  parts
    :: Array
         { prefix :: List Char
         , results :: Array (Tuple String (Array SearchResult))
         }
  parts = prefixes <#> \prefix ->
    let
      results =
        Array.fromFoldable $ toTuple <$>
          if List.length prefix == 2 then
            Trie.query prefix trie
          else
            -- Entries with path lengths > 1 have been added already.
            List.filter (\(Tuple path _value) -> List.length path == 1)
              ( Trie.query prefix trie
              )
    in
      { prefix, results }

  toTuple (Tuple path value) =
    Tuple (path >#> String.singleton) (Array.fromFoldable value)

  prefixes :: Array (List Char)
  prefixes =
    Set.toUnfoldable
      $ List.foldr (\path -> Set.insert (List.take 2 path)) mempty
      $
        fst <$> Trie.entriesUnordered trie

writeIndex :: Config -> Declarations -> Aff Unit
writeIndex { generatedDocs } = getIndex >>> \resultsMap -> do
  for_ (Map.toUnfoldableUnordered resultsMap :: Array _)
    \(Tuple indexPartId results) -> do
      let
        header =
          "// This file was generated by purescript-docs-search.\n"
            <> "window.DocsSearchIndex[\""
            <> show indexPartId
            <> "\"] = "

      writeTextFile UTF8 (generatedDocs <> Config.mkIndexPartPath indexPartId) $
        header <> stringify (encodeJson results)

patchHTML :: String -> Tuple Boolean String
patchHTML html =
  let
    pattern = Pattern "</body>"
    patch = "<!-- Docs search index. -->"
      <> "<script type=\"text/javascript\" src=\"./docs-search-app.js\"></script>"
      <> "<script type=\"text/javascript\">"
      <> "window.DocsSearchTypeIndex = {};"
      <> "window.DocsSearchIndex = {};"
      <> "</script>"
      <>
        "</body>"
  in
    if not $ String.contains (Pattern patch) html then Tuple true $ String.replace pattern (Replacement patch) html
    else Tuple false html

-- | Iterate through the HTML files generated by the PureScript compiler, and
-- | modify them using `patchHTML`.
patchDocs :: Config -> Aff Unit
patchDocs cfg = do
  let dirname = cfg.generatedDocs

  files <- readdir (dirname <> "html")

  for_ files \file -> do
    let path = dirname <> "html/" <> file

    whenM (fileExists path) do
      contents <- readTextFile UTF8 path
      case patchHTML contents of
        Tuple true patchedContents -> do
          writeTextFile UTF8 path patchedContents
        _ -> pure unit

-- | Create directories for two indices, or fail with a message
-- | in case the docs were not generated.
createDirectories :: Config -> Aff Unit
createDirectories { generatedDocs } = do
  let
    htmlDocs = generatedDocs <> "/html"
    indexDir = generatedDocs <> "/html/index"
    declIndexDir = generatedDocs <> "/html/index/declarations"
    typeIndexDir = generatedDocs <> "/html/index/types"

  whenM (not <$> directoryExists generatedDocs) $ liftEffect do
    logAndExit "Generate the documentation first!"

  whenM (not <$> directoryExists htmlDocs) $ liftEffect do
    logAndExit "Generate the documentation first!"

  whenM (not <$> directoryExists indexDir) do
    mkdir indexDir

  whenM (not <$> directoryExists declIndexDir) do
    mkdir declIndexDir

  whenM (not <$> directoryExists typeIndexDir) do
    mkdir typeIndexDir

-- | Copy the client-side application, responsible for handling user input and rendering
-- | the results, to the destination path.
copyAppFile :: Config -> Aff Unit
copyAppFile { generatedDocs } = do
  appDir <- liftEffect getDirname
  let appFile = appDir <> "/docs-search-app.js"
  whenM (not <$> fileExists appFile) do
    liftEffect do
      logAndExit $
        "Client-side app was not found at " <> appFile <> ".\n" <>
          "Check your installation."
  buffer <- readFile appFile
  writeFile (generatedDocs <> "/html/docs-search-app.js") buffer

directoryExists :: String -> Aff Boolean
directoryExists path = do
  doesExist <- liftEffect $ exists path
  case doesExist of
    false -> pure false
    true -> isDirectory <$> stat path

fileExists :: String -> Aff Boolean
fileExists path = do
  doesExist <- liftEffect $ exists path
  case doesExist of
    false -> pure false
    true -> isFile <$> stat path

withExisting :: forall a. String -> (String -> Aff a) -> Aff (Maybe a)
withExisting file f = do
  doesExist <- fileExists file

  if doesExist then do
    contents <- readTextFile UTF8 file
    res <- f contents
    pure $ Just res
  else do
    liftEffect $ do
      log $
        "File does not exist: " <> file
    pure Nothing

logAndExit :: forall a. String -> Effect a
logAndExit message = do
  log message
  Process.exit 1

showGlobs :: Array String -> String
showGlobs = Array.intercalate ", "

getPathsByGlobs :: Array String -> Aff (Array String)
getPathsByGlobs globs =
  liftEffect $ Array.concat <$> for globs glob

-- | Get __dirname.
foreign import getDirname :: Effect String

foreign import glob :: String -> Effect (Array String)
