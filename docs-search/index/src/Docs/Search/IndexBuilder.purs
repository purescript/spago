module Docs.Search.IndexBuilder
  ( run
  , patchHtml
  , getPathsByGlobs
  ) where

import Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Codec.Json.Unidirectional.Value as CJ.Codec
import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Either (Either(..), either)
import Data.Foldable (sum)
import Data.Foldable as Fold
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Profunctor.Choice (left)
import Data.Search.Trie as Trie
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodePoints (contains) as String
import Data.String.CodeUnits (singleton) as String
import Data.String.Common (replace) as String
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Docs.Search.Config as Config
import Docs.Search.Declarations (Declarations(..))
import Docs.Search.Declarations as Declarations
import Docs.Search.DocTypes (DocModule)
import Docs.Search.DocTypes as Docs
import Docs.Search.Extra ((>#>))
import Docs.Search.Meta (Meta)
import Docs.Search.Meta as Meta
import Docs.Search.ModuleIndex (PackedModuleIndex)
import Docs.Search.ModuleIndex as ModuleIndex
import Docs.Search.PackageIndex (PackageInfo)
import Docs.Search.PackageIndex as PackageIndex
import Docs.Search.Score (mkScores)
import Docs.Search.SearchResult (SearchResult)
import Docs.Search.SearchResult as SearchResult
import Docs.Search.TypeIndex (TypeIndex)
import Docs.Search.TypeIndex as TypeIndex
import Docs.Search.Types (PartId)
import Effect (Effect)
import Effect.Aff (Aff, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Console (log)
import JSON (JSON)
import JSON as JSON
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (mkdir, readFile, readTextFile, readdir, stat, writeFile, writeTextFile)
import Node.FS.Stats (isDirectory, isFile)
import Node.FS.Sync (exists)
import Node.Path as Path
import Node.Process as Process
import Registry.Manifest (Manifest(..))
import Registry.Manifest as Manifest
import Registry.PackageName (PackageName)
import Spago.Purs.Types as Graph
import Unsafe.Coerce (unsafeCoerce)

type Config =
  { docsFiles :: Array String
  , pursJsonFiles :: Array String
  , generatedDocs :: String
  , workspacePackages :: Set PackageName
  , moduleGraph :: Graph.ModuleGraphWithPackage
  }

run :: Config -> Aff Unit
run cfg = do

  checkDirectories cfg

  liftEffect do
    log "Building the search index..."

  docsJsons /\ packageMetas <- sequential $
    Tuple
      <$> parallel (decodeDocsJsons cfg)
      <*> parallel (decodePursJsons cfg)

  let
    countOfPackages = Array.length packageMetas
    countOfModules = Array.length docsJsons

  liftEffect do
    log $
      "Indexing "
        <> show countOfModules
        <> " modules from "
        <> show countOfPackages
        <> " packages..."

  let
    scores = mkScores packageMetas
    index = Declarations.mkDeclarations cfg.moduleGraph cfg.workspacePackages scores docsJsons
    typeIndex = TypeIndex.mkTypeIndex cfg.moduleGraph cfg.workspacePackages scores docsJsons
    packageInfo = PackageIndex.mkPackageInfo scores packageMetas
    moduleIndex = ModuleIndex.mkPackedModuleIndex cfg.moduleGraph cfg.workspacePackages index
    meta = {}

  createDirectories cfg

  void $ sequential do
    ignore <$> parallel (writeIndex cfg index)
      <*> parallel (writeTypeIndex typeIndex)
      <*> parallel (writePackageInfo packageInfo)
      <*> parallel (writeModuleIndex moduleIndex)
      <*> parallel (writeMeta meta)
      <*> parallel (patchDocs cfg)
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
      , Path.concat [ cfg.generatedDocs, "html" ]
      ]

  for_ dirs \dir -> do
    whenM (not <$> directoryExists dir) $
      liftEffect do
        logAndExit "Build the documentation first!"

-- | Read and decode given `docs.json` files.
decodeDocsJsons
  :: forall rest
   . { docsFiles :: Array String | rest }
  -> Aff (Array DocModule)
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
      let
        eiResult :: Either String DocModule
        eiResult =
          JSON.parse contents >>=
            ((unsafeCoerce :: JSON -> Json) >>> Docs.toDocModule >>> left CJ.Codec.printDecodeError)

      case eiResult of
        Left error -> do
          liftEffect $ log $
            "\"docs.json\" decoding failed failed for " <> jsonFile <> ": " <> error
          pure Nothing
        Right result -> pure $ Just result

    else do
      liftEffect $ do
        log $
          "File does not exist: " <> jsonFile
      pure Nothing

  when (Array.null docsJsons) do
    liftEffect $ logAndExit $
      "Couldn't decode any of the files matched by the following globs: " <> showGlobs cfg.docsFiles

  pure docsJsons

decodePursJsons :: forall rest. { pursJsonFiles :: Array String | rest } -> Aff (Array Manifest)
decodePursJsons { pursJsonFiles } = do
  paths <- getPathsByGlobs pursJsonFiles

  when (Array.null paths) do
    liftEffect do
      logAndExit $
        "The following globs do not match any files: " <> showGlobs pursJsonFiles <>
          ".\nAre you in a project directory?"

  Array.nubBy compareNames
    <$> Array.catMaybes
    <$>
      for paths \jsonFileName ->
        join <$> withExisting jsonFileName
          \contents ->
            either (logError jsonFileName) (pure <<< Just)
              ( JSON.parse contents >>=
                  CJ.decode Manifest.codec >>>
                    left CJ.DecodeError.print
              )

  where
  compareNames
    (Manifest { name: name1 })
    (Manifest { name: name2 }) = compare name1 name2

  logError fileName error = do
    liftEffect $ log $
      "\"purs.json\" decoding failed for " <> fileName <> ": " <> error
    pure Nothing

-- | Write type index parts to files.
writeTypeIndex :: TypeIndex -> Aff Unit
writeTypeIndex typeIndex =
  for_ entries \(Tuple typeShape results) -> do
    writeTextFile UTF8 (unwrap Config.typeIndexDirectory <> "/" <> typeShape <> ".js")
      (mkHeader typeShape <> JSON.print (CJ.encode codec results))
  where
  mkHeader typeShape =
    "// This file was generated by docs-search\n"
      <> "window.DocsSearchTypeIndex[\""
      <> typeShape
      <> "\"] = "

  codec = CJ.Common.maybe $ CJ.array SearchResult.searchResultCodec

  entries :: Array _
  entries = Map.toUnfoldableUnordered (unwrap typeIndex)

writePackageInfo :: PackageInfo -> Aff Unit
writePackageInfo packageInfo = do

  writeTextFile UTF8 (unwrap Config.packageInfoPath) $
    header <> JSON.print (CJ.encode (CJ.array PackageIndex.packageResultCodec) packageInfo)

  where
  header = "window.DocsSearchPackageIndex = "

writeModuleIndex :: PackedModuleIndex -> Aff Unit
writeModuleIndex moduleIndex = do
  writeTextFile UTF8 (unwrap Config.moduleIndexPath) $
    header <> JSON.print (CJ.encode ModuleIndex.packedModuleIndexCodec moduleIndex)
  where
  header = "window.DocsSearchModuleIndex = "

writeMeta :: Meta -> Aff Unit
writeMeta meta = do
  writeTextFile UTF8 (unwrap Config.metaPath) $
    header <> JSON.print (CJ.encode Meta.metaCodec meta)
  where
  header = "window." <> unwrap Config.metaItem <> " = "

-- | Get a mapping from index parts to index contents.
getIndex :: Declarations -> Map PartId (Array (Tuple String (Array SearchResult)))
getIndex (Declarations trie) =
  Array.foldr insert Map.empty parts
  where
  insert part = Map.insertWith append (Config.getPartId part.prefix) part.results

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
          "// This file was generated by docs-search.\n"
            <> "window.DocsSearchIndex[\""
            <> show indexPartId
            <> "\"] = "

      writeTextFile UTF8 (generatedDocs <> Config.mkIndexPartPath indexPartId) $
        header <> JSON.print (CJ.encode codec results)
  where
  codec = CJ.array $ CJ.Common.tuple CJ.string $ CJ.array SearchResult.searchResultCodec

patchHtml :: String -> Maybe String
patchHtml html =
  let
    patch = Fold.fold
      [ "<!-- Docs search index. -->"
      , "<script type=\"text/javascript\" src=\"./docs-search-app.js\"></script>"
      , "<script type=\"text/javascript\">"
      , "window.DocsSearchTypeIndex = {};"
      , "window.DocsSearchIndex = {};"
      , "</script>"
      , "</body>"
      ]
  in
    if String.contains (Pattern patch) html then Nothing
    else Just $
      String.replace (Pattern "</body>") (Replacement patch) html

-- | Iterate through the HTML files generated by the PureScript compiler, and
-- | modify them using `patchHtml`.
patchDocs :: Config -> Aff Unit
patchDocs cfg = do
  let dirname = cfg.generatedDocs

  files <- readdir (Path.concat [ dirname, "html" ])

  for_ files \file -> do
    let path = Path.concat [ dirname, "html", file ]

    whenM (fileExists path) do
      contents <- readTextFile UTF8 path
      let html = patchHtml contents
      Fold.for_ html $ writeTextFile UTF8 path

-- | Create directories for two indices, or fail with a message
-- | in case the docs were not generated.
createDirectories :: Config -> Aff Unit
createDirectories { generatedDocs } = do
  let
    htmlDocs = Path.concat [ generatedDocs, "html" ]
    indexDir = Path.concat [ generatedDocs, "html", "index" ]
    declIndexDir = Path.concat [ generatedDocs, "html", "index", "declarations" ]
    typeIndexDir = Path.concat [ generatedDocs, "html", "index", "types" ]

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
  appFile <- liftEffect getDocsSearchAppPath
  whenM (not <$> fileExists appFile) do
    liftEffect do
      logAndExit $
        "Client-side app was not found at " <> appFile <> ".\n" <>
          "Check your installation."
  buffer <- readFile appFile
  writeFile (Path.concat [ generatedDocs, "html", "docs-search-app.js" ]) buffer

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
  Process.exit' 1

showGlobs :: Array String -> String
showGlobs = Array.intercalate ", "

getPathsByGlobs :: Array String -> Aff (Array String)
getPathsByGlobs globs =
  liftEffect $ Array.concat <$> for globs glob

foreign import glob :: String -> Effect (Array String)
foreign import getDocsSearchAppPath :: Effect String
