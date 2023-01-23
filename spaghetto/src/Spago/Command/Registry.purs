module Spago.Command.Registry where

import Spago.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.String (Pattern(..))
import Data.String as String
import Node.Path as Path
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.FS as FS
import Spago.Git as Git
import Spago.Paths as Paths

type RegistryEnv a =
  { getManifestFromIndex :: PackageName -> Version -> Spago (LogEnv ()) (Maybe Manifest)
  , getMetadata :: PackageName -> Spago (LogEnv ()) (Either String Metadata)
  , logOptions :: LogOptions
  , git :: Git.Git
  | a
  }

-- TODO: some of these commands output text, some JSON, and the interface feels unpolished.
-- We should do some user testing and make the experience a little more cohesive

search :: forall a. String -> Spago (RegistryEnv a) Unit
search searchString = do
  logInfo $ "Searching for " <> show searchString <> " in the Registry package names..."
  metadataFiles <- FS.ls $ Path.concat [ Paths.registryPath, "metadata" ]

  let matches = Array.filter (String.contains (Pattern searchString)) (Array.mapMaybe (String.stripSuffix (Pattern ".json")) metadataFiles)

  if Array.null matches then
    logError "Did not find any packages matching the search string."
  else do
    output $ OutputLines matches
    logInfo "Use `spago registry info $package` to get more details on a package."

info :: forall a. { package :: String, maybeVersion :: Maybe String } -> Spago (RegistryEnv a) Unit
info args = do
  packageName <- case PackageName.parse args.package of
    Left err -> die [ toDoc "Could not parse package name, error:", indent (toDoc $ show err) ]
    Right name -> pure name

  maybeVersion <- case args.maybeVersion of
    Nothing -> pure Nothing
    Just v -> case parseLenientVersion v of
      Left err -> die [ toDoc "Could not parse version, error:", indent (toDoc $ show err) ]
      Right version -> pure $ Just version

  { getMetadata, logOptions } <- ask
  runSpago { logOptions } (getMetadata packageName) >>= case _ of
    Left err -> do
      logDebug err
      die $ "Could not find package " <> PackageName.print packageName
    Right (Metadata metadata) -> case maybeVersion of
      Nothing -> do
        output $ OutputLines $ map Version.print $ Array.fromFoldable $ Map.keys $ metadata.published
        logInfo $ "Use `spago registry info " <> PackageName.print packageName <> " $version` to get more details on a version."
      Just version -> case Map.lookup version metadata.published of
        Nothing -> die $ "Version " <> Version.print version <> " does not exist for package " <> PackageName.print packageName
        -- TODO: unify the formats. Here we output json, above just lines, this is terrible
        Just pubInfo -> output $ OutputJson Metadata.publishedMetadataCodec pubInfo
