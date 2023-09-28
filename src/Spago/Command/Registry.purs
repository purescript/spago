module Spago.Command.Registry where

import Spago.Prelude

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.DateTime (DateTime(..))
import Data.Formatter.DateTime as DateTime
import Data.Map as Map
import Data.String (Pattern(..))
import Data.String as String
import Node.Path as Path
import Registry.Internal.Codec as Internal
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Db as Db
import Spago.FS as FS
import Spago.Paths as Paths
import Spago.Registry (RegistryEnv)

type RegistrySearchArgs =
  { package :: String
  , json :: Boolean
  }

-- TODO: I guess we could also search in (1) the tags and (2) the description
search :: forall a. RegistrySearchArgs -> Spago (RegistryEnv a) Unit
search { package: searchString, json } = do
  logInfo $ "Searching for " <> show searchString <> " in the Registry package names..."
  metadataFiles <- FS.ls $ Path.concat [ Paths.registryPath, "metadata" ]

  let matches = Array.filter (String.contains (Pattern searchString)) (Array.mapMaybe (String.stripSuffix (Pattern ".json")) metadataFiles)

  if Array.null matches then do
    logError "Did not find any packages matching the search string."
  else do
    -- We have only the match names, at least we get the time of the last release to be even a little useful
    { getMetadata, logOptions } <- ask
    infos <- map (Map.fromFoldable <<< Array.catMaybes) $ for matches \match -> case PackageName.parse match of
      Left err -> do
        logWarn $ "Couldn't parse package name: " <> err
        pure Nothing
      Right packageName -> runSpago { logOptions } (getMetadata packageName) >>= case _ of
        Left err -> do
          logWarn $ "Couldn't read metadata for pacakge " <> PackageName.print packageName <> ", error: " <> err
          pure Nothing
        Right (Metadata meta) -> pure $ Just $ case Map.findMax meta.published of
          Nothing -> Tuple packageName { version: Nothing, publishedTime: Nothing }
          Just { key: version, value: { publishedTime } } -> Tuple packageName { version: Just version, publishedTime: Just publishedTime }

    -- Finally print all this stuff
    logInfo "Use `spago registry info $package` to get more details on a package."
    output $ case json of
      true ->
        let
          infoDataCodec = CA.Record.object "InfoData"
            { publishedTime: CA.Record.optional Internal.Codec.iso8601DateTime
            , version: CA.Record.optional Version.codec
            }
        in
          OutputJson (Internal.packageMap infoDataCodec) infos
      false -> OutputTable
        { titles: [ "NAME", "VERSION", "PUBLISHED TIME" ]
        , rows: infos # Map.toUnfoldable # map \(Tuple name { version, publishedTime }) ->
            [ PackageName.print name
            , maybe "-" Version.print version
            , maybe "-" (DateTime.format Internal.Format.iso8601DateTime) publishedTime
            ]
        }

type RegistryInfoArgs =
  { package :: String
  , json :: Boolean
  }

info :: forall a. RegistryInfoArgs -> Spago (RegistryEnv a) Unit
info { package, json } = do
  packageName <- case PackageName.parse package of
    Left err -> die [ toDoc "Could not parse package name, error:", indent (toDoc $ show err) ]
    Right name -> pure name

  { getMetadata, logOptions } <- ask
  runSpago { logOptions } (getMetadata packageName) >>= case _ of
    Left err -> do
      logDebug err
      die $ "Could not find package " <> PackageName.print packageName
    Right meta -> do
      -- We just print out the metadata file
      output case json of
        true -> OutputJson Metadata.codec meta
        false -> OutputYaml Metadata.codec meta

type RegistryPackageSetsArgs =
  { latest :: Boolean
  , json :: Boolean
  }

packageSets :: forall a. RegistryPackageSetsArgs -> Spago (RegistryEnv a) Unit
packageSets { latest, json } = do
  { db } <- ask
  availableSets <- liftEffect $ Db.selectPackageSets db

  let
    sets = case latest of
      false -> availableSets
      true ->
        -- here we need to keep only the highest version of all the sets with the same compiler version
        Array.fromFoldable
          $ Map.values
          $
            foldl
              ( \acc newSet -> case Map.lookup newSet.compiler acc of
                  Nothing -> Map.insert newSet.compiler newSet acc
                  Just { version } -> case newSet.version > version of
                    true -> Map.insert newSet.compiler newSet acc
                    false -> acc
              )
              Map.empty
              availableSets

  output case json of
    true -> OutputJson (CA.array Db.packageSetCodec) sets
    false -> OutputTable
      { titles: [ "VERSION", "DATE", "COMPILER" ]
      , rows: sets # map \{ version, date, compiler } ->
          [ Version.print version
          , DateTime.format Internal.Format.iso8601Date $ DateTime date bottom
          , Version.print compiler
          ]
      }
