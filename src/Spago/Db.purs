module Spago.Db
  ( ConnectOptions
  , Db
  , PackageSet
  , PackageSetEntry
  , PackageVersion
  , connect
  , getLastPull
  , getManifest
  , getMetadata
  , getMetadatas
  , insertManifest
  , insertMetadata
  , insertPackageSet
  , insertPackageSetEntry
  , packageSetCodec
  , selectLatestPackageSetByCompiler
  , selectPackageSets
  , updateLastPull
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.DateTime (Date, DateTime(..))
import Data.DateTime as DateTime
import Data.Either as Either
import Data.Filterable (filterMap)
import Data.Formatter.DateTime as DateTime.Format
import Data.Map as Map
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Time.Duration (Minutes(..))
import Effect.Now as Now
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4)
import Effect.Uncurried as Uncurried
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.Manifest as Manifest
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.Version as Version

--------------------------------------------------------------------------------
-- API

type ConnectOptions =
  { database :: FilePath
  , logger :: String -> Effect Unit
  }

connect :: ConnectOptions -> Effect Db
connect { database, logger } = Uncurried.runEffectFn2 connectImpl database (Uncurried.mkEffectFn1 logger)

insertPackageSet :: Db -> PackageSet -> Effect Unit
insertPackageSet db = Uncurried.runEffectFn2 insertPackageSetImpl db <<< packageSetToJs

insertPackageSetEntry :: Db -> PackageSetEntry -> Effect Unit
insertPackageSetEntry db = Uncurried.runEffectFn2 insertPackageSetEntryImpl db <<< packageSetEntryToJs

selectPackageSets :: Db -> Effect (Array PackageSet)
selectPackageSets db = do
  packageSets <- Uncurried.runEffectFn1 selectPackageSetsImpl db
  pure $ Array.mapMaybe packageSetFromJs packageSets

selectLatestPackageSetByCompiler :: Db -> Version -> Effect (Maybe PackageSet)
selectLatestPackageSetByCompiler db compiler = do
  maybePackageSet <- Nullable.toMaybe <$> Uncurried.runEffectFn2 selectLatestPackageSetByCompilerImpl db (Version.print compiler)
  pure $ packageSetFromJs =<< maybePackageSet

{-

We'll need these when implementing a command for "show me what's in this package set"

selectPackageSetEntriesBySet :: Db -> Version -> Effect (Array PackageSetEntry)
selectPackageSetEntriesBySet db packageSetVersion = do
  packageSetEntries <- Uncurried.runEffectFn2 selectPackageSetEntriesBySetImpl db (Version.print packageSetVersion)
  pure $ Array.mapMaybe packageSetEntryFromJs packageSetEntries

selectPackageSetEntriesByPackage :: Db -> PackageName -> Version -> Effect (Array PackageSetEntry)
selectPackageSetEntriesByPackage db packageName version = do
  packageSetEntries <- Uncurried.runEffectFn3 selectPackageSetEntriesByPackageImpl db (PackageName.print packageName) (Version.print version)
  pure $ Array.mapMaybe packageSetEntryFromJs packageSetEntries
-}

getLastPull :: Db -> String -> Effect (Maybe DateTime)
getLastPull db key = do
  maybePull <- Nullable.toMaybe <$> Uncurried.runEffectFn2 getLastPullImpl db key
  pure $ (Either.hush <<< DateTime.Format.unformat Internal.Format.iso8601DateTime) =<< maybePull

updateLastPull :: Db -> String -> DateTime -> Effect Unit
updateLastPull db key date = Uncurried.runEffectFn3 updateLastPullImpl db key (DateTime.Format.format Internal.Format.iso8601DateTime date)

getManifest :: Db -> PackageName -> Version -> Effect (Maybe Manifest)
getManifest db packageName version = do
  maybeManifest <- Nullable.toMaybe <$> Uncurried.runEffectFn3 getManifestImpl db (PackageName.print packageName) (Version.print version)
  pure $ (Either.hush <<< parseJson Manifest.codec) =<< maybeManifest

insertManifest :: Db -> PackageName -> Version -> Manifest -> Effect Unit
insertManifest db packageName version manifest = Uncurried.runEffectFn4 insertManifestImpl db (PackageName.print packageName) (Version.print version) (printJson Manifest.codec manifest)

getMetadata :: Db -> PackageName -> Effect (Maybe Metadata)
getMetadata db packageName = do
  maybeMetadataEntry <- Nullable.toMaybe <$> Uncurried.runEffectFn2 getMetadataImpl db (PackageName.print packageName)
  now <- Now.nowDateTime
  pure $ do
    metadataEntry <- maybeMetadataEntry
    lastFetched <- Either.hush $ DateTime.Format.unformat Internal.Format.iso8601DateTime metadataEntry.last_fetched
    -- if the metadata is older than 15 minutes, we consider it stale
    case DateTime.diff now lastFetched of
      Minutes n | n <= 15.0 -> do
        metadata <- Either.hush $ parseJson Metadata.codec metadataEntry.metadata
        pure metadata
      _ -> Nothing

getMetadatas :: Db -> Array PackageName -> Effect (Map PackageName Metadata)
getMetadatas db packageNames = do
  metadataEntries <- Uncurried.runEffectFn2 getMetadatasImpl db (PackageName.print <$> packageNames)
  now <- Now.nowDateTime
  pure
    $ metadataEntries
    #
      ( filterMap \metadataEntry -> do
          packageName <- hush $ PackageName.parse metadataEntry.name
          lastFetched <- Either.hush $ DateTime.Format.unformat Internal.Format.iso8601DateTime metadataEntry.last_fetched
          -- if the metadata is older than 15 minutes, we consider it stale
          case DateTime.diff now lastFetched of
            Minutes n | n <= 15.0 -> do
              metadata <- Either.hush $ parseJson Metadata.codec metadataEntry.metadata
              pure $ packageName /\ metadata
            _ -> Nothing
      )
    # Map.fromFoldable

insertMetadata :: Db -> PackageName -> Metadata -> Effect Unit
insertMetadata db packageName metadata@(Metadata { unpublished }) = do
  now <- Now.nowDateTime
  Uncurried.runEffectFn4 insertMetadataImpl db (PackageName.print packageName) (printJson Metadata.codec metadata) (DateTime.Format.format Internal.Format.iso8601DateTime now)
  -- we also do a pass of removing the cached manifests that have been unpublished
  for_ (Map.toUnfoldable unpublished :: Array _) \(Tuple version _) -> do
    Uncurried.runEffectFn3 removeManifestImpl db (PackageName.print packageName) (Version.print version)

--------------------------------------------------------------------------------
-- Table types and conversions

-- Note: bump `Paths.databaseVersion` every time we change the database schema in a breaking way
data Db

type PackageSetJs =
  { version :: String
  , compiler :: String
  , date :: String
  }

type PackageSet =
  { version :: Version
  , compiler :: Version
  , date :: Date
  }

type PackageVersionJs =
  { name :: String
  , version :: String
  , published :: Int
  , date :: String
  , manifest :: String
  , location :: String
  }

type PackageVersion =
  { name :: PackageName
  , version :: Version
  , published :: Boolean
  , date :: DateTime
  , manifest :: Manifest
  , location :: Location
  }

type PackageSetEntryJs =
  { packageSetVersion :: String
  , packageName :: String
  , packageVersion :: String
  }

type PackageSetEntry =
  { packageSetVersion :: Version
  , packageName :: PackageName
  , packageVersion :: Version
  }

type MetadataEntryJs =
  { name :: String
  , metadata :: String
  , last_fetched :: String
  }

packageSetToJs :: PackageSet -> PackageSetJs
packageSetToJs { version, compiler, date } =
  { version: Version.print version
  , compiler: Version.print compiler
  , date: DateTime.Format.format Internal.Format.iso8601Date $ DateTime date bottom
  }

packageSetFromJs :: PackageSetJs -> Maybe PackageSet
packageSetFromJs p = hush do
  version <- Version.parse p.version
  compiler <- Version.parse p.compiler
  date <- map DateTime.date $ DateTime.Format.unformat Internal.Format.iso8601Date p.date
  pure $ { version, compiler, date }

packageSetEntryToJs :: PackageSetEntry -> PackageSetEntryJs
packageSetEntryToJs { packageSetVersion, packageName, packageVersion } =
  { packageSetVersion: Version.print packageSetVersion
  , packageName: PackageName.print packageName
  , packageVersion: Version.print packageVersion
  }

{-

packageSetEntryFromJs :: PackageSetEntryJs -> Maybe PackageSetEntry
packageSetEntryFromJs p = hush do
  packageSetVersion <- Version.parse p.packageSetVersion
  packageName <- PackageName.parse p.packageName
  packageVersion <- Version.parse p.packageVersion
  pure $ { packageSetVersion, packageName, packageVersion }

-}

--------------------------------------------------------------------------------
-- Codecs

packageSetCodec :: CJ.Codec PackageSet
packageSetCodec = CJ.named "PackageSet" $ CJ.Record.object
  { date: Internal.Codec.iso8601Date
  , version: Version.codec
  , compiler: Version.codec
  }

--------------------------------------------------------------------------------
-- FFI

foreign import connectImpl :: EffectFn2 FilePath (EffectFn1 String Unit) Db

foreign import insertPackageSetImpl :: EffectFn2 Db PackageSetJs Unit

foreign import insertPackageSetEntryImpl :: EffectFn2 Db PackageSetEntryJs Unit

foreign import selectLatestPackageSetByCompilerImpl :: EffectFn2 Db String (Nullable PackageSetJs)

foreign import selectPackageSetsImpl :: EffectFn1 Db (Array PackageSetJs)

foreign import selectPackageSetEntriesBySetImpl :: EffectFn2 Db String (Array PackageSetEntryJs)

foreign import selectPackageSetEntriesByPackageImpl :: EffectFn3 Db String String (Array PackageSetEntryJs)

foreign import getLastPullImpl :: EffectFn2 Db String (Nullable String)

foreign import updateLastPullImpl :: EffectFn3 Db String String Unit

foreign import getManifestImpl :: EffectFn3 Db String String (Nullable String)

foreign import insertManifestImpl :: EffectFn4 Db String String String Unit

foreign import removeManifestImpl :: EffectFn3 Db String String Unit

foreign import getMetadataImpl :: EffectFn2 Db String (Nullable MetadataEntryJs)

foreign import getMetadatasImpl :: EffectFn2 Db (Array String) (Array MetadataEntryJs)

foreign import insertMetadataImpl :: EffectFn4 Db String String String Unit
