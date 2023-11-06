module Spago.Db
  ( ConnectOptions
  , Db
  , PackageSet
  , PackageSetEntry
  , PackageVersion
  , connect
  , selectPackageSets
  , selectLatestPackageSetByCompiler
  , insertPackageSet
  , insertPackageSetEntry
  , packageSetCodec
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Codec.Argonaut as Json
import Data.Codec.Argonaut.Record as CA.Record
import Data.DateTime (Date, DateTime(..))
import Data.DateTime as Date
import Data.Formatter.DateTime as DateTime
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3)
import Effect.Uncurried as Uncurried
import Node.Path as Path
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.Location as Location
import Registry.Manifest as Manifest
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Paths as Paths

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

insertPackageVersion :: Db -> PackageVersion -> Effect Unit
insertPackageVersion db = Uncurried.runEffectFn2 insertPackageVersionImpl db <<< packageVersionToJs

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

selectPackageVersion :: Db -> PackageName -> Version -> Effect (Maybe PackageVersion)
selectPackageVersion db packageName version = do
  maybePackageVersion <- Nullable.toMaybe <$> Uncurried.runEffectFn3 selectPackageVersionImpl db (PackageName.print packageName) (Version.print version)
  pure $ packageVersionFromJs =<< maybePackageVersion

unpublishPackageVersion :: Db -> PackageName -> Version -> Effect Unit
unpublishPackageVersion db packageName version = Uncurried.runEffectFn3 unpublishPackageVersionImpl db (PackageName.print packageName) (Version.print version)

selectPackageSetEntriesBySet :: Db -> Version -> Effect (Array PackageSetEntry)
selectPackageSetEntriesBySet db packageSetVersion = do
  packageSetEntries <- Uncurried.runEffectFn2 selectPackageSetEntriesBySetImpl db (Version.print packageSetVersion)
  pure $ Array.mapMaybe packageSetEntryFromJs packageSetEntries

selectPackageSetEntriesByPackage :: Db -> PackageName -> Version -> Effect (Array PackageSetEntry)
selectPackageSetEntriesByPackage db packageName version = do
  packageSetEntries <- Uncurried.runEffectFn3 selectPackageSetEntriesByPackageImpl db (PackageName.print packageName) (Version.print version)
  pure $ Array.mapMaybe packageSetEntryFromJs packageSetEntries

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

packageSetToJs :: PackageSet -> PackageSetJs
packageSetToJs { version, compiler, date } =
  { version: Version.print version
  , compiler: Version.print compiler
  , date: DateTime.format Internal.Format.iso8601Date $ DateTime date bottom
  }

packageSetFromJs :: PackageSetJs -> Maybe PackageSet
packageSetFromJs p = hush do
  version <- Version.parse p.version
  compiler <- Version.parse p.compiler
  date <- map Date.date $ DateTime.unformat Internal.Format.iso8601Date p.date
  pure $ { version, compiler, date }

packageVersionToJs :: PackageVersion -> PackageVersionJs
packageVersionToJs { name, version, published, date, manifest, location } =
  { name: PackageName.print name
  , version: Version.print version
  , published: if published then 1 else 0
  , date: DateTime.format Internal.Format.iso8601DateTime date
  , manifest: printJson Manifest.codec manifest
  , location: printJson Location.codec location
  }

packageVersionFromJs :: PackageVersionJs -> Maybe PackageVersion
packageVersionFromJs p = hush do
  name <- PackageName.parse p.name
  version <- Version.parse p.version
  date <- DateTime.unformat Internal.Format.iso8601DateTime p.date
  manifest <- lmap Json.printJsonDecodeError $ parseJson Manifest.codec p.manifest
  location <- lmap Json.printJsonDecodeError $ parseJson Location.codec p.location
  pure $ { name, version, published: p.published == 1, date, manifest, location }

packageSetEntryToJs :: PackageSetEntry -> PackageSetEntryJs
packageSetEntryToJs { packageSetVersion, packageName, packageVersion } =
  { packageSetVersion: Version.print packageSetVersion
  , packageName: PackageName.print packageName
  , packageVersion: Version.print packageVersion
  }

packageSetEntryFromJs :: PackageSetEntryJs -> Maybe PackageSetEntry
packageSetEntryFromJs p = hush do
  packageSetVersion <- Version.parse p.packageSetVersion
  packageName <- PackageName.parse p.packageName
  packageVersion <- Version.parse p.packageVersion
  pure $ { packageSetVersion, packageName, packageVersion }

--------------------------------------------------------------------------------
-- Codecs

packageSetCodec :: JsonCodec PackageSet
packageSetCodec = CA.Record.object "PackageSet"
  { date: Internal.Codec.iso8601Date
  , version: Version.codec
  , compiler: Version.codec
  }

--------------------------------------------------------------------------------
-- FFI

foreign import connectImpl :: EffectFn2 FilePath (EffectFn1 String Unit) Db

foreign import insertPackageSetImpl :: EffectFn2 Db PackageSetJs Unit

foreign import insertPackageVersionImpl :: EffectFn2 Db PackageVersionJs Unit

foreign import insertPackageSetEntryImpl :: EffectFn2 Db PackageSetEntryJs Unit

foreign import selectLatestPackageSetByCompilerImpl :: EffectFn2 Db String (Nullable PackageSetJs)

foreign import selectPackageSetsImpl :: EffectFn1 Db (Array PackageSetJs)

foreign import selectPackageVersionImpl :: EffectFn3 Db String String (Nullable PackageVersionJs)

foreign import unpublishPackageVersionImpl :: EffectFn3 Db String String Unit

foreign import selectPackageSetEntriesBySetImpl :: EffectFn2 Db String (Array PackageSetEntryJs)

foreign import selectPackageSetEntriesByPackageImpl :: EffectFn3 Db String String (Array PackageSetEntryJs)
