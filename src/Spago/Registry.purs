module Spago.Registry where

import Spago.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Node.Path as Path
import Registry.PackageSet (PackageSet(..))
import Registry.PackageSet as PackageSet
import Registry.Version as Version
import Spago.Db (Db)
import Spago.Db as Db
import Spago.FS as FS
import Spago.Git as Git
import Spago.Paths as Paths
import Spago.Purs as Purs

type RegistryEnv a =
  { getManifestFromIndex :: PackageName -> Version -> Spago (LogEnv ()) (Maybe Manifest)
  , getMetadata :: PackageName -> Spago (LogEnv ()) (Either String Metadata)
  , logOptions :: LogOptions
  , purs :: Purs.Purs
  , git :: Git.Git
  , db :: Db.Db
  | a
  }

-- | Update the database with the latest package sets
updatePackageSetsDb :: forall a. Db -> Spago (LogEnv a) Unit
updatePackageSetsDb db = do
  setsAvailable <- map Set.fromFoldable getAvailablePackageSets
  setsInDb <- map (Set.fromFoldable <<< map _.version) (liftEffect $ Db.selectPackageSets db)
  let setsToInsert = Set.difference setsAvailable setsInDb

  unless (Set.isEmpty setsToInsert) do
    for_ (Set.toUnfoldable setsToInsert :: Array _) \setVersion -> do
      PackageSet set <- readPackageSet setVersion
      -- First insert the package set
      logDebug $ "Inserting package set in DB: " <> Version.print setVersion
      liftEffect $ Db.insertPackageSet db { compiler: set.compiler, date: set.published, version: set.version }
      -- Then we insert every entry separately
      for_ (Map.toUnfoldable set.packages :: Array _) \(Tuple name version) -> do
        liftEffect $ Db.insertPackageSetEntry db { packageName: name, packageVersion: version, packageSetVersion: set.version }

-- | List all the package sets versions available in the Registry repo
getAvailablePackageSets :: forall a. Spago (LogEnv a) (Array Version)
getAvailablePackageSets = do
  { success: setVersions, fail: parseFailures } <- map (partitionEithers <<< map parseSetVersion) $ FS.ls Paths.packageSetsPath

  unless (Array.null parseFailures) do
    logDebug $ [ toDoc "Failed to parse some package-sets versions:" ] <> map (indent <<< toDoc <<< show) parseFailures

  pure setVersions
  where
  parseSetVersion str = Version.parse case String.stripSuffix (Pattern ".json") str of
    Nothing -> str
    Just v -> v

readPackageSet :: forall a. Version -> Spago (LogEnv a) PackageSet
readPackageSet setVersion = do
  logDebug "Reading the package set from the Registry repo..."
  let packageSetPath = Path.concat [ Paths.packageSetsPath, Version.print setVersion <> ".json" ]
  liftAff (FS.readJsonFile PackageSet.codec packageSetPath) >>= case _ of
    Left err -> die $ "Couldn't read the package set: " <> err
    Right registryPackageSet -> do
      logDebug $ "Read the package set " <> Version.print setVersion <> " from the registry"
      pure registryPackageSet

findPackageSet :: forall a. Maybe Version -> Spago (RegistryEnv a) Version
findPackageSet maybeSet = do
  { db, purs } <- ask
  availableSets <- liftEffect $ Db.selectPackageSets db
  let availableVersions = map _.version availableSets

  case maybeSet of
    -- if our input param is in the list of sets just return that
    Just desiredSet -> case Set.member desiredSet (Set.fromFoldable availableVersions) of
      true -> pure desiredSet
      false -> die $ [ toDoc $ "Could not find desired set " <> Version.print desiredSet <> " in the list of available set versions:" ]
        <> map (indent <<< toDoc <<< Version.print) (Array.sort availableVersions)
    -- no set in input: read the compiler version, get the latest set matching
    Nothing -> do
      maybeVersion <- liftEffect $ Db.selectLatestPackageSetByCompiler db purs.version
      case maybeVersion of
        Just { version } -> pure version
        -- TODO: well we could approximate with any minor version really? See old Spago:
        -- https://github.com/purescript/spago/blob/01eecf041851ca0fbced1d4f7147fcbdd8bf168d/src/Spago/PackageSet.hs#L66
        Nothing -> do
          let availableCompilers = Set.fromFoldable $ map _.compiler availableSets
          die $ [ toDoc $ "No package set is compatible with your compiler version " <> Version.print purs.version, toDoc "Compatible versions:" ]
            <> map (indent <<< toDoc <<< Version.print) (Array.fromFoldable availableCompilers)
