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
  , offline :: OnlineStatus
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
        -- if we get a version, all good
        Just { version } -> pure version
        -- if we dont, we try to find a set that is not directly associated with the current compiler, but is still good
        -- see `isVersionCompatible` for more info
        Nothing -> do
          let maybeLatestRelease = Array.last $ Array.sortBy (compare `on` _.version) availableSets
          case maybeLatestRelease of
            Just latest | isVersionCompatible purs.version latest.compiler -> pure latest.version
            -- otherwise fail but try to be helpful about the situation
            _ -> do
              let availableCompilers = Set.fromFoldable $ map _.compiler availableSets
              die $ [ toDoc $ "No package set is compatible with your compiler version " <> Version.print purs.version, toDoc "Compatible versions:" ]
                <> map (indent <<< toDoc <<< Version.print) (Array.fromFoldable availableCompilers)

-- | The check is successful only when the installed compiler is "slightly"
-- | greater or equal to the minimum version. E.g. fine cases are:
-- | - current is 0.12.2 and package-set is on 0.12.1
-- | - current is 1.4.3 and package-set is on 1.3.4
-- | Not fine cases are e.g.:
-- | - current is 0.1.2 and package-set is 0.2.3
-- | - current is 1.2.3 and package-set is 1.3.4
-- | - current is 1.2.3 and package-set is 0.2.3
isVersionCompatible :: Version -> Version -> Boolean
isVersionCompatible installedVersion minVersion =
  let
    installedVersionList = [ Version.major installedVersion, Version.minor installedVersion, Version.patch installedVersion ]
    minVersionList = [ Version.major minVersion, Version.minor minVersion, Version.patch minVersion ]
  in
    case installedVersionList, minVersionList of
      [ 0, b, c ], [ 0, y, z ] | b == y && c >= z -> true
      [ a, b, _c ], [ x, y, _z ] | a /= 0 && a == x && b >= y -> true
      _, _ -> false
