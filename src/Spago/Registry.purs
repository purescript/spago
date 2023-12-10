module Spago.Registry
  ( PreRegistryEnv
  , PreRegistryEnvRow
  , RegistryEnv
  , RegistryEnvRow
  , RegistryFunctions
  , findPackageSet
  , getManifestFromIndex
  , getMetadata
  , getRegistryFns
  , listMetadataFiles
  , readPackageSet
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.DateTime as DateTime
import Data.Map as Map
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Time.Duration (Minutes(..))
import Effect.Now as Now
import Effect.Ref as Ref
import Node.Path as Path
import Registry.Constants as Registry.Constants
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.PackageSet (PackageSet(..))
import Registry.PackageSet as PackageSet
import Registry.Version as Version
import Spago.Db (Db)
import Spago.Db as Db
import Spago.FS as FS
import Spago.Git as Git
import Spago.Paths as Paths
import Spago.Purs as Purs

type PreRegistryEnvRow a =
  ( offline :: OnlineStatus
  , logOptions :: LogOptions
  , purs :: Purs.Purs
  , git :: Git.Git
  , db :: Db.Db
  | a
  )

type PreRegistryEnv a = Record (PreRegistryEnvRow a)

type RegistryEnvRow a = PreRegistryEnvRow
  ( getRegistry :: Spago (PreRegistryEnv ()) RegistryFunctions
  | a
  )

type RegistryEnv a = Record (RegistryEnvRow a)

type RegistryFunctions =
  { getManifestFromIndex :: PackageName -> Version -> Spago (LogEnv ()) (Maybe Manifest)
  , getMetadata :: PackageName -> Spago (LogEnv ()) (Either String Metadata)
  , findPackageSet :: Maybe Version -> Spago (PreRegistryEnv ()) Version
  , listMetadataFiles :: Spago (LogEnv ()) (Array String)
  , readPackageSet :: Version -> Spago (LogEnv ()) PackageSet
  }

getMetadata :: PackageName -> Spago (RegistryEnv _) _
getMetadata packageName = do
  { getRegistry, logOptions, db, git, purs, offline } <- ask
  { getMetadata: fn } <- runSpago { logOptions, db, git, purs, offline } getRegistry
  runSpago { logOptions } (fn packageName)

getManifestFromIndex :: PackageName -> Version -> Spago (RegistryEnv _) _
getManifestFromIndex packageName version = do
  { getRegistry, logOptions, db, git, purs, offline } <- ask
  { getManifestFromIndex: fn } <- runSpago { logOptions, db, git, purs, offline } getRegistry
  runSpago { logOptions } (fn packageName version)

findPackageSet :: Maybe Version -> Spago (RegistryEnv _) _
findPackageSet version = do
  { getRegistry, logOptions, db, git, purs, offline } <- ask
  { findPackageSet: fn } <- runSpago { logOptions, db, git, purs, offline } getRegistry
  runSpago { logOptions, db, git, purs, offline } (fn version)

listMetadataFiles :: Spago (RegistryEnv _) _
listMetadataFiles = do
  { getRegistry, logOptions, db, git, purs, offline } <- ask
  { listMetadataFiles: fn } <- runSpago { logOptions, db, git, purs, offline } getRegistry
  runSpago { logOptions } fn

readPackageSet :: Version -> Spago (RegistryEnv _) _
readPackageSet version = do
  { getRegistry, logOptions, db, git, purs, offline } <- ask
  { readPackageSet: fn } <- runSpago { logOptions, db, git, purs, offline } getRegistry
  runSpago { logOptions } (fn version)

getRegistryFns :: Ref (Maybe RegistryFunctions) -> Spago (PreRegistryEnv _) RegistryFunctions
getRegistryFns registryRef = do
  -- We make a Ref here that we use to keep track of the Registry pull:
  -- if something needs to use the Registry, then it will have to call this function
  -- and populate this container, and all subsequent calls will just read from it
  { db } <- ask
  liftEffect (Ref.read registryRef) >>= case _ of
    Just registry -> pure registry
    Nothing -> do
      fetchingFreshRegistry <- fetchRegistry db
      let
        registryFns =
          { getManifestFromIndex: getManifestFromIndexImpl db
          , getMetadata: getMetadataImpl db fetchingFreshRegistry
          , listMetadataFiles: FS.ls (Path.concat [ Paths.registryPath, Registry.Constants.metadataDirectory ])
          , findPackageSet: findPackageSetImpl
          , readPackageSet: readPackageSetImpl
          }
      liftEffect $ Ref.write (Just registryFns) registryRef
      pure registryFns

  where
  fetchRegistry :: Db -> Spago (PreRegistryEnv _) Boolean
  fetchRegistry db = do
    -- we keep track of how old the latest pull was - if the last pull was recent enough
    -- we just move on, otherwise run the fibers
    fetchingFreshRegistry <- shouldFetchRegistryRepos db
    when fetchingFreshRegistry do
      -- clone the registry and index repo, or update them
      logInfo "Refreshing the Registry Index..."
      parallelise
        [ Git.fetchRepo { git: "https://github.com/purescript/registry-index.git", ref: "main" } Paths.registryIndexPath >>= case _ of
            Right _ -> pure unit
            Left _err -> logWarn "Couldn't refresh the registry-index, will proceed anyways"
        , Git.fetchRepo { git: "https://github.com/purescript/registry.git", ref: "main" } Paths.registryPath >>= case _ of
            Right _ -> pure unit
            Left _err -> logWarn "Couldn't refresh the registry, will proceed anyways"
        ]

    -- Now that we are up to date with the Registry we init/refresh the database
    updatePackageSetsDb db
    pure fetchingFreshRegistry

  -- | Update the database with the latest package sets
  updatePackageSetsDb :: Db -> Spago (LogEnv _) Unit
  updatePackageSetsDb db = do
    { logOptions } <- ask
    setsAvailable <- map Set.fromFoldable getAvailablePackageSets
    setsInDb <- map (Set.fromFoldable <<< map _.version) (liftEffect $ Db.selectPackageSets db)
    let setsToInsert = Set.difference setsAvailable setsInDb

    unless (Set.isEmpty setsToInsert) do
      for_ (Set.toUnfoldable setsToInsert :: Array _) \setVersion -> do
        PackageSet set <- runSpago { logOptions } (readPackageSetImpl setVersion)
        -- First insert the package set
        logDebug $ "Inserting package set in DB: " <> Version.print setVersion
        liftEffect $ Db.insertPackageSet db { compiler: set.compiler, date: set.published, version: set.version }
        -- Then we insert every entry separately
        for_ (Map.toUnfoldable set.packages :: Array _) \(Tuple name version) -> do
          liftEffect $ Db.insertPackageSetEntry db { packageName: name, packageVersion: version, packageSetVersion: set.version }

  -- | List all the package sets versions available in the Registry repo
  getAvailablePackageSets :: Spago (LogEnv _) (Array Version)
  getAvailablePackageSets = do
    { success: setVersions, fail: parseFailures } <- map (partitionEithers <<< map parseSetVersion) $ FS.ls Paths.packageSetsPath

    unless (Array.null parseFailures) do
      logDebug $ [ toDoc "Failed to parse some package-sets versions:" ] <> map (indent <<< toDoc <<< show) parseFailures

    pure setVersions
    where
    parseSetVersion str = Version.parse case String.stripSuffix (Pattern ".json") str of
      Nothing -> str
      Just v -> v

  readPackageSetImpl :: Version -> Spago (LogEnv ()) PackageSet
  readPackageSetImpl setVersion = do
    logDebug "Reading the package set from the Registry repo..."
    let packageSetPath = Path.concat [ Paths.packageSetsPath, Version.print setVersion <> ".json" ]
    liftAff (FS.readJsonFile PackageSet.codec packageSetPath) >>= case _ of
      Left err -> die $ "Couldn't read the package set: " <> err
      Right registryPackageSet -> do
        logDebug $ "Read the package set " <> Version.print setVersion <> " from the registry"
        pure registryPackageSet

-- Metadata can change over time (unpublished packages, and new packages), so we need
-- to read it from file every time we have a fresh Registry
getMetadataImpl :: Db -> Boolean -> PackageName -> Spago (LogEnv ()) (Either String Metadata)
getMetadataImpl db fetchingFreshRegistry name = do
  -- we first try reading it from the DB
  liftEffect (Db.getMetadata db name) >>= case _ of
    Just metadata | not fetchingFreshRegistry -> do
      logDebug $ "Got metadata from DB: " <> PackageName.print name
      pure (Right metadata)
    _ -> do
      -- if we don't have it we try reading it from file
      metadataFromFile name >>= case _ of
        Left e -> pure (Left e)
        Right m -> do
          -- and memoize it
          liftEffect (Db.insertMetadata db name m)
          pure (Right m)
  where
  metadataFromFile pkgName = do
    let metadataFilePath = Path.concat [ Paths.registryPath, Registry.Constants.metadataDirectory, PackageName.print pkgName <> ".json" ]
    logDebug $ "Reading metadata from file: " <> metadataFilePath
    liftAff (FS.readJsonFile Metadata.codec metadataFilePath)

-- Manifests are immutable so we can just lookup in the DB or read from file if not there
getManifestFromIndexImpl :: Db -> PackageName -> Version -> Spago (LogEnv ()) (Maybe Manifest)
getManifestFromIndexImpl db name version = do
  liftEffect (Db.getManifest db name version) >>= case _ of
    Just manifest -> pure (Just manifest)
    Nothing -> do
      -- if we don't have it we need to read it from file
      -- (note that we have all the versions of a package in the same file)
      logDebug $ "Reading package from Index: " <> PackageName.print name
      maybeManifests <- liftAff $ ManifestIndex.readEntryFile Paths.registryIndexPath name
      manifests <- map (map (\m@(Manifest m') -> Tuple m'.version m)) case maybeManifests of
        Right ms -> pure $ NonEmptyArray.toUnfoldable ms
        Left err -> do
          logWarn $ "Could not read package manifests from index, proceeding anyways. Error: " <> err
          pure []
      let versions = Map.fromFoldable manifests
      -- and memoize it
      for_ manifests \(Tuple _ manifest@(Manifest m)) -> do
        logDebug $ "Inserting manifest in DB: " <> PackageName.print name <> " v" <> Version.print m.version
        liftEffect $ Db.insertManifest db name m.version manifest
      pure (Map.lookup version versions)

findPackageSetImpl :: forall a. Maybe Version -> Spago (PreRegistryEnv a) Version
findPackageSetImpl maybeSet = do
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

-- | Check if we have fetched the registry recently enough, so we don't hit the net all the time
shouldFetchRegistryRepos :: forall a. Db -> Spago (LogEnv a) Boolean
shouldFetchRegistryRepos db = do
  now <- liftEffect $ Now.nowDateTime
  let registryKey = "registry"
  maybeLastRegistryFetch <- liftEffect $ Db.getLastPull db registryKey
  case maybeLastRegistryFetch of
    -- No record, so we have to fetch
    Nothing -> do
      logDebug "No record of last registry pull, will fetch"
      liftEffect $ Db.updateLastPull db registryKey now
      pure true
    -- We have a record, so we check if it's old enough
    Just lastRegistryFetch -> do
      let staleAfter = Minutes 15.0
      let (timeDiff :: Minutes) = DateTime.diff now lastRegistryFetch
      let isOldEnough = timeDiff > staleAfter
      -- We check if it's old, but also if we have it at all
      registryExists <- FS.exists Paths.registryPath
      if isOldEnough || not registryExists then do
        logDebug "Registry is old, refreshing"
        liftEffect $ Db.updateLastPull db registryKey now
        pure true
      else do
        logDebug "Registry is fresh enough, moving on..."
        pure false
