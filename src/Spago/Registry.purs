module Spago.Registry
  ( PreRegistryEnv
  , PreRegistryEnvRow
  , RegistryEnv
  , RegistryEnvRow
  , RegistryFunctions
  , findPackageSet
  , getManifestFromIndex
  , getMetadata
  , getMetadataForPackages
  , getRegistryFns
  , listMetadataFiles
  , listPackageSets
  , readPackageSet
  , submitRegistryOperation
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.JSON as CJ
import Data.DateTime (DateTime)
import Data.DateTime (diff) as DateTime
import Data.Formatter.DateTime (format) as DateTime
import Data.Map as Map
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..), Minutes(..))
import Data.Traversable (sequence)
import Effect.AVar (AVar)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Exception as Exception
import Effect.Now as Now
import Fetch as Http
import Node.Path as Path
import Node.Process as Process
import Registry.API.V1 as V1
import Registry.Constants as Registry.Constants
import Registry.Internal.Format as Internal.Format
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.PackageSet (PackageSet(..))
import Registry.PackageSet as PackageSet
import Registry.Version as Version
import Routing.Duplex as Duplex
import Spago.Db (Db)
import Spago.Db as Db
import Spago.FS as FS
import Spago.Git (GitEnv)
import Spago.Git as Git
import Spago.Json as Json
import Spago.Log (LogVerbosity(..))
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
  , getMetadataForPackages :: Array PackageName -> Spago (LogEnv ()) (Either String (Map PackageName Metadata))
  , findPackageSet :: Maybe Version -> Spago (PreRegistryEnv ()) Version
  , listPackageSets :: Spago (PreRegistryEnv ()) (Array Db.PackageSet)
  , listMetadataFiles :: Spago (LogEnv ()) (Array String)
  , readPackageSet :: Version -> Spago (LogEnv ()) PackageSet
  }

getMetadata :: PackageName -> Spago (RegistryEnv _) _
getMetadata packageName = do
  { getRegistry, logOptions, db, git, purs, offline } <- ask
  { getMetadata: fn } <- runSpago { logOptions, db, git, purs, offline } getRegistry
  runSpago { logOptions } (fn packageName)

getMetadataForPackages :: Array PackageName -> Spago (RegistryEnv _) _
getMetadataForPackages packageNames = do
  { getRegistry, logOptions, db, git, purs, offline } <- ask
  { getMetadataForPackages: fn } <- runSpago { logOptions, db, git, purs, offline } getRegistry
  runSpago { logOptions } (fn packageNames)

getManifestFromIndex :: PackageName -> Version -> Spago (RegistryEnv _) _
getManifestFromIndex packageName version = do
  { getRegistry, logOptions, db, git, purs, offline } <- ask
  { getManifestFromIndex: fn } <- runSpago { logOptions, db, git, purs, offline } getRegistry
  runSpago { logOptions } (fn packageName version)

listPackageSets :: Spago (RegistryEnv _) (Array Db.PackageSet)
listPackageSets = do
  { getRegistry, logOptions, db, git, purs, offline } <- ask
  { listPackageSets: fn } <- runSpago { logOptions, db, git, purs, offline } getRegistry
  runSpago { logOptions, db, git, purs, offline } fn

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

getRegistryFns :: AVar RegistryFunctions -> AVar Unit -> Spago (PreRegistryEnv _) RegistryFunctions
getRegistryFns registryBox registryLock = do
  -- The Box AVar will be empty until the first time we fetch the Registry, then
  -- we can just use the value that is cached.
  -- The Lock AVar is used to make sure
  -- that only one fiber is fetching the Registry at a time, and that all the other
  -- fibers will wait for it to finish and then use the cached value.
  { offline } <- ask
  liftAff $ AVar.take registryLock
  fns <- liftAff (AVar.tryRead registryBox) >>= case _ of
    -- If we are asked to bypass the cache then we need to rebuild again
    Just _registry | offline == OnlineBypassCache -> do
      -- We know the box is full so first thing we do is to empty it
      void $ liftAff $ AVar.take registryBox
      buildRegistryFns
    -- If we are in other states we can just return the cached value
    Just registry -> pure registry
    -- No cached value, so build it
    Nothing -> buildRegistryFns
  liftAff $ AVar.put unit registryLock
  pure fns
  where
  buildRegistryFns :: Spago (PreRegistryEnv _) RegistryFunctions
  buildRegistryFns = do
    { db, offline } <- ask
    _fetchingFreshRegistry <- fetchRegistry
    let
      registryFns =
        { getManifestFromIndex: getManifestFromIndexImpl db
        , getMetadata: getMetadataImpl db offline
        , getMetadataForPackages: getMetadataForPackagesImpl db offline
        , listMetadataFiles: FS.ls (Path.concat [ Paths.registryPath, Registry.Constants.metadataDirectory ])
        , listPackageSets: listPackageSetsImpl
        , findPackageSet: findPackageSetImpl
        , readPackageSet: readPackageSetImpl
        }
    liftAff $ AVar.put registryFns registryBox
    pure registryFns

  fetchRegistry :: Spago (PreRegistryEnv _) Boolean
  fetchRegistry = do
    -- we keep track of how old the latest pull was - if the last pull was recent enough
    -- we just move on, otherwise run the fibers
    { db, offline } <- ask
    fetchingFreshRegistry <- shouldFetchRegistryRepos db
    -- we also check if we need to bypass this cache (for when we need the freshest data)
    when (fetchingFreshRegistry || offline == OnlineBypassCache) do
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
getMetadataImpl :: Db -> OnlineStatus -> PackageName -> Spago (LogEnv ()) (Either String Metadata)
getMetadataImpl db onlineStatus name =
  getMetadataForPackagesImpl db onlineStatus [ name ]
    <#> case _ of
      Left err -> Left err
      Right metadataMap -> case Map.lookup name metadataMap of
        Nothing -> Left $ "Failed to get metadata for package: " <> PackageName.print name
        Just metadata -> Right metadata

-- Parallelised version of `getMetadataImpl`
getMetadataForPackagesImpl :: Db -> OnlineStatus -> Array PackageName -> Spago (LogEnv ()) (Either String (Map PackageName Metadata))
getMetadataForPackagesImpl db onlineStatus names = do
  (map Map.fromFoldable <<< sequence) <$> case onlineStatus == OnlineBypassCache of
    true -> do
      logDebug "Bypassing cache, reading metadata from file"
      parTraverseSpago metadataFromFile names
    false -> do
      -- the first layer of caching is in the DB, so we try that first
      metadatas <- liftEffect $ Db.getMetadataForPackages db names
      parTraverseSpago
        ( \name -> do
            case Map.lookup name metadatas of
              -- if we don't have it in cache we try reading it from file
              Nothing -> metadataFromFile name
              Just m -> pure $ Right $ name /\ m
        )
        names
  where
  metadataFromFile :: PackageName -> Spago (LogEnv ()) (Either String (Tuple PackageName Metadata))
  metadataFromFile pkgName = do
    let metadataFilePath = Path.concat [ Paths.registryPath, Registry.Constants.metadataDirectory, PackageName.print pkgName <> ".json" ]
    logDebug $ "Reading metadata from file: " <> metadataFilePath
    liftAff (FS.readJsonFile Metadata.codec metadataFilePath) >>= case _ of
      Left e -> pure $ Left e
      Right m -> do
        -- memoize it if found
        liftEffect (Db.insertMetadata db pkgName m)
        pure $ Right (pkgName /\ m)

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

listPackageSetsImpl :: Spago (PreRegistryEnv _) (Array Db.PackageSet)
listPackageSetsImpl = do
  { db } <- ask
  liftEffect $ Db.selectPackageSets db

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
        pure false

--------------------------------------------------------------------------------
-- | Registry operations
--------------------------------------------------------------------------------

data JobType = Publish | Transfer | Unpublish

printJobType :: JobType -> String
printJobType = case _ of
  Publish -> "Publish"
  Transfer -> "Transfer"
  Unpublish -> "Unpublish"

submitRegistryOperation :: Operation.PackageOperation -> Spago (GitEnv _) Unit
submitRegistryOperation payload = do
  { jobId, jobType } <- case payload of
    Operation.Publish publishData -> do
      { jobId } <- callRegistry (baseApi <> Duplex.print V1.routes V1.Publish) V1.jobCreatedResponseCodec (Just { codec: Operation.publishCodec, data: publishData })
      pure { jobId, jobType: Publish }
    Operation.Authenticated authedData@{ payload: Operation.Transfer _ } -> do
      { jobId } <- callRegistry (baseApi <> Duplex.print V1.routes V1.Transfer) V1.jobCreatedResponseCodec (Just { codec: Operation.authenticatedCodec, data: authedData })
      pure { jobId, jobType: Transfer }
    Operation.Authenticated authedData@{ payload: Operation.Unpublish _ } -> do
      { jobId } <- callRegistry (baseApi <> Duplex.print V1.routes V1.Unpublish) V1.jobCreatedResponseCodec (Just { codec: Operation.authenticatedCodec, data: authedData })
      pure { jobId, jobType: Unpublish }
  logSuccess $ "Registry accepted the " <> printJobType jobType <> " request and is processing..."
  logDebug $ "Job ID: " <> unwrap jobId
  logInfo "Logs from the Registry pipeline:"
  waitForJobFinish { jobId, jobType }

callRegistry :: forall env a b. String -> CJ.Codec b -> Maybe { codec :: CJ.Codec a, data :: a } -> Spago (GitEnv env) b
callRegistry url outputCodec maybeInput = handleError do
  logDebug $ "Calling registry at " <> url
  response <- liftAff $ withBackoff' $ try case maybeInput of
    Just { codec: inputCodec, data: input } -> Http.fetch url
      { method: Http.POST
      , headers: { "Content-Type": "application/json" }
      , body: Json.stringifyJson inputCodec input
      }
    Nothing -> Http.fetch url { method: Http.GET }
  case response of
    Nothing -> pure $ Left $ "Could not reach the registry at " <> url
    Just (Left err) -> pure $ Left $ "Error while calling the registry:\n  " <> Exception.message err
    Just (Right { status, text }) | status /= 200 -> do
      bodyText <- liftAff text
      pure $ Left $ "Registry did not like this and answered with status " <> show status <> ", got answer:\n  " <> bodyText
    Just (Right { json }) -> do
      jsonBody <- Json.unsafeFromForeign <$> liftAff json
      pure $ case CJ.decode outputCodec jsonBody of
        Right output -> Right output
        Left err -> Left $ "Could not parse response from the registry, error: " <> show err
  where
  -- TODO: see if we want to just kill the process generically here, or give out customized errors
  handleError a = do
    { offline } <- ask
    case offline of
      Offline -> die "Spago is offline - not able to call the Registry."
      _ -> a >>= case _ of
        Left err -> die err
        Right res -> pure res

waitForJobFinish :: forall env. { jobId :: V1.JobId, jobType :: JobType } -> Spago (GitEnv env) Unit
waitForJobFinish { jobId, jobType } = go Nothing
  where
  go :: Maybe DateTime -> Spago (GitEnv env) Unit
  go lastTimestamp = do
    { logOptions } <- ask
    let
      url = baseApi <> Duplex.print V1.routes
        ( V1.Job jobId
            { since: lastTimestamp
            , level: case logOptions.verbosity of
                LogVerbose -> Just V1.Debug
                _ -> Just V1.Info
            }
        )
    jobInfo :: V1.Job <- callRegistry url V1.jobCodec Nothing
    -- first of all, print all the logs we get
    for_ jobInfo.logs \log -> do
      let line = indent $ toDoc $ DateTime.format Internal.Format.iso8601DateTime log.timestamp <> " " <> log.message
      case log.level of
        V1.Debug -> logDebug line
        V1.Info -> logInfo line
        V1.Warn -> logWarn line
        V1.Error -> logError line
    case jobInfo.finishedAt of
      Nothing -> do
        -- If the job is not finished, we grab the timestamp of the last log line, wait a bit and retry
        let
          latestTimestamp = jobInfo.logs # Array.last # case _ of
            Just log -> Just log.timestamp
            Nothing -> lastTimestamp
        liftAff $ Aff.delay $ Milliseconds 500.0
        go latestTimestamp
      Just _finishedAt -> do
        -- if it's done we report the failure.
        logDebug $ "Job: " <> printJson V1.jobCodec jobInfo
        case jobInfo.success of
          false -> die $ toDoc
            [ "Registry finished processing the package, but it failed."
            , "If this was due to the package not meeting the requirements, you can find more info in the logs above, and try again."
            , "If you think this was a mistake, please report it to the core team: https://github.com/purescript/registry-dev/issues"
            ]
          true -> do
            let
              verb = case jobType of
                Publish -> "published"
                Transfer -> "transferred"
                Unpublish -> "unpublished"
            logSuccess $ "Registry finished processing the package. Your package was " <> verb <> " successfully!"
            -- TODO: I am not sure why, but this is needed to make the process exit, otherwise Spago will hang
            liftEffect $ Process.exit

baseApi :: String
baseApi = "https://registry.purescript.org"
