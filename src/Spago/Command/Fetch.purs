module Spago.Command.Fetch
  ( PackageTransitiveDeps
  , FetchEnv
  , FetchEnvRow
  , FetchOpts
  , toAllDependencies
  , getWorkspacePackageDeps
  , getTransitiveDeps
  , getTransitiveDepsFromRegistry
  , run
  ) where

import Spago.Prelude

import Affjax.Node as Http
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.State as State
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Either as Either
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Map as Map
import Data.Set as Set
import Effect.Ref as Ref
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.Path as Path
import Registry.Internal.Codec as Internal.Codec
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Solver as Registry.Solver
import Registry.Version as Registry.Version
import Registry.Version as Version
import Spago.Config (Dependencies(..), GitPackage, LockfileSettings(..), Package(..), PackageMap, PackageSet(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Db as Db
import Spago.FS as FS
import Spago.Git as Git
import Spago.Lock (LockEntry(..))
import Spago.Lock as Lock
import Spago.Paths as Paths
import Spago.Purs as Purs
import Spago.Repl as Repl
import Spago.Tar as Tar

type PackageTransitiveDeps = Map PackageName PackageMap

type FetchEnvRow a =
  ( getManifestFromIndex :: PackageName -> Version -> Spago (LogEnv ()) (Maybe Manifest)
  , getMetadata :: PackageName -> Spago (LogEnv ()) (Either String Metadata)
  , workspace :: Workspace
  , logOptions :: LogOptions
  , offline :: OnlineStatus
  , purs :: Purs.Purs
  , git :: Git.Git
  , db :: Db.Db
  | a
  )

type FetchEnv a = Record (FetchEnvRow a)

type FetchOpts =
  { packages :: Array PackageName
  , ensureRanges :: Boolean
  , isTest :: Boolean
  }

run
  :: forall a
   . FetchOpts
  -> Spago (FetchEnv a) PackageTransitiveDeps
run { packages: packagesToInstall, ensureRanges, isTest } = do
  logDebug $ "Requested to install these packages: " <> printJson (CA.array PackageName.codec) packagesToInstall

  { getMetadata, logOptions, workspace, offline } <- ask

  let
    installingPackages = not $ Array.null packagesToInstall

    getSelectedPackageTransitiveDeps :: WorkspacePackage -> Spago (FetchEnv a) PackageMap
    getSelectedPackageTransitiveDeps selected =
      getTransitiveDeps $ getWorkspacePackageDeps selected <> Dependencies (Map.fromFoldable $ map (_ /\ Nothing) packagesToInstall)

  -- lookup the dependencies in the package set, so we get their version numbers.
  { dependencies, transitiveDeps } <- case workspace.selected of
    Just (selected :: WorkspacePackage) -> do
      transitiveDeps <- getSelectedPackageTransitiveDeps selected
      pure
        { dependencies: Map.singleton selected.package.name transitiveDeps
        , transitiveDeps
        }
    Nothing -> do
      dependencies <- traverse getTransitiveDeps
        $ Map.fromFoldable
        $ map (\p -> Tuple p.package.name (getWorkspacePackageDeps p))
        $ Config.getWorkspacePackages workspace.packageSet
      pure
        { dependencies
        , transitiveDeps: toAllDependencies dependencies
        }

  let
    getPackageConfigPath errorMessageEnd = do
      case workspace.selected of
        Just { path, doc, package } -> pure { configPath: Path.concat [ path, "spago.yaml" ], yamlDoc: doc, package }
        Nothing -> case workspace.rootPackage of
          Just rootPackage -> do pure { configPath: "spago.yaml", yamlDoc: workspace.doc, package: rootPackage }
          Nothing -> die
            [ "No package found in the root configuration."
            , "Please use the `-p` flag to select a package " <> errorMessageEnd
            ]

  -- write to the config file if we are adding new packages
  when installingPackages do
    { configPath, package, yamlDoc } <- getPackageConfigPath "to install your packages in."
    let packageDependencies = Map.keys $ unwrap package.dependencies
    -- Prevent users from installing a circular dependency
    let packages = Array.filter (\p -> p /= package.name) packagesToInstall
    let overlappingPackages = Set.intersection packageDependencies (Set.fromFoldable packages)
    unless (Set.isEmpty overlappingPackages) do
      logWarn
        $ [ toDoc "You tried to install some packages that are already present in the configuration, proceeding anyways:" ]
        <> map (indent <<< toDoc <<< append "- " <<< PackageName.print) (Array.fromFoldable overlappingPackages)
    logInfo $ "Adding " <> show (Array.length packages) <> " packages to the config in " <> configPath
    liftEffect $ Config.addPackagesToConfig yamlDoc isTest packages
    liftAff $ FS.writeYamlDocFile configPath yamlDoc

  -- if the flag is selected, we kick off the process of adding ranges to the config
  when ensureRanges do
    { configPath, package, yamlDoc } <- getPackageConfigPath "in which to add ranges."
    logInfo $ "Adding ranges to dependencies to the config in " <> configPath
    packageDeps <- (Map.lookup package.name dependencies) `justOrDieWith`
      "Impossible: package dependencies must be in dependencies map"
    let rangeMap = map getRangeFromPackage packageDeps
    liftEffect $ Config.addRangesToConfig yamlDoc rangeMap
    liftAff $ FS.writeYamlDocFile configPath yamlDoc

  -- TODO: need to be careful about what happens when we select a single package vs the whole workspace
  -- because otherwise the lockfile will be partial.
  -- Most likely we'll want to first figure out if we want a new lockfile at all,
  -- then possibly resolve all the packages anyways, then resolve the ones for a single package
  -- (which comes for free at that point since the cache is already populated)
  lockfile <- do
    let
      fromWorkspacePackage :: WorkspacePackage -> Tuple PackageName Lock.WorkspaceLockPackage
      fromWorkspacePackage { path, package } = Tuple package.name
        { path
        , dependencies: package.dependencies
        , test_dependencies: foldMap _.dependencies package.test
        }

      lockfileWorkspace :: Lock.WorkspaceLock
      lockfileWorkspace =
        { package_set: workspace.workspaceConfig.package_set
        , packages: Map.fromFoldable
            $ map fromWorkspacePackage (Config.getWorkspacePackages workspace.packageSet)
        , extra_packages: fromMaybe Map.empty workspace.workspaceConfig.extra_packages
        }

    (lockfilePackages :: Map PackageName Lock.LockEntry) <- Map.catMaybes <$>
      forWithIndex transitiveDeps \packageName package -> do
        (packageDependencies :: Array PackageName) <- (Array.fromFoldable <<< Map.keys <<< fromMaybe Map.empty)
          <$> getPackageDependencies packageName package
        case package of
          GitPackage gitPackage -> do
            let packageLocation = Config.getPackageLocation packageName package
            Git.getRef (Just packageLocation) >>= case _ of
              Left err -> die err
              Right rev -> pure $ Just $ FromGit { rev, dependencies: packageDependencies, url: gitPackage.git, subdir: gitPackage.subdir }
          RegistryVersion version -> do
            metadata <- runSpago { logOptions } $ getMetadata packageName
            registryVersion <- case (metadata >>= (\(Metadata meta) -> Either.note "Didn't find version in the metadata file" $ Map.lookup version meta.published)) of
              Left err -> die $ "Couldn't read metadata, reason:\n  " <> err
              Right { hash: integrity } ->
                pure { version, integrity, dependencies: packageDependencies }
            pure $ Just $ FromRegistry registryVersion
          LocalPackage { path } -> pure $ Just (FromPath { path, dependencies: packageDependencies })
          WorkspacePackage _ -> pure $ Nothing

    pure { workspace: lockfileWorkspace, packages: lockfilePackages }

  let
    shouldWriteLockFile = case workspace.selected, workspace.lockfile of
      Nothing, GenerateLockfile -> true
      Nothing, (UseLockfile _) -> true
      _, _ -> false

  when shouldWriteLockFile do
    logInfo "Writing a new lockfile"
    liftAff $ FS.writeYamlFile Lock.lockfileCodec "spago.lock" lockfile

  -- then for every package we have we try to download it, and copy it in the local cache
  logInfo "Downloading dependencies..."

  -- the repl needs a support package, so we fetch it here as a sidecar
  supportPackage <- Repl.supportPackage workspace.packageSet
  let transitiveDeps' = Map.union transitiveDeps supportPackage

  parallelise $ (flip map) (Map.toUnfoldable transitiveDeps' :: Array (Tuple PackageName Package)) \(Tuple name package) -> do
    let localPackageLocation = Config.getPackageLocation name package
    -- first of all, we check if we have the package in the local cache. If so, we don't even do the work
    unlessM (FS.exists localPackageLocation) case package of
      GitPackage gitPackage -> getGitPackageInLocalCache name gitPackage
      RegistryVersion v -> do
        -- if the version comes from the registry then we have a longer list of things to do
        let versionString = Registry.Version.print v
        let packageVersion = PackageName.print name <> "@" <> versionString
        -- get the metadata for the package, so we have access to the hash and other info
        metadata <- runSpago { logOptions } $ getMetadata name
        case (metadata >>= (\(Metadata meta) -> Either.note "Didn't find version in the metadata file" $ Map.lookup v meta.published)) of
          Left err -> die $ "Couldn't read metadata, reason:\n  " <> err
          Right versionMetadata -> do
            logDebug $ "Metadata read: " <> printJson Metadata.publishedMetadataCodec versionMetadata
            -- then check if we have a tarball cached. If not, download it
            let globalCachePackagePath = Path.concat [ Paths.globalCachePath, "packages", PackageName.print name ]
            let archivePath = Path.concat [ globalCachePackagePath, versionString <> ".tar.gz" ]
            FS.mkdirp globalCachePackagePath
            -- We need to see if the tarball is there, and if we can decompress it.
            -- This is because if Spago is killed while it's writing the tar, then it might leave it corrupted.
            -- By checking that it's broken we can try to redownload it here.
            tarExists <- FS.exists archivePath
            -- unpack the tars in a temp folder, then move to local cache
            let tarInnerFolder = PackageName.print name <> "-" <> Version.print v
            tempDir <- mkTemp
            FS.mkdirp tempDir
            tarIsGood <-
              if tarExists then do
                logDebug $ "Trying to unpack archive to temp folder: " <> tempDir
                map (either (const false) (const true)) $ liftEffect $ Tar.extract { filename: archivePath, cwd: tempDir }
              else
                pure false
            case tarExists, tarIsGood, offline of
              true, true, _ -> pure unit -- Tar exists and is good, and we already unpacked it. Happy days!
              _, _, Offline -> die $ "Package " <> packageVersion <> " is not in the local cache, and Spago is running in offline mode - can't make progress."
              _, _, Online -> do
                let packageUrl = "https://packages.registry.purescript.org/" <> PackageName.print name <> "/" <> versionString <> ".tar.gz"
                logInfo $ "Fetching package " <> packageVersion
                response <- liftAff $ withBackoff' $ Http.request
                  ( Http.defaultRequest
                      { method = Left Method.GET
                      , responseFormat = Response.arrayBuffer
                      , url = packageUrl
                      }
                  )
                case response of
                  Nothing -> die $ "Couldn't reach the registry at " <> packageUrl
                  Just (Left err) -> die $ "Couldn't fetch package " <> packageVersion <> ":\n  " <> Http.printError err
                  Just (Right { status, body }) | status /= StatusCode 200 -> do
                    (buf :: Buffer) <- liftEffect $ Buffer.fromArrayBuffer body
                    bodyString <- liftEffect $ Buffer.toString Encoding.UTF8 buf
                    die $ "Couldn't fetch package " <> packageVersion <> ", status was not ok " <> show status <> ", got answer:\n  " <> bodyString
                  Just (Right r@{ body: archiveArrayBuffer }) -> do
                    logDebug $ "Got status: " <> show r.status
                    -- check the size and hash of the tar against the metadata
                    archiveBuffer <- liftEffect $ Buffer.fromArrayBuffer archiveArrayBuffer
                    archiveSize <- liftEffect $ Buffer.size archiveBuffer
                    archiveSha <- liftEffect $ Sha256.hashBuffer archiveBuffer
                    unless (Int.toNumber archiveSize == versionMetadata.bytes) do
                      die $ "Archive fetched for " <> packageVersion <> " has a different size (" <> show archiveSize <> ") than expected (" <> show versionMetadata.bytes <> ")"
                    unless (archiveSha == versionMetadata.hash) do
                      die $ "Archive fetched for " <> packageVersion <> " has a different hash (" <> Sha256.print archiveSha <> ") than expected (" <> Sha256.print versionMetadata.hash <> ")"
                    -- if everything's alright we stash the tar in the global cache
                    logDebug $ "Fetched archive for " <> packageVersion <> ", saving it in the global cache: " <> archivePath
                    FS.writeFile archivePath archiveBuffer
                    logDebug $ "Unpacking archive to temp folder: " <> tempDir
                    (liftEffect $ Tar.extract { filename: archivePath, cwd: tempDir }) >>= case _ of
                      Right _ -> pure unit
                      Left err -> die [ "Failed to decode downloaded package " <> packageVersion <> ", error:", show err ]
            logDebug $ "Moving extracted file to local cache:" <> localPackageLocation
            FS.moveSync { src: (Path.concat [ tempDir, tarInnerFolder ]), dst: localPackageLocation }
      -- Local package, no work to be done
      LocalPackage _ -> pure unit
      WorkspacePackage _ -> pure unit

  pure dependencies

toAllDependencies :: PackageTransitiveDeps -> PackageMap
toAllDependencies = foldl (Map.unionWith (\l _ -> l)) Map.empty

getGitPackageInLocalCache :: forall a. PackageName -> GitPackage -> Spago (Git.GitEnv a) Unit
getGitPackageInLocalCache name package = do
  let localPackageLocation = Config.getPackageLocation name (GitPackage package)
  tempDir <- mkTemp' (Just $ printJson Config.gitPackageCodec package)
  logDebug $ "Cloning repo in " <> tempDir
  Git.fetchRepo package tempDir >>= case _ of
    Left err -> die err
    Right _ -> do
      logDebug $ "Repo cloned. Moving to " <> localPackageLocation
      FS.mkdirp $ Path.concat [ Paths.localCachePackagesPath, PackageName.print name ]
      FS.moveSync { src: tempDir, dst: localPackageLocation }

getPackageDependencies :: forall a. PackageName -> Package -> Spago (FetchEnv a) (Maybe (Map PackageName Range))
getPackageDependencies packageName package = case package of
  RegistryVersion v -> do
    { getManifestFromIndex, logOptions } <- ask
    maybeManifest <- runSpago { logOptions } $ getManifestFromIndex packageName v
    pure $ map (_.dependencies <<< unwrap) maybeManifest
  GitPackage p -> do
    -- Note: we get the package in local cache nonetheless,
    -- so we have guarantees about being able to fetch it
    let packageLocation = Config.getPackageLocation packageName package
    unlessM (FS.exists packageLocation) do
      getGitPackageInLocalCache packageName p
    case p.dependencies of
      Just (Dependencies dependencies) -> pure (Just (map (fromMaybe Config.widestRange) dependencies))
      Nothing -> do
        readLocalDependencies case p.subdir of
          Nothing -> packageLocation
          Just s -> Path.concat [ packageLocation, s ]
  LocalPackage p -> do
    readLocalDependencies p.path
  WorkspacePackage p ->
    pure (Just (map (fromMaybe Config.widestRange) (unwrap $ getWorkspacePackageDeps p)))
  where
  -- try to see if the package has a spago config, and if it's there we read it
  readLocalDependencies :: FilePath -> Spago (FetchEnv a) (Maybe (Map PackageName Range))
  readLocalDependencies configLocation = do
    -- TODO: make this work with manifests
    Config.readConfig (Path.concat [ configLocation, "spago.yaml" ]) >>= case _ of
      Right { yaml: { package: Just { dependencies: (Dependencies deps) } } } -> do
        pure (Just (map (fromMaybe Config.widestRange) deps))
      Right _ -> die [ "Read valid configuration from " <> configLocation, "However, there was no `package` section to be read." ]
      Left err -> die [ "Could not read config at " <> configLocation, "Error: " <> err ]

getWorkspacePackageDeps :: WorkspacePackage -> Dependencies
getWorkspacePackageDeps pkg =
  if pkg.hasTests then
    pkg.package.dependencies <> fromMaybe mempty (map _.dependencies pkg.package.test)
  else pkg.package.dependencies

type TransitiveDepsResult =
  { packages :: Map PackageName Package
  , errors ::
      { cycle :: Set PackageName
      , notInIndex :: Set PackageName
      , notInPackageSet :: Set PackageName
      }
  }

getTransitiveDeps :: forall a. Dependencies -> Spago (FetchEnv a) PackageMap
getTransitiveDeps (Dependencies deps) = do
  let depsRanges = map (fromMaybe Config.widestRange) deps
  { workspace } <- ask
  case workspace.packageSet of
    Registry extraPackages -> do
      plan <- getTransitiveDepsFromRegistry depsRanges extraPackages
      logDebug $ "Got a plan from the Solver: " <> printJson (Internal.Codec.packageMap Version.codec) plan
      pure (map RegistryVersion plan)
    PackageSet set -> getTransitiveDepsFromPackageSet set (Array.fromFoldable $ Map.keys depsRanges)

getTransitiveDepsFromRegistry :: forall a. Map PackageName Range -> PackageMap -> Spago (FetchEnv a) (Map PackageName Version)
getTransitiveDepsFromRegistry depsRanges extraPackages = do
  { logOptions, getMetadata, getManifestFromIndex } <- ask
  let
    loader :: PackageName -> Spago (FetchEnv a) (Map Version (Map PackageName Range))
    loader packageName = do
      -- First look up in the extra packages, as they are the workspace ones, and overrides
      case Map.lookup packageName extraPackages of
        Just p -> map (Map.singleton (getVersionFromPackage p) <<< fromMaybe Map.empty) $ getPackageDependencies packageName p
        Nothing -> do
          maybeMetadata <- runSpago { logOptions } (getMetadata packageName)
          let
            versions = case maybeMetadata of
              Right (Metadata metadata) -> Array.fromFoldable $ Map.keys metadata.published
              Left _err -> []
          map (Map.fromFoldable :: Array _ -> Map _ _) $ for versions \v -> do
            maybeManifest <- runSpago { logOptions } $ getManifestFromIndex packageName v
            let deps = fromMaybe Map.empty $ map (_.dependencies <<< unwrap) maybeManifest
            pure (Tuple v deps)
  maybePlan <- Registry.Solver.loadAndSolve loader depsRanges
  case maybePlan of
    Left errs -> die
      [ toDoc "Could not solve the package dependencies, errors:"
      , indent $ toDoc $ Array.fromFoldable $ map Registry.Solver.printSolverError errs
      ]
    Right (buildPlan :: Map PackageName Version) -> do
      pure buildPlan

-- | Return the transitive dependencies of a list of packages
getTransitiveDepsFromPackageSet :: forall a. PackageMap -> Array PackageName -> Spago (FetchEnv a) PackageMap
getTransitiveDepsFromPackageSet packageSet deps = do
  logDebug "Getting transitive deps"
  -- { workspace } <- ask
  packageDependenciesCache <- liftEffect $ Ref.new Map.empty
  let
    memoisedGetPackageDependencies :: PackageName -> Package -> Spago (FetchEnv a) (Maybe (Map PackageName Range))
    memoisedGetPackageDependencies packageName package = do
      cache <- liftEffect $ Ref.read packageDependenciesCache
      case Map.lookup packageName cache of
        Just cached -> pure cached
        Nothing -> do
          -- Not cached. Compute it, write to ref, return it
          res <- getPackageDependencies packageName package
          liftEffect $ Ref.modify_ (Map.insert packageName res) packageDependenciesCache
          pure res

    printPackageError :: PackageName -> String
    printPackageError p = "  - " <> PackageName.print p <> "\n"

    init :: TransitiveDepsResult
    init = { packages: (Map.empty :: Map PackageName Package), errors: mempty }

    -- | Cheap size check to be used in place of Set equality
    errorSize :: TransitiveDepsResult -> Int
    errorSize { errors: { notInPackageSet: p, notInIndex: i, cycle: c } } =
      Set.size p + Set.size i + Set.size c

    go :: Set PackageName -> PackageName -> StateT TransitiveDepsResult (Spago (FetchEnv a)) Unit
    go seen dep = do
      if Set.member dep seen then do
        State.modify_ $ cycleError dep
      else do
        alreadyRun <- Map.member dep <$> State.gets _.packages
        when (not alreadyRun)
          case Map.lookup dep packageSet of
            Nothing -> State.modify_ $ notInPackageSetError dep
            Just package -> do
              maybeDeps <- State.lift $ memoisedGetPackageDependencies dep package
              case maybeDeps of
                Nothing -> State.modify_ $ notInIndexError dep
                Just dependenciesMap -> do
                  -- Compare errors before and after recursively running transitive deps
                  errors <- errorSize <$> State.get

                  -- recur here, as we need to get the transitive tree, not just the first level
                  void $ forWithIndex dependenciesMap
                    (\dependency _ -> go (Set.insert dep seen) dependency)

                  errorsAfterTransitiveDeps <- errorSize <$> State.get

                  when (errors == errorsAfterTransitiveDeps) do
                    State.modify_ \st -> st { packages = Map.insert dep package st.packages }

  { packages, errors } <-
    State.execStateT
      (for deps (go mempty))
      init

  when (not (Set.isEmpty errors.cycle)) do
    die $ "The following packages have circular dependencies:\n" <> foldMap printPackageError (Set.toUnfoldable errors.cycle :: Array PackageName)
  when (not (Set.isEmpty errors.notInPackageSet)) do
    die $ "The following packages do not exist in your package set:\n" <> foldMap printPackageError errors.notInPackageSet
  when (not (Set.isEmpty errors.notInIndex)) do
    die $ "The following packages do not exist in the package index:\n" <> foldMap printPackageError errors.notInIndex
  pure packages

-- | Given a Package, figure out a reasonable range.
-- We default to the widest range for packages that are not pointing to the Registry.
getRangeFromPackage :: Package -> Range
getRangeFromPackage = case _ of
  RegistryVersion v -> Range.caret v
  _ -> Config.widestRange

getVersionFromPackage :: Package -> Version
getVersionFromPackage = case _ of
  RegistryVersion v -> v
  _ -> unsafeFromRight $ Version.parse "0.0.0"

notInPackageSetError :: PackageName -> TransitiveDepsResult -> TransitiveDepsResult
notInPackageSetError dep result = result
  { errors { notInPackageSet = Set.insert dep result.errors.notInPackageSet } }

notInIndexError :: PackageName -> TransitiveDepsResult -> TransitiveDepsResult
notInIndexError dep result = result
  { errors { notInIndex = Set.insert dep result.errors.notInIndex } }

cycleError :: PackageName -> TransitiveDepsResult -> TransitiveDepsResult
cycleError dep result = result
  { errors { cycle = Set.insert dep result.errors.cycle } }

