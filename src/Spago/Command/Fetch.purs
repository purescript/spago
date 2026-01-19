module Spago.Command.Fetch
  ( ByEnv
  , FetchEnv
  , FetchEnvRow
  , FetchOpts
  , PackageTransitiveDeps
  , getTransitiveDeps
  , getTransitiveDepsFromRegistry
  , getWorkspacePackageDeps
  , fetchPackagesToLocalCache
  , run
  , toAllDependencies
  , writeNewLockfile
  ) where

import Spago.Prelude

import Affjax.Node as Http
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.State as State
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Either as Either
import Data.FunctorWithIndex (mapWithIndex)
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.List as List
import Data.Map as Map
import Data.Newtype (wrap)
import Data.Set as Set
import Data.String (joinWith)
import Data.Traversable (sequence, traverse_)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Ref as Ref
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Registry.Internal.Codec as Internal.Codec
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Solver as Registry.Solver
import Registry.Version as Registry.Version
import Registry.Version as Version
import Spago.Config (BuildType(..), Dependencies(..), GitPackage, Package(..), PackageMap, Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Db as Db
import Spago.FS as FS
import Spago.Git as Git
import Spago.Lock (LockEntry(..))
import Spago.Lock as Lock
import Spago.Log as Log
import Spago.Path as Path
import Spago.Paths as Paths
import Spago.Purs as Purs
import Spago.Registry as Registry
import Spago.Repl as Repl
import Spago.Tar as Tar

type PackageTransitiveDeps = Map PackageName (ByEnv PackageMap)

type FetchEnvRow a =
  ( getRegistry :: Spago (Registry.PreRegistryEnv ()) Registry.RegistryFunctions
  , workspace :: Workspace
  , logOptions :: LogOptions
  , rootPath :: Path.RootPath
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
  , isRepl :: Boolean
  }

type VersionResolution =
  { name :: PackageName
  , requested :: Range
  , resolved :: Version
  }

run :: forall a. FetchOpts -> Spago (FetchEnv a) PackageTransitiveDeps
run { packages: packagesRequestedToInstall, ensureRanges, isTest, isRepl } = do
  logDebug $ "Requested to install these packages: " <> printJson (CJ.array PackageName.codec) packagesRequestedToInstall

  { workspace: currentWorkspace, rootPath } <- ask

  let
    getPackageConfigPath errorMessageEnd = do
      res <- case currentWorkspace.selected, currentWorkspace.rootPackage of
        Just { path, doc, package }, _ ->
          pure { configPath: path </> "spago.yaml", yamlDoc: doc, package }
        _, Just rootPackage ->
          pure { configPath: rootPath </> "spago.yaml", yamlDoc: currentWorkspace.doc, package: rootPackage }
        Nothing, Nothing -> die
          [ "No package found in the root configuration."
          , "Please use the `-p` flag to select a package " <> errorMessageEnd
          ]

      pure res { yamlDoc = res.yamlDoc }

  installingPackagesData <- do
    case packagesRequestedToInstall of
      [] ->
        pure Nothing
      _ -> do
        { configPath, package, yamlDoc } <- getPackageConfigPath "to install your packages in."
        currentWorkspacePackage <- NEA.find (\p -> p.package.name == package.name) (Config.getWorkspacePackages currentWorkspace.packageSet) `justOrDieWith` "Impossible: package must be in workspace packages"
        let
          packageDependencies = Map.keys $ case isTest of
            false -> unwrap package.dependencies
            true -> unwrap $ maybe mempty _.dependencies package.test
          -- Prevent users from installing a circular dependency
          packages = Array.filter (\p -> p /= package.name) packagesRequestedToInstall
          overlappingPackages = Set.intersection packageDependencies (Set.fromFoldable packages)
          actualPackagesToInstall = Array.filter (\p -> not $ Set.member p overlappingPackages) packages
          newPackageDependencies = wrap $ Map.fromFoldable $ map (\p -> Tuple p Nothing) actualPackagesToInstall
          newWorkspacePackage = case isTest of
            false -> currentWorkspacePackage { package { dependencies = package.dependencies <> newPackageDependencies } }
            true -> currentWorkspacePackage { package { test = package.test # map (\t -> t { dependencies = t.dependencies <> newPackageDependencies }) } }
        logDebug $ "Overlapping packages: " <> printJson (CJ.Common.set PackageName.codec) overlappingPackages
        logDebug $ "Actual packages to install: " <> printJson (CJ.array PackageName.codec) actualPackagesToInstall
        -- If we are installing new packages, we need to add them to the config
        -- We also warn the user if they are already present in the config
        unless (Set.isEmpty overlappingPackages) do
          logWarn
            $ [ toDoc "You tried to install some packages that are already present in the configuration, proceeding anyways:" ]
            <> map (indent <<< toDoc <<< append "- " <<< PackageName.print) (Array.fromFoldable overlappingPackages)
        case Array.null actualPackagesToInstall of
          true -> pure Nothing
          false -> do
            logDebug $ "Packages to install: " <> printJson (CJ.array PackageName.codec) actualPackagesToInstall
            pure $ Just { configPath, yamlDoc, actualPackagesToInstall, newWorkspacePackage }

  let
    -- If we need to install new packages then we need to zero the current lockfile, we're going to need a new one
    workspace = case installingPackagesData of
      Nothing -> currentWorkspace
      Just { newWorkspacePackage } -> currentWorkspace
        { packageSet = currentWorkspace.packageSet
            { lockfile = Left "Lockfile is out of date (reason: installing new packages)"
            -- If we are installing packages, we need to add the new deps to the selected package
            , buildType = case currentWorkspace.packageSet.buildType of
                RegistrySolverBuild packageMap -> RegistrySolverBuild $ Map.insert newWorkspacePackage.package.name (WorkspacePackage newWorkspacePackage) packageMap
                PackageSetBuild info packageMap -> PackageSetBuild info $ Map.insert newWorkspacePackage.package.name (WorkspacePackage newWorkspacePackage) packageMap
            }
        , selected = Just newWorkspacePackage
        }

  local (_ { workspace = workspace }) do
    -- We compute the transitive deps for all the packages in the workspace, but keep them
    -- split by package - we need all of them so we can stash them in the lockfile, but we
    -- are going to only download the ones that we need to, if e.g. there's a package selected
    dependencies <- traverse getTransitiveDeps
      $ Map.fromFoldable
      $ map (\p -> Tuple p.package.name p)
      $ Config.getWorkspacePackages workspace.packageSet

    for_ installingPackagesData \{ configPath, yamlDoc, actualPackagesToInstall } -> do
      let
        countString = case Array.length actualPackagesToInstall of
          1 -> "1 package"
          n -> show n <> " packages"
      logInfo $ "Adding " <> countString <> " to the config in " <> Path.quote configPath
      doc <- justOrDieWith yamlDoc Config.configDocMissingErrorMessage
      liftAff $ Config.addPackagesToConfig configPath doc isTest actualPackagesToInstall

    -- if the flag is selected, we kick off the process of adding ranges to the config
    when ensureRanges do
      { configPath, package, yamlDoc } <- getPackageConfigPath "in which to add ranges."
      logInfo $ "Adding ranges to core dependencies to the config in " <> Path.quote configPath
      packageDeps <- (Map.lookup package.name dependencies) `justOrDieWith`
        "Impossible: package dependencies must be in dependencies map"
      let rangeMap = map getRangeFromPackage packageDeps.core
      doc <- justOrDieWith yamlDoc Config.configDocMissingErrorMessage
      liftEffect $ Config.addRangesToConfig doc rangeMap
      liftAff $ FS.writeYamlDocFile configPath doc

    -- the repl needs a support package, so we add it here as a sidecar
    supportPackage <- Repl.supportPackage workspace.packageSet
    let
      allTransitiveDeps = case isRepl of
        false -> dependencies
        -- It should be enough to add the support package to the core dependencies only,
        -- but that's more code, so nevermind
        true -> map (onEachEnv \packageMap -> Map.union packageMap supportPackage) dependencies
    depsToFetch <- case workspace.selected of
      Nothing -> pure (toAllDependencies allTransitiveDeps)
      -- If there's a package selected, we only fetch the transitive deps for that one
      Just p -> case Map.lookup p.package.name dependencies of
        Nothing ->
          die "Impossible: package dependencies must be in dependencies map"
        Just deps -> do
          let supportDeps = if isRepl then supportPackage else Map.empty
          pure $ Map.union supportDeps $ Map.union deps.test deps.core

    -- then for every package we have we try to download it, and copy it in the local cache
    logInfo "Downloading dependencies..."
    fetchPackagesToLocalCache depsToFetch

    -- We return the dependencies, going through the lockfile write if we need to
    -- (we return them from inside there because we need to update the commit hashes)
    case workspace.packageSet.lockfile of
      Right _lockfile -> pure dependencies
      Left reason -> writeNewLockfile reason dependencies

fetchPackagesToLocalCache :: ∀ a. Map PackageName Package -> Spago (FetchEnv a) Unit
fetchPackagesToLocalCache packages = do
  { offline } <- ask
  -- Before starting to fetch packages we build a Map of AVars to act as locks for each git location.
  -- This is so we don't have two threads trying to clone the same repo at the same time.
  gitLocks <- liftAff $ map (Map.fromFoldable <<< List.catMaybes) $ for (Map.values packages) case _ of
    GitPackage gitPackage -> (Just <<< Tuple gitPackage.git) <$> AVar.new unit
    _ -> pure Nothing
  parallelise $ packages # Map.toUnfoldable <#> \(Tuple name package) -> do
    { rootPath } <- ask
    let localPackageLocation = Config.getLocalPackageLocation rootPath name package
    -- first of all, we check if we have the package in the local cache. If so, we don't even do the work
    unlessM (FS.exists localPackageLocation) case package of
      GitPackage gitPackage -> do
        -- for git repos it's a little more involved since cloning them takes a while and we risk race conditions
        -- and possibly cloning the same repo multiple times - so we use a lock on the git url to prevent that
        let lock = Map.lookup gitPackage.git gitLocks
        -- Take the lock, do the git thing, release the lock
        liftAff $ AVar.take `traverse_` lock
        getGitPackageInLocalCache name gitPackage
        liftAff $ AVar.put unit `traverse_` lock
      RegistryVersion v -> do
        -- if the version comes from the registry then we have a longer list of things to do
        let versionString = Registry.Version.print v
        let packageVersion = PackageName.print name <> "@" <> versionString
        -- get the metadata for the package, so we have access to the hash and other info
        metadata <- Registry.getMetadata name
        case (metadata >>= (\(Metadata meta) -> Either.note "Didn't find version in the metadata file" $ Map.lookup v meta.published)) of
          Left err -> die $ "Couldn't read metadata, reason:\n  " <> err
          Right versionMetadata -> do
            logDebug $ "Metadata read: " <> printJson Metadata.publishedMetadataCodec versionMetadata
            -- then check if we have a tarball cached. If not, download it
            let globalCachePackagePath = Paths.globalCachePath </> "packages" </> PackageName.print name
            let archivePath = globalCachePackagePath </> (versionString <> ".tar.gz")
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
                logDebug $ "Trying to unpack archive to temp folder: " <> Path.quote tempDir
                map (either (const false) (const true)) $ liftEffect $ Tar.extract { filename: archivePath, cwd: tempDir }
              else
                pure false
            case tarExists, tarIsGood, offline of
              true, true, _ -> pure unit -- Tar exists and is good, and we already unpacked it. Happy days!
              _, _, Offline -> die $ "Package " <> packageVersion <> " is not in the local cache, and Spago is running in offline mode - can't make progress."
              _, _, _ -> do
                let packageUrl = "https://packages.registry.purescript.org/" <> PackageName.print name <> "/" <> versionString <> ".tar.gz"
                logInfo $ "Fetching package " <> packageVersion
                response <- liftAff $ withBackoff' do
                  res <- Http.request
                    ( Http.defaultRequest
                        { method = Left Method.GET
                        , responseFormat = Response.arrayBuffer
                        , url = packageUrl
                        }
                    )
                  -- If we get a 503, we want the backoff to kick in, so we wait here and we'll eventually be retried
                  case res of
                    Right { status } | status == StatusCode 503 -> Aff.delay (Aff.Milliseconds 30_000.0)
                    _ -> pure unit
                  pure res
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
                    logDebug $ "Fetched archive for " <> packageVersion <> ", saving it in the global cache: " <> Path.quote archivePath
                    FS.writeFile archivePath archiveBuffer
                    logDebug $ "Unpacking archive to temp folder: " <> Path.quote tempDir
                    (liftEffect $ Tar.extract { filename: archivePath, cwd: tempDir }) >>= case _ of
                      Right _ -> pure unit
                      Left err -> die [ "Failed to decode downloaded package " <> packageVersion <> ", error:", show err ]
            logDebug $ "Moving extracted file to local cache: " <> Path.quote localPackageLocation
            FS.moveSync { src: tempDir </> tarInnerFolder, dst: Path.toGlobal localPackageLocation }
      -- Local package, no work to be done
      LocalPackage _ -> pure unit
      WorkspacePackage _ -> pure unit

lookupInCache :: ∀ a k v. Ord k => k -> Ref.Ref (Map k v) -> Spago a (Maybe v)
lookupInCache key cacheRef = liftEffect $ Ref.read cacheRef >>= Map.lookup key >>> pure

withCache :: ∀ a k v. Ord k => k -> Ref.Ref (Map k v) -> Spago a v -> Spago a v
withCache key cacheRef create =
  lookupInCache key cacheRef >>= case _ of
    Just v ->
      pure v
    Nothing -> do
      v <- create
      updateCache key v cacheRef
      pure v

updateCache :: ∀ a k v. Ord k => k -> v -> Ref.Ref (Map k v) -> Spago a Unit
updateCache key value cacheRef = liftEffect $ Ref.modify_ (Map.insert key value) cacheRef

writeNewLockfile :: ∀ a. String -> PackageTransitiveDeps -> Spago (FetchEnv a) PackageTransitiveDeps
writeNewLockfile reason allTransitiveDeps = do
  logInfo $ reason <> ", generating it..."
  { workspace, rootPath } <- ask

  -- All these Refs are needed to memoise Db and file reads
  packageDependenciesCache <- liftEffect $ Ref.new Map.empty
  gitRefCache <- liftEffect $ Ref.new Map.empty
  metadataRefCache <- liftEffect $ Ref.new Map.empty

  -- Fetch the Registry metadata in one go for all required packages
  let
    allDependencies = toAllDependencies allTransitiveDeps

    uniqueRegistryPackageNames = do
      name /\ package <- Map.toUnfoldable allDependencies
      case package of
        RegistryVersion _ -> pure name
        _ -> []

  metadataMap <- Registry.getMetadataForPackages uniqueRegistryPackageNames >>= case _ of
    Left err -> die $ "Couldn't read metadata, reason:\n  " <> err
    Right ms -> pure ms

  registryVersions :: Map PackageName Sha256 <- sequence do
    allDependencies # Map.mapMaybeWithKey \name package ->
      case package of
        RegistryVersion version -> Just do
          let metadata = Map.lookup name metadataMap
          case (metadata >>= (\(Metadata meta) -> Map.lookup version meta.published)) of
            Nothing | isNothing metadata ->
              die $ "Couldn't read metadata for " <> PackageName.print name
            Nothing ->
              die $ "Couldn't read metadata for " <> PackageName.print name
                <> ": didn't find version in the metadata file"
            Just { hash: integrity } ->
              pure integrity
        _ ->
          Nothing

  let
    -- Used only as part of `packageToLockEntry`, this function returns only
    -- core dependencies of the given package, not test ones. This is because
    -- `packageToLockEntry` is used only for non-workspace packages (i.e. true
    -- dependencies of the whole workspace), for which we do not care about test
    -- dependencies.
    corePackageDepsOrEmpty packageName package =
      withCache packageName packageDependenciesCache do
        getPackageDependencies packageName package <#> case _ of
          Just deps -> Array.fromFoldable $ Map.keys deps.core
          Nothing -> []

    packageToLockEntry packageName package = case package of
      WorkspacePackage _ ->
        Nothing
      GitPackage gitPackage -> Just do
        let packageLocation = Config.getLocalPackageLocation rootPath packageName package
        withCache packageLocation gitRefCache do
          Git.getRef packageLocation >>= case _ of
            Left err ->
              die err -- TODO maybe not die here?
            Right rev -> do
              dependencies <- corePackageDepsOrEmpty packageName package
              pure $ FromGit { rev, dependencies, url: gitPackage.git, subdir: gitPackage.subdir }
      RegistryVersion version -> Just do
        withCache packageName metadataRefCache do
          FromRegistry <$> case Map.lookup packageName registryVersions of
            -- This shouldn't be Nothing because it's already handled when building the integrity map above
            Nothing ->
              die $ "Couldn't read metadata"
            Just integrity -> do
              dependencies <- corePackageDepsOrEmpty packageName package
              pure { version, integrity, dependencies }
      LocalPackage { path } -> Just do
        dependencies <- corePackageDepsOrEmpty packageName package
        pure $ FromPath { path, dependencies }

    workspacePackageLockEntries = Map.fromFoldable do
      Config.workspacePackageToLockfilePackage <$> Config.getWorkspacePackages workspace.packageSet

  -- For every non-workspace package, we convert it to its respective type of
  -- lockfile entry via `packageToLockEntry`.
  nonWorkspacePackageLockEntries <-
    sequence $ Map.mapMaybeWithKey packageToLockEntry allDependencies

  let
    lockfile =
      { packages: nonWorkspacePackageLockEntries
      , workspace:
          { package_set: case workspace.packageSet.buildType of
              RegistrySolverBuild _ -> Nothing
              PackageSetBuild info _ -> Just info
          , packages: workspacePackageLockEntries
          , extra_packages: fromMaybe Map.empty workspace.workspaceConfig.extraPackages
          }
      }
  liftAff $ FS.writeJsonFile Lock.lockfileCodec (rootPath </> "spago.lock") lockfile
  logInfo "Lockfile written to spago.lock. Please commit this file."

  -- We update the dependencies here with the commit hashes that came from the getRef calls,
  -- so that the build uses them instead of the tags
  let
    updateGitDependencyRefsToCommitHashes =
      mapWithIndex \name package -> case package of
        GitPackage gitPackage -> case Map.lookup name nonWorkspacePackageLockEntries of
          Nothing -> package
          Just (FromGit { rev }) -> GitPackage $ gitPackage { ref = rev }
          Just _ -> package
        _ ->
          package

  pure $ allTransitiveDeps <#> onEachEnv updateGitDependencyRefsToCommitHashes

-- | Given a map of several packages to their respective dependencies, collapses
-- | all dependencies into a single bucket.
toAllDependencies :: PackageTransitiveDeps -> PackageMap
toAllDependencies =
  Map.values
    >>> foldMap (\m -> [ m.core, m.test ])
    >>> foldl Map.union Map.empty

getGitPackageInLocalCache :: forall a. PackageName -> GitPackage -> Spago (FetchEnv a) Unit
getGitPackageInLocalCache name package = do
  { rootPath } <- ask
  FS.mkdirp $ rootPath </> Paths.localCachePackagesPath </> PackageName.print name
  let repoCache = rootPath </> Paths.localCacheGitPath </> Config.fileSystemCharEscape package.git

  ensureRepoCloned repoCache
  ensureRefPresent repoCache

  let localPackageLocation = Config.getLocalPackageLocation rootPath name (GitPackage package)
  logDebug $ "Copying repo to " <> Path.quote localPackageLocation
  FS.copyTree { src: repoCache, dst: localPackageLocation }
  logDebug $ "Checking out ref '" <> package.ref <> "'"
  Git.checkout { repo: localPackageLocation, ref: package.ref } >>= rightOrDie_

  -- Note: the package might have been cloned with a tag, but we stick the commit hash in the lockfiles
  -- so we need to make a copy to a location that has the commit hash too.
  -- So we run getRef here and then do a copy if the ref is different than the original one
  -- (since it might be a commit to start with)
  logDebug $ "Checking if we need to copy the package to a commit hash location..."
  commitHash <- Git.getRef localPackageLocation >>= rightOrDie
  when (commitHash /= package.ref) do
    let commitHashLocation = Config.getLocalPackageLocation rootPath name (GitPackage $ package { ref = commitHash })
    logDebug $ "Copying the repo also to " <> Path.quote commitHashLocation
    FS.copyTree { src: localPackageLocation, dst: commitHashLocation }
  where
  ensureRepoCloned repoCache = unlessM (FS.exists repoCache) do
    tempDir <- mkTemp' (Just $ printJson Config.gitPackageCodec package)
    logDebug $ "Cloning repo in " <> Path.quote tempDir
    Git.fetchRepo package tempDir >>= rightOrDie_

    logDebug $ "Repo cloned. Moving to " <> Path.quote repoCache
    FS.moveSync { src: tempDir, dst: Path.toGlobal repoCache }

  ensureRefPresent repoCache = do
    logDebug $ "Verifying ref " <> package.ref
    { offline } <- ask
    Git.getRefType { repo: repoCache, ref: package.ref } >>= case _, offline of
      Right _, _ ->
        pure unit
      Left _, Offline ->
        die $ "Repo " <> package.git <> " does not have ref " <> package.ref <> " in local cache. Cannot pull from origin in offline mode."
      Left _, _ -> do
        logDebug $ "Ref " <> package.ref <> " is not present, trying to pull from origin"
        Git.fetch { repo: repoCache, remote: "origin" } >>= rightOrDie_

getPackageDependencies :: forall a. PackageName -> Package -> Spago (FetchEnv a) (Maybe (ByEnv (Map PackageName Range)))
getPackageDependencies packageName package = case package of
  RegistryVersion v -> do
    maybeManifest <- Registry.getManifestFromIndex packageName v
    pure $ maybeManifest <#> \(Manifest m) -> { core: m.dependencies, test: Map.empty }
  GitPackage p -> do
    -- Note: we get the package in local cache nonetheless,
    -- so we have guarantees about being able to fetch it
    { rootPath } <- ask
    let packageLocation = Config.getLocalPackageLocation rootPath packageName package
    unlessM (FS.exists packageLocation) do
      getGitPackageInLocalCache packageName p
    case p.dependencies of
      Just (Dependencies dependencies) ->
        pure $ Just { core: map (fromMaybe Config.widestRange) dependencies, test: Map.empty }
      Nothing -> do
        readLocalDependencies $ Path.toGlobal $ maybe packageLocation (packageLocation </> _) p.subdir
  LocalPackage p -> do
    readLocalDependencies $ Path.global p.path
  WorkspacePackage p ->
    pure $ Just $ (map (fromMaybe Config.widestRange) <<< unwrap) `onEachEnv` getWorkspacePackageDeps p
  where
  -- try to see if the package has a spago config, and if it's there we read it
  readLocalDependencies :: GlobalPath -> Spago (FetchEnv a) (Maybe (ByEnv (Map PackageName Range)))
  readLocalDependencies configLocation = do
    -- TODO: make this work with manifests
    Config.readConfig (configLocation </> "spago.yaml") >>= case _ of
      Right { yaml: { package: Just { dependencies: Dependencies deps, test } } } ->
        pure $ Just
          { core: fromMaybe Config.widestRange <$> deps
          , test: fromMaybe Config.widestRange <$> (test <#> _.dependencies <#> unwrap # fromMaybe Map.empty)
          }
      Right _ -> die
        [ "Read the configuration at path " <> Path.quote configLocation
        , "However, it didn't contain a `package` section."
        ]
      Left errLines -> die
        [ toDoc $ "Could not lookup the dependencies of " <> PackageName.print packageName <> " in a spago.yaml config file."
        , toDoc $ "Either make sure this file exists and is readable or declare its dependencies in your project's spago.yaml under workspace.extraPackages." <> PackageName.print packageName <> ".dependencies"
        , toDoc "Error: "
        , indent $ toDoc errLines
        ]

getWorkspacePackageDeps :: WorkspacePackage -> ByEnv Dependencies
getWorkspacePackageDeps pkg =
  { core: pkg.package.dependencies
  , test: fromMaybe (Dependencies Map.empty) $
      if pkg.hasTests then _.dependencies <$> pkg.package.test else Nothing
  }

type TransitiveDepsResult =
  { packages :: Map PackageName Package
  , errors ::
      { cycle :: Set PackageName
      , notInIndex :: Set PackageName
      , notInPackageSet :: Set PackageName
      }
  }

-- | For a given workspace package, returns a list of all its transitive
-- | dependencies, but seperately for core and test.
-- |
-- | Note that test dependencies are _not transitive_. That is, if package A's
-- | tests depend on package B, it means that package A's tests transitively
-- | depend on package B's core dependencies, not test ones. Package B's test
-- | dependencies are used only for package B's own tests, not for tests of any
-- | of its consumers.
-- |
-- | For this reason, this function only picks up the core and test dependencies
-- | of the root package, and after that computes the transitive dependencies of
-- | each by traversing core dependencies of the dependencies.
-- |
-- | The traversal itself is done in either `getTransitiveDepsFromRegistry` or
-- | `getTransitiveDepsFromPackageSet`, depending on what package source the
-- | workspace is using.
getTransitiveDeps :: forall a. Config.WorkspacePackage -> Spago (FetchEnv a) (ByEnv PackageMap)
getTransitiveDeps workspacePackage = do
  let depsRanges = (map (fromMaybe Config.widestRange) <<< unwrap) `onEachEnv` getWorkspacePackageDeps workspacePackage
  { workspace } <- ask
  case workspace.packageSet.lockfile of
    -- If we have a lockfile we can compute transitive deps from the lockfile data
    Right lockfile -> do
      case Map.lookup workspacePackage.package.name lockfile.workspace.packages of
        Nothing ->
          die $ "Package " <> PackageName.print workspacePackage.package.name <> " not found in lockfile"
        Just envs ->
          pure
            { core: computeTransitiveDeps envs.core.dependencies
            , test: computeTransitiveDeps envs.test.dependencies
            }
          where
          allWorkspacePackages = Map.fromFoldable $ map (\p -> Tuple p.package.name (WorkspacePackage p)) (Config.getWorkspacePackages workspace.packageSet)

          -- Get direct dependencies of a package from the lockfile
          getDeps :: PackageName -> Set PackageName
          getDeps name = case Map.lookup name lockfile.workspace.packages of
            Just pkg -> Set.fromFoldable $ Map.keys $ unwrap pkg.core.dependencies
            Nothing -> Set.fromFoldable $ case Map.lookup name lockfile.packages of
              Just (FromPath { dependencies }) -> dependencies
              Just (FromGit { dependencies }) -> dependencies
              Just (FromRegistry { dependencies }) -> dependencies
              Nothing -> []

          transitiveClosure :: Set PackageName -> Set PackageName
          transitiveClosure initial = go initial initial
            where
            go seen new
              | Set.isEmpty new = seen
              | otherwise =
                  let
                    discovered = foldMap getDeps new `Set.difference` seen
                  in
                    go (seen <> discovered) discovered

          computeTransitiveDeps :: Dependencies -> PackageMap
          computeTransitiveDeps deps =
            let
              transitiveDeps = transitiveClosure $ Set.fromFoldable $ Map.keys $ unwrap deps

              isInTransitiveDeps :: forall v. PackageName -> v -> Boolean
              isInTransitiveDeps name _ = Set.member name transitiveDeps
            in
              Map.union
                (Map.filterWithKey isInTransitiveDeps allWorkspacePackages)
                (map fromLockEntry $ Map.filterWithKey isInTransitiveDeps lockfile.packages)

    -- No lockfile, we need to build a plan from scratch, and hit the Registry and so on
    Left _ -> case workspace.packageSet.buildType of
      RegistrySolverBuild extraPackages -> do
        let
          forEnv envName depsRanges' = do
            plan <- getTransitiveDepsFromRegistry depsRanges' extraPackages
            logDebug $ Array.fold
              [ "Got a plan from the Solver for "
              , envName
              , " deps: "
              , printJson (Internal.Codec.packageMap Version.codec) plan
              ]
            pure $ plan # Map.mapMaybeWithKey \packageName version -> case Map.lookup packageName extraPackages of
              Just p -> Just p
              Nothing -> Just $ RegistryVersion version

        { core: _, test: _ }
          <$> forEnv "core" depsRanges.core
          <*> forEnv "test" depsRanges.test

      PackageSetBuild _info set ->
        do
          packages <- depsRanges # onEachEnvM \depsRanges' ->
            getTransitiveDepsFromPackageSet set $ (Array.fromFoldable $ Map.keys depsRanges')

          let
            mergeEnvs :: ∀ k v. Ord k => ByEnv (Map k v) -> Map k v
            mergeEnvs { core, test } = Map.union core test

            resolvePackageVersionsToRanges :: Map PackageName Package -> Map PackageName Range -> Array VersionResolution
            resolvePackageVersionsToRanges registry =
              Array.fromFoldable
                <<< Map.values
                <<< Map.mapMaybeWithKey \name requested ->
                  Map.lookup name registry >>= case _ of
                    RegistryVersion resolved -> Just { name, requested, resolved }
                    _ -> Nothing

            itemisePackages :: String -> Array (Tuple PackageName String) -> Array Docc
            itemisePackages heading pairs =
              Array.cons (toDoc heading) $ pairs <#> \(Tuple name version) ->
                Log.indent <<< toDoc
                  $ "- "
                  <> PackageName.print name
                  <> ": "
                  <> version

            missingVersions =
              Array.filter
                (\{ requested, resolved } -> not $ Range.includes requested resolved)
                $ resolvePackageVersionsToRanges (mergeEnvs packages) (mergeEnvs depsRanges)

          when (Array.length missingVersions > 0) do
            logWarn
              [ itemisePackages "The following package versions do not exist in your package set:"
                  $ (\{ name, requested } -> Tuple name (Range.print requested))
                  <$> missingVersions
              , [ Log.break ]
              , itemisePackages "Proceeding with the latest available versions instead:"
                  $ (\{ name, resolved } -> Tuple name (Version.print resolved))
                  <$> missingVersions
              ]

          pure packages

  where
  -- Note: here we can safely discard the dependencies because we don't need to bother about building a build plan,
  -- we already built it when the lockfile was put together in the first place. All the dependency info is there so
  -- that other things can use it (e.g. Nix), but Spago is not going to need it at this point.
  fromLockEntry :: LockEntry -> Package
  fromLockEntry = case _ of
    FromPath { path } -> LocalPackage { path }
    FromRegistry { version } -> RegistryVersion version
    FromGit { rev, dependencies, url, subdir } -> GitPackage
      { ref: rev
      , dependencies: Just $ wrap $ Map.fromFoldable $ map (\p -> Tuple p Nothing) dependencies
      , git: url
      , subdir
      }

-- | See comments on `getTransitiveDeps`.
getTransitiveDepsFromRegistry :: forall a. Map PackageName Range -> PackageMap -> Spago (FetchEnv a) (Map PackageName Version)
getTransitiveDepsFromRegistry depsRanges extraPackages = do
  let
    loader :: PackageName -> Spago (FetchEnv a) (Map Version (Map PackageName Range))
    loader packageName = do
      -- First look up in the extra packages, as they are the workspace ones, and overrides
      case Map.lookup packageName extraPackages of
        Just p -> do
          deps <- getPackageDependencies packageName p
          let coreDeps = deps <#> _.core # fromMaybe Map.empty
          pure $ Map.singleton (getVersionFromPackage p) coreDeps
        Nothing -> do
          maybeMetadata <- Registry.getMetadata packageName
          let
            versions = case maybeMetadata of
              Right (Metadata metadata) -> Array.fromFoldable $ Map.keys metadata.published
              Left _err -> []
          map (Map.fromFoldable :: Array _ -> Map _ _) $ for versions \v -> do
            maybeManifest <- Registry.getManifestFromIndex packageName v
            let deps = fromMaybe Map.empty $ map (_.dependencies <<< unwrap) maybeManifest
            pure (Tuple v deps)

  maybePlan <- Registry.Solver.loadAndSolve loader depsRanges

  case maybePlan of
    Left errs -> die
      [ toDoc "Could not solve the package dependencies, errors:"
      , indent $ toDoc $ Array.fromFoldable $ map Registry.Solver.printSolverError errs
      ]
    Right buildPlan ->
      pure buildPlan

-- | See comments on `getTransitiveDeps`.
getTransitiveDepsFromPackageSet :: ∀ a. PackageMap -> Array PackageName -> Spago (FetchEnv a) PackageMap
getTransitiveDepsFromPackageSet packageSet deps = do
  logDebug "Getting transitive deps"
  packageDependenciesCache <- liftEffect $ Ref.new Map.empty
  let
    memoisedGetPackageDependencies :: PackageName -> Package -> Spago (FetchEnv a) (Maybe (Map PackageName Range))
    memoisedGetPackageDependencies packageName package = do
      cache <- liftEffect $ Ref.read packageDependenciesCache
      case Map.lookup packageName cache of
        Just cached -> pure cached
        Nothing -> do
          -- Not cached. Compute it, write to ref, return it
          coreDeps <- map _.core <$> getPackageDependencies packageName package
          liftEffect $ Ref.modify_ (Map.insert packageName coreDeps) packageDependenciesCache
          pure coreDeps

    printPackageError :: PackageName -> String
    printPackageError p = "  - " <> PackageName.print p <> "\n"

    printNotInPackageSetError :: PackageName -> String
    printNotInPackageSetError p = "  - " <> PackageName.print p <> strSuggestions
      where
      strSuggestions = case typoSuggestions PackageName.print p (Map.keys packageSet) of
        [] -> "\n"
        suggestions -> " (did you mean: " <> joinWith ", " (PackageName.print <$> suggestions) <> ")\n"

    init :: TransitiveDepsResult
    init = { packages: Map.empty, errors: mempty }

    go :: Set PackageName -> PackageName -> StateT TransitiveDepsResult (Spago (FetchEnv a)) Unit
    go seen dep = do
      -- We stash packages that we encountered along the way in `seen`,
      -- so if we see it again we have a cycle
      if Set.member dep seen then do
        State.modify_ $ cycleError dep
      else do
        -- If the package is a transitive dependency of some other package that
        -- we already met, then we don't need to do the work again
        alreadyRun <- Map.member dep <$> State.gets _.packages
        when (not alreadyRun)
          -- If we need to compute the dependencies from scratch instead, we first look
          -- in the package set to get a version number out, then use that version to
          -- look it up in the index and get the dependencies
          case Map.lookup dep packageSet of
            Nothing -> State.modify_ $ notInPackageSetError dep
            Just package -> do
              maybeDeps <- State.lift $ memoisedGetPackageDependencies dep package
              case maybeDeps of
                Nothing -> State.modify_ $ notInIndexError dep
                Just dependenciesMap -> do
                  -- Compare errors before and after recursively running transitive deps
                  errors <- State.gets _.errors

                  -- recur here, as we need to get the transitive tree, not just the first level
                  void $ forWithIndex dependenciesMap
                    \dependency _ -> go (Set.insert dep seen) dependency

                  -- Errors may have changed after running through the child deps
                  errorsAfterTransitiveDeps <- State.gets _.errors

                  -- Do not include the package if any child deps fail
                  when (errors == errorsAfterTransitiveDeps) do
                    State.modify_ \st -> st { packages = Map.insert dep package st.packages }

  { packages, errors } <-
    State.execStateT
      (for deps (go mempty))
      init

  when (not (Set.isEmpty errors.cycle)) do
    die $ "The following packages have circular dependencies:\n" <> foldMap printPackageError (Set.toUnfoldable errors.cycle :: Array PackageName)
  when (not (Set.isEmpty errors.notInPackageSet)) do
    die $ "The following packages do not exist in your package set:\n" <> foldMap printNotInPackageSetError errors.notInPackageSet
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

-- | When we work with dependencies, we need to keep track of them separately
-- | for core and test, because they need to be treated differently in some
-- | contexts.
type ByEnv a = { core :: a, test :: a }

onEachEnv :: ∀ a b. (a -> b) -> ByEnv a -> ByEnv b
onEachEnv f e = e { core = f e.core, test = f e.test }

onEachEnvM :: ∀ m a b. Apply m => (a -> m b) -> ByEnv a -> m (ByEnv b)
onEachEnvM f e = e { core = _, test = _ } <$> f e.core <*> f e.test

