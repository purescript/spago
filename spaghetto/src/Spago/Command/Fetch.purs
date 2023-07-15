module Spago.Command.Fetch
  ( run
  , getWorkspacePackageDeps
  , getTransitiveDeps
  , FetchEnv
  , FetchEnvRow
  , FetchOpts
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
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version as Registry.Version
import Registry.Version as Version
import Spago.Command.Repl as Repl
import Spago.Config (Dependencies(..), GitPackage, LockfileSettings(..), Package(..), PackageSet, Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.FS as FS
import Spago.Git as Git
import Spago.Lock (LockEntry(..))
import Spago.Lock as Lock
import Spago.Paths as Paths
import Spago.Tar as Tar

type FetchEnvRow a =
  ( getManifestFromIndex :: PackageName -> Version -> Spago (LogEnv ()) (Maybe Manifest)
  , getMetadata :: PackageName -> Spago (LogEnv ()) (Either String Metadata)
  , getCachedIndex :: Effect ManifestIndex
  , workspace :: Workspace
  , logOptions :: LogOptions
  , git :: Git.Git
  | a
  )

type FetchEnv a = Record (FetchEnvRow a)

type FetchOpts =
  { packages :: Array PackageName
  , ensureRanges :: Boolean
  }

run :: forall a. FetchOpts -> Spago (FetchEnv a) PackageSet
run { packages, ensureRanges } = do
  logDebug $ "Requested to install these packages: " <> printJson (CA.array PackageName.codec) packages

  { getMetadata, workspace, logOptions } <- ask

  -- lookup the dependencies in the package set, so we get their version numbers
  let
    (Dependencies deps) = case workspace.selected of
      Just selected -> getWorkspacePackageDeps selected
      Nothing ->
        -- get all the dependencies of all the workspace packages if none was selected
        foldMap getWorkspacePackageDeps (Config.getWorkspacePackages workspace.packageSet)

  -- here get transitive packages
  -- TODO: here we are throwing away the ranges from the config, but we should care about them
  transitivePackages <- getTransitiveDeps $ (Set.toUnfoldable $ Map.keys deps) <> packages

  -- write to the config file if we are adding new packages
  let
    { configPath, yamlDoc } = case workspace.selected of
      Nothing -> { configPath: "spago.yaml", yamlDoc: workspace.doc }
      Just { path, doc } -> { configPath: Path.concat [ path, "spago.yaml" ], yamlDoc: doc }
  unless (Array.null packages) do
    logInfo $ "Adding " <> show (Array.length packages) <> " packages to the config in " <> configPath
    liftEffect $ Config.addPackagesToConfig yamlDoc packages
    liftAff $ FS.writeYamlDocFile configPath yamlDoc

  -- TODO: add ranges
  -- if the flag is selected, we kick off the process of adding ranges to the config
  when ensureRanges do
    logInfo $ "Adding ranges to dependencies to the config in " <> configPath
    let rangeMap = map getRangeFromPackage transitivePackages
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
      fromWorkspacePackage { path, package } = Tuple package.name { path, dependencies, test_dependencies }
        where
        dependencies = package.dependencies
        test_dependencies = foldMap _.dependencies package.test

      lockfileWorkspace :: Lock.WorkspaceLock
      lockfileWorkspace =
        { package_set: workspace.originalConfig.package_set
        , packages: Map.fromFoldable
            $ map fromWorkspacePackage (Config.getWorkspacePackages workspace.packageSet)
        , extra_packages: fromMaybe Map.empty workspace.originalConfig.extra_packages
        }

    (lockfilePackages :: Map PackageName Lock.LockEntry) <- Map.catMaybes <$>
      forWithIndex transitivePackages \packageName package -> do
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
  let transitivePackages' = Map.union transitivePackages (Repl.supportPackage workspace.packageSet)

  parallelise $ (flip map) (Map.toUnfoldable transitivePackages' :: Array (Tuple PackageName Package)) \(Tuple name package) -> do
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
            case tarExists, tarIsGood of
              true, true -> pure unit -- Tar exists and is good, and we already unpacked it. Happy days!
              _, _ -> do
                logInfo $ "Fetching package " <> packageVersion
                response <- liftAff $ Http.request
                  ( Http.defaultRequest
                      { method = Left Method.GET
                      , responseFormat = Response.arrayBuffer
                      , url = "https://packages.registry.purescript.org/" <> PackageName.print name <> "/" <> versionString <> ".tar.gz"
                      }
                  )
                case response of
                  Left err -> die $ "Couldn't fetch package " <> packageVersion <> ":\n  " <> Http.printError err
                  Right { status, body } | status /= StatusCode 200 -> do
                    (buf :: Buffer) <- liftEffect $ Buffer.fromArrayBuffer body
                    bodyString <- liftEffect $ Buffer.toString Encoding.UTF8 buf
                    die $ "Couldn't fetch package " <> packageVersion <> ", status was not ok " <> show status <> ", got answer:\n  " <> bodyString
                  Right r@{ body: archiveArrayBuffer } -> do
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

  pure transitivePackages

getGitPackageInLocalCache :: forall a. PackageName -> GitPackage -> Spago (Git.GitEnv a) Unit
getGitPackageInLocalCache name package = do
  let localPackageLocation = Config.getPackageLocation name (GitPackage package)
  tempDir <- mkTemp' (Just $ printJson Config.gitPackageCodec package)
  logDebug $ "Cloning repo in " <> tempDir
  Git.fetchRepo package tempDir
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

-- | Return the transitive dependencies of a list of packages
getTransitiveDeps :: forall a. Array PackageName -> Spago (FetchEnv a) (Map PackageName Package)
getTransitiveDeps deps = do
  logDebug "Getting transitive deps"
  { workspace } <- ask
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

    mergeResults :: TransitiveDepsResult -> TransitiveDepsResult -> TransitiveDepsResult
    mergeResults r1 r2 =
      { packages: Map.union r1.packages r2.packages
      , errors: r1.errors <> r2.errors
      }

    go :: Set PackageName -> PackageName -> StateT (Map PackageName (Map PackageName Package)) (Spago (FetchEnv a)) TransitiveDepsResult
    go seen dep =
      if (Set.member dep seen) then do
        pure (init { errors { cycle = Set.singleton dep } })
      else do
        cache <- State.get
        case Map.lookup dep cache of
          Just allDeps ->
            pure (init { packages = allDeps })
          Nothing ->
            -- First look for the package in the set to get a version number out,
            -- then use that version to look it up in the index and get the dependencies
            case Map.lookup dep workspace.packageSet of
              Nothing -> pure (init { errors { notInPackageSet = Set.singleton dep } })
              Just package -> do
                maybeDeps <- State.lift $ memoisedGetPackageDependencies dep package
                case maybeDeps of
                  Nothing -> pure (init { errors { notInIndex = Set.singleton dep } })
                  Just dependenciesMap -> do
                    -- recur here, as we need to get the transitive tree, not just the first level
                    { packages: childDeps, errors } <-
                      -- TODO: we are just ignoring the ranges for now, but we should use them for solving if we don't get a package set
                      for (Map.toUnfoldable dependenciesMap :: Array (Tuple PackageName Range)) (\(Tuple d _) -> go (Set.insert dep seen) d)
                        >>= (pure <<< foldl mergeResults init)
                    let allDeps = Map.insert dep package childDeps
                    when (Set.isEmpty (errors.cycle <> errors.notInIndex <> errors.notInPackageSet)) do
                      State.modify_ $ Map.insert dep allDeps
                    pure { packages: allDeps, errors }

  { packages, errors } <-
    for deps (\d -> State.evalStateT (go mempty d) Map.empty) >>= (pure <<< foldl mergeResults init)

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
