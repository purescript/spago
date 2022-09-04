module Spago.Commands.Fetch where

import Spago.Prelude

import Affjax.Node as Http
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.State as State
import Data.Either as Either
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Map as Map
import Data.Set as Set
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.FS.Sync as FS.Sync
import Node.Path as Path
import Partial.Unsafe (unsafeCrashWith)
import Registry.Hash as Registry.Hash
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Prelude as Registry.Prelude
import Registry.Schema (Manifest, Metadata)
import Registry.Version (Range, Version)
import Registry.Version as Registry.Version
import Spago.Config (Config, Dependencies(..))
import Spago.FS as FS
import Spago.Git as Git
import Spago.PackageSet (Package(..), PackageSet)
import Spago.PackageSet as PackageSet
import Spago.Paths as Paths
import Spago.Tar as Tar

type FetchEnvRow a =
  ( getManifestFromIndex :: PackageName -> Version -> Aff (Maybe Manifest)
  , getMetadata :: PackageName -> Aff (Either String Metadata)
  , config :: Config
  , packageSet :: PackageSet
  | a
  )

type FetchEnv a = Record (FetchEnvRow a)

run :: forall a. Array PackageName -> Spago (FetchEnv a) (Map PackageName Package)
run packages = do
  logShow packages

  { getMetadata
  , config
  } <- ask

  -- lookup the dependencies in the package set, so we get their version numbers
  let (Dependencies deps) = config.dependencies

  -- here get transitive packages
  -- TODO: here we are throwing away the ranges from the config, but we should care about them
  transitivePackages <- getTransitiveDeps (Set.toUnfoldable $ Map.keys deps)

  -- then for every package we have we try to download it, and copy it in the local cache
  -- TODO: this should all happen in parallel, and we need a process pool to limit concurrency
  void $ for (Map.toUnfoldable transitivePackages :: Array (Tuple PackageName Package)) \(Tuple name package) -> do
    let localPackageLocation = Paths.getPackageLocation name package
    -- first of all, we check if we have the package in the local cache. If so, we don't even do the work
    unlessM (liftEffect $ FS.Sync.exists localPackageLocation) case package of
      GitPackage gitPackage -> do
        -- Easy, just git clone it in the local cache
        liftAff do
          -- TODO: error handling here
          Git.fetchRepo gitPackage localPackageLocation
      Version v -> do
        -- if the version comes from the registry then we have a longer list of things to do
        let versionString = Registry.Version.printVersion v
        -- get the metadata for the package, so we have access to the hash and other info
        metadata <- liftAff (getMetadata name)
        case (metadata >>= (\meta -> Either.note "Didn't find version in the metadata file" $ Map.lookup v meta.published)) of
          Left err -> crash $ "Couldn't read metadata, reason: " <> err
          Right versionMetadata -> do
            log $ "Metadata read: " <> show versionMetadata
            -- then check if we have a tarball cached. If not, download it
            let globalCachePackagePath = Path.concat [ Paths.globalCachePath, "packages", PackageName.print name ]
            let tarballPath = Path.concat [ globalCachePackagePath, versionString <> ".tar.gz" ]
            liftAff $ FS.mkdirp globalCachePackagePath
            unlessM (liftEffect $ FS.Sync.exists tarballPath) do
              response <- liftAff $ Http.request
                ( Http.defaultRequest
                    { method = Left Method.GET
                    , responseFormat = Response.arrayBuffer
                    , url = "https://packages.registry.purescript.org/" <> PackageName.print name <> "/" <> versionString <> ".tar.gz"
                    }
                )
              case response of
                Left err -> crash $ "Couldn't fetch package" <> Http.printError err
                Right { status, body } | status /= StatusCode 200 -> do
                  (buf :: Buffer) <- liftEffect $ Buffer.fromArrayBuffer body
                  bodyString <- liftEffect $ Buffer.toString Encoding.UTF8 buf
                  crash $ "Couldn't fetch package, status was not ok " <> show status <> ", got answer: " <> bodyString
                Right r@{ body: tarballArrayBuffer } -> do
                  logShow r.status
                  -- check the size and hash of the tar against the metadata
                  tarballBuffer <- liftEffect $ Buffer.fromArrayBuffer tarballArrayBuffer
                  tarballSize <- liftEffect $ Buffer.size tarballBuffer
                  tarballSha <- liftEffect $ Registry.Hash.sha256Buffer tarballBuffer
                  unless (Int.toNumber tarballSize == versionMetadata.bytes) do
                    crash $ "Fetched tarball has a different size (" <> show tarballSize <> ") than expected (" <> show versionMetadata.bytes <> ")"
                  unless (tarballSha == versionMetadata.hash) do
                    crash $ "Fetched tarball has a different hash (" <> show tarballSha <> ") than expected (" <> show versionMetadata.hash <> ")"
                  -- if everything's alright we stash the tar in the global cache
                  log $ "Copying tarball to global cache: " <> tarballPath
                  liftAff $ FS.writeFile tarballPath tarballBuffer
            -- unpack the tars in the local cache
            log $ "Unpacking tarball to local cache: " <> localPackageLocation
            liftEffect $ Tar.extract { filename: tarballPath, cwd: Paths.localCachePackagesPath }
  pure transitivePackages

getPackageDependencies :: forall a. PackageName -> PackageSet.Package -> Spago (FetchEnv a) (Maybe (Map PackageName Range))
getPackageDependencies packageName package = case package of
  Version v -> do
    { getManifestFromIndex } <- ask
    maybeManifest <- liftAff $ getManifestFromIndex packageName v
    pure $ map (_.dependencies <<< unwrap) maybeManifest
  GitPackage p -> liftAff do
    -- TODO: error handling here
    Git.fetchRepo p (Paths.getPackageLocation packageName package)
    -- TODO: try to see if the package has a manifest, and if not we require the package.dependencies array to be there
    let fakeRange = Either.fromRight' (\_ -> unsafeCrashWith "Fake range failed") (Registry.Version.parseRange Registry.Version.Lenient ">=0.0.0 <2147483647.0.0")
    let dependencies = Registry.Prelude.fromJust' (\_ -> unsafeCrashWith $ "Dependencies are not there: " <> show p) p.dependencies
    pure $ Just $ Map.fromFoldable $ map (\d -> Tuple d fakeRange) dependencies

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
  log "Getting transitive deps"
  { packageSet } <- ask
  let
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
            case Map.lookup dep packageSet of
              Nothing -> pure (init { errors { notInPackageSet = Set.singleton dep } })
              Just package -> do
                maybeDeps <- State.lift $ getPackageDependencies dep package
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
    crash $ "The following packages have circular dependencies:\n" <> foldMap printPackageError (Set.toUnfoldable errors.cycle :: Array PackageName)
  when (not (Set.isEmpty errors.notInPackageSet)) do
    crash $ "The following packages do not exist in your package set:\n" <> foldMap printPackageError errors.notInPackageSet
  when (not (Set.isEmpty errors.notInIndex)) do
    crash $ "The following packages do not exist in the package index:\n" <> foldMap printPackageError errors.notInPackageSet
  pure packages
