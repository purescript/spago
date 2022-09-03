module Spago.Commands.Fetch where

import Spago.Prelude

import Affjax.Node as Http
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Data.Array as Array
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
import Spago.Tar as Tar

type FetchEnv a =
  { getManifestFromIndex :: PackageName -> Version -> Aff (Maybe Manifest)
  , getMetadata :: PackageName -> Aff (Either String Metadata)
  , config :: Config
  , packageSet :: PackageSet
  , globalCachePath :: FilePath
  , localCachePath :: FilePath
  | a
  }

run :: forall a. Array PackageName -> Spago (FetchEnv a) Unit
run packages = do
  logShow packages

  { getMetadata
  , config
  , packageSet
  , globalCachePath
  , localCachePath
  } <- ask

  -- lookup the dependencies in the package set, so we get their version numbers
  let (Dependencies deps) = config.dependencies
  depsFromPackageSet <- lookupPackagesInPackageSet packageSet (Map.toUnfoldable deps)

  -- lookup all the dependencies in the index, so we get their manifests, that we can use to pull their dependencies
  { left: packagesNotInTheIndex, right: transitiveDeps } <- partitionMap identity <$> for depsFromPackageSet \{ name, package } -> getPackageDependencies name package >>= case _ of
    Nothing -> pure $ Left { name, package }
    -- TODO: this is the other place where we should care about solving the ranges: need to add a solver constraint with them
    Just deps -> pure $ Right $ [ name ] <> Array.fromFoldable (Map.keys deps)
  when (not (Array.null packagesNotInTheIndex)) do
    crash $ "Some packages were not found in the index: " <> show packagesNotInTheIndex
  let allDependencies = Set.fromFoldable $ join transitiveDeps
  log "Collected all transitive dependencies"
  logShow allDependencies

  -- solve: make sure that all the transitive deps are in the set
  -- so we, once again, run this list through the package set and get all the versions out, and fail if any are missing.
  -- FIXME: aaaaah, this is fetching only the first level of transitive deps, we need to recur
  transitivePackages <- lookupPackagesInPackageSet packageSet (map (\p -> Tuple p Nothing) $ Array.fromFoldable allDependencies)

  -- then for every package we have we try to download it, and copy it in the local cache
  -- TODO: this should all happen in parallel, and we need a process pool to limit concurrency
  void $ for transitivePackages \{ name, package } -> case package of
    GitPackage gitPackage -> do
      -- Easy, just git clone it in the local cache
      liftAff do
        -- TODO: error handling here
        Git.fetchRepo gitPackage (localCachePackagePath localCachePath name gitPackage.ref)
    Version v -> do
      -- if the version comes from the registry then we have a longer list of things to do
      -- first of all, we check if we have the package in the local cache. If so, we don't even do the work
      let versionString = Registry.Version.printVersion v
      let localVersionPath = Path.concat [ localCachePath, PackageName.print name <> "-" <> versionString ]
      unlessM (liftEffect $ FS.Sync.exists localVersionPath) do
        -- get the metadata for the package, so we have access to the hash and other info
        metadata <- liftAff (getMetadata name)
        case (metadata >>= (\meta -> Either.note "Didn't find version in the metadata file" $ Map.lookup v meta.published)) of
          Left err -> crash $ "Couldn't read metadata, reason: " <> err
          Right versionMetadata -> do
            log $ "Metadata read: " <> show versionMetadata
            -- then check if we have a tarball cached. If not, download it
            let globalCachePackagePath = Path.concat [ globalCachePath, "packages", PackageName.print name ]
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
            log $ "Unpacking tarball to local cache: " <> localVersionPath
            let localPackagesPath = Path.concat [ localCachePath, "packages" ]
            liftAff $ FS.mkdirp localPackagesPath
            liftEffect $ Tar.extract { filename: tarballPath, cwd: localPackagesPath }

getPackageDependencies :: forall a. PackageName -> PackageSet.Package -> Spago (FetchEnv a) (Maybe (Map PackageName Range))
getPackageDependencies packageName package = case package of
  Version v -> do
    { getManifestFromIndex } <- ask
    maybeManifest <- liftAff $ getManifestFromIndex packageName v
    pure $ map (_.dependencies <<< unwrap) maybeManifest
  GitPackage p -> do
    localCachePath <- asks _.localCachePath
    liftAff do
      -- TODO: error handling here
      Git.fetchRepo p (localCachePackagePath localCachePath packageName p.ref)
      -- TODO: try to see if the package has a manifest, and if not we require the package.dependencies array to be there
      let fakeRange = Either.fromRight' (\_ -> unsafeCrashWith "Fake range failed") (Registry.Version.parseRange Registry.Version.Lenient ">=0.0.0 <2147483647.0.0")
      let dependencies = Registry.Prelude.fromJust' (\_ -> unsafeCrashWith $ "Dependencies are not there: " <> show p) p.dependencies
      pure $ Just $ Map.fromFoldable $ map (\d -> Tuple d fakeRange) dependencies

lookupPackagesInPackageSet packageSet packages = do
  let
    -- TODO: we are just ignoring the ranges for now, but we should use them for solving if we don't get a package set
    { left: packagesNotInTheSet, right: packagesInTheSet } = (flip partitionMap) packages \(Tuple name maybeRange) -> case Map.lookup name packageSet of
      Nothing -> Left name
      Just package -> Right { name, package }
  when (not (Array.null packagesNotInTheSet)) do
    crash $ "Some packages were not found in the set: " <> show packagesNotInTheSet
  log "Found all the packages in the set"
  pure packagesInTheSet

localCachePackagePath localCachePath packageName ref = Path.concat [ localCachePath, "packages", PackageName.print packageName, ref ]
