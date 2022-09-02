module Spago.Commands.Fetch where

import Prelude
import Spago.Prelude

import Affjax.Node as Http
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (try)
import Control.Monad.Except as Except
import Data.Array as Array
import Data.Either as Either
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Map as Map
import Data.Set as Set
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Foreign.Git as Registry
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.FS.Sync as FS.Sync
import Node.Path as Path
import Node.Process as Process
import Partial.Unsafe (unsafeCrashWith)
import Registry.API as Registry
import Registry.Hash as Registry
import Registry.Index (RegistryIndex)
import Registry.Index as Registry
import Registry.Json as RegistryJson
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Prelude as Encoding
import Registry.Prelude as Maybe
import Registry.Schema (Manifest(..), Metadata)
import Registry.Schema as Registry
import Registry.Version (Range, Version)
import Registry.Version as Registry
import Spago.Config (Config, Dependencies(..))
import Spago.Config as Config
import Spago.FS as FS
import Spago.PackageSet (Package(..))
import Spago.PackageSet as PackageSet
import Spago.Tar as Tar
import Yaml as Yaml

type FetchEnv a =
  { registryIndex :: Registry.RegistryIndex
  , globalCachePath :: FilePath
  , localCachePath :: FilePath
  | a
  }

run :: forall a. Array PackageName -> Spago (FetchEnv a) Unit
run packages = do
  logShow packages

  -- make a cache dir
  globalCachePath <- asks _.globalCachePath
  localCachePath <- asks _.localCachePath
  log $ "Global cache: " <> show globalCachePath
  log $ "Local cache: " <> show localCachePath
  let registryPath = Path.concat [ globalCachePath, "registry" ]
  let registryIndexPath = Path.concat [ globalCachePath, "registry-index" ]

  liftAff do
    -- clone the registry and index repo, or update them
    try (fetchRepo { git: "https://github.com/purescript/registry-index.git", ref: "main" } registryIndexPath) >>= case _ of
      Right _ -> pure unit
      Left _err -> do
        log "Couldn't refresh the registry-index, will proceed anyways"
    try (fetchRepo { git: "https://github.com/purescript/registry-preview.git", ref: "main" } registryPath) >>= case _ of
      Right _ -> pure unit
      Left _err -> do
        log "Couldn't refresh the registry, will proceed anyways"

  -- read the config
  log "Reading config.."
  eitherConfig :: Either String Config <- liftAff $ Yaml.readYamlFile "./spago.yaml"
  case eitherConfig of
    Left err -> crash $ "Can't read config: " <> err -- TODO: better error here
    Right conf -> do
      log "Read config:"
      log (Yaml.printYaml conf)

      -- read in the package set
      -- TODO: try to parse that field, it might be a URL instead of a version number
      log "Reading the package set"
      let packageSetPath = Path.concat [ registryPath, "package-sets", conf.packages_db.set <> ".json" ]
      liftAff (RegistryJson.readJsonFile packageSetPath) >>= case _ of
        Left err -> crash $ "Couldn't read the package set: " <> err
        Right (Registry.PackageSet registryPackageSet) -> do
          log "Read the package set from the registry"

          -- Mix in the package set the ExtraPackages from the config
          -- Note: if there are duplicate packages we prefer the ones from the extra_packages
          let
            packageSet = Map.union
              (map PackageSet.GitPackage conf.packages_db.extra_packages)
              (map PackageSet.Version registryPackageSet.packages)

          -- read registry index
          -- TODO: this is wayyyy too slow right now, we should instead read only the stuff we need
          liftAff (try $ Registry.readRegistryIndex registryIndexPath) >>= case _ of
            Left err -> crash $ "Couldn't read the registry index: " <> show err
            Right registryIndex -> do
              log "Read the index"

              -- lookup the dependencies in the package set, so we get their version numbers
              let (Dependencies deps) = conf.dependencies
              depsFromPackageSet <- lookupPackagesInPackageSet packageSet (Map.toUnfoldable deps)

              -- lookup all the dependencies in the index, so we get their manifests, that we can use to pull their dependencies
              { left: packagesNotInTheIndex, right: transitiveDeps } <- partitionMap identity <$> for depsFromPackageSet \{ name, package } -> getPackageDependencies registryIndex name package >>= case _ of
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
              transitivePackages <- lookupPackagesInPackageSet packageSet (map (\p -> Tuple p Nothing) $ Array.fromFoldable allDependencies)

              -- then for every package we have we try to download it, and copy it in the local cache
              -- TODO: this should all happen in parallel, and we need a process pool to limit concurrency
              void $ for transitivePackages \{ name, package } -> case package of
                GitPackage gitPackage -> do
                  -- Easy, just git clone it in the local cache
                  liftAff do
                    -- TODO: error handling here
                    fetchRepo gitPackage (localCachePackagePath localCachePath name gitPackage.ref)
                Version v -> do
                  -- if the version comes from the registry then we have a longer list of things to do
                  -- first of all, we check if we have the package in the local cache. If so, we don't even do the work
                  let versionString = Registry.printVersion v
                  let localVersionPath = Path.concat [ localCachePath, PackageName.print name <> "-" <> versionString ]
                  unlessM (liftEffect $ FS.Sync.exists localVersionPath) do
                    -- read the metadata for the package, so we have access to shas
                    let metadataFilePath = Registry.metadataFile registryPath name
                    log $ "Reading metadata from file: " <> metadataFilePath
                    liftAff (RegistryJson.readJsonFile metadataFilePath) >>= case _ of
                      Left err -> crash $ "Couldn't read package metadata, error: " <> err
                      Right (metadata :: Metadata) -> do
                        -- get the metadata for the version
                        let versionMetadata = Maybe.fromJust' (\_ -> unsafeCrashWith "Didn't find version in the metadata") (Map.lookup v metadata.published)
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
                              tarballSha <- liftEffect $ Registry.sha256Buffer tarballBuffer
                              -- FIXME: re-enable these
                              -- unless (Int.toNumber tarballSize == versionMetadata.bytes) do
                              --  crash $ "Fetched tarball has a different size (" <> show tarballSize <> ") than expected (" <> show versionMetadata.bytes <> ")"
                              -- unless (tarballSha == versionMetadata.hash) do
                              --  crash $ "Fetched tarball has a different hash (" <> show tarballSha <> ") than expected (" <> show versionMetadata.hash <> ")"
                              -- if everything's alright we stash the tar in the global cache
                              log $ "Copying tarball to global cache: " <> tarballPath
                              liftAff $ FS.writeFile tarballPath tarballBuffer
                        -- unpack the tars in the local cache
                        log $ "Unpacking tarball to local cache: " <> localVersionPath
                        let localPackagesPath = Path.concat [ localCachePath, "packages" ]
                        liftAff $ FS.mkdirp localPackagesPath
                        liftEffect $ Tar.extract { filename: tarballPath, cwd: localPackagesPath }

getPackageDependencies :: forall a. RegistryIndex -> PackageName -> PackageSet.Package -> Spago { localCachePath :: FilePath | a } (Maybe (Map PackageName Range))
getPackageDependencies index packageName package = case package of
  Version v -> pure $ map (_.dependencies <<< unwrap) (Map.lookup packageName index >>= Map.lookup v)
  GitPackage p -> do
    localCachePath <- asks _.localCachePath
    liftAff do
      -- TODO: error handling here
      fetchRepo p (localCachePackagePath localCachePath packageName p.ref)
      -- TODO: try to see if the package has a manifest, and if not we require the package.dependencies array to be there
      let fakeRange = Either.fromRight' (\_ -> unsafeCrashWith "Fake range failed") (Registry.parseRange Registry.Lenient ">=0.0.0 <2147483647.0.0")
      let dependencies = Maybe.fromJust' (\_ -> unsafeCrashWith $ "Dependencies are not there: " <> show p) p.dependencies
      pure $ Just $ Map.fromFoldable $ map (\d -> Tuple d fakeRange) dependencies

crash msg = do
  log msg
  liftEffect $ Process.exit 1

fetchRepo :: forall a. { git :: String, ref :: String | a } -> FilePath -> Aff Unit
fetchRepo { git, ref } path = liftEffect (FS.Sync.exists path) >>= case _ of
  true -> do
    log $ "Found the " <> git <> " repo locally, pulling..."
    result <- Except.runExceptT do
      Registry.runGit_ [ "fetch", "origin" ] (Just path)
      Registry.runGit [ "checkout", ref ] (Just path)
    case result of
      Left err -> Aff.throwError $ Aff.error err
      Right _ -> pure unit
  _ -> do
    log $ "Didn't find " <> git <> " repo, cloning..."
    Except.runExceptT (Registry.runGit [ "clone", git, path ] Nothing) >>= case _ of
      Left err -> Aff.throwError $ Aff.error err
      Right _ -> pure unit

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
