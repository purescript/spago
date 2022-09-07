module Main where

import Spago.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Control.Plus (empty)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), isRight)
import Data.Foldable (foldMap, for_, oneOf)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard, power)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt, effectCanceler, error, launchAff_, makeAff, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Foreign.SPDX as Registry.License
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.FS.Aff as FS
import Node.FS.Perms as Perms
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Node.Stream as Stream
import Record as Record
import Registry.API as Registry.API
import Registry.Index as Index
import Registry.Json as RegistryJson
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest(..), Metadata)
import Registry.Schema as Registry
import Registry.Version (Version)
import Registry.Version as Registry.Version
import Spago.Command.Build as Build
import Spago.Command.Bundle as Bundle
import Spago.Commands.Fetch as Fetch
import Spago.Config (Config)
import Spago.Config as Config
import Spago.FS as FS
import Spago.Git as Git
import Spago.PackageSet (Package)
import Spago.PackageSet as PackageSet
import Spago.Paths as Paths
import Spago.Prelude as Either
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Yaml as Yaml

type GlobalArgs = {}
type FetchArgs = { packages :: List String }
type InstallArgs = FetchArgs
type BuildArgs = {}
type BundleArgs =
  { minify :: Maybe Boolean
  , entrypoint :: Maybe FilePath
  , outfile :: Maybe FilePath
  , platform :: Maybe String
  }

data Command
  = Fetch FetchArgs
  | Install InstallArgs
  | Build BuildArgs
  | Bundle BundleArgs

argParser :: ArgParser Command
argParser =
  ArgParser.choose "command"
    [ ArgParser.command [ "fetch" ]
        "Downloads all of the project's dependencies"
        do
          (Fetch <$> fetchArgsParser) <* ArgParser.flagHelp
    , ArgParser.command [ "install" ]
        "Compile the project's dependencies"
        do
          (Install <$> installArgsParser) <* ArgParser.flagHelp
    , ArgParser.command [ "build" ]
        "Compile the project"
        do
          (Build <$> buildArgsParser) <* ArgParser.flagHelp
    , ArgParser.command [ "bundle" ]
        "Bundle the project in a single file"
        do
          (Bundle <$> bundleArgsParser) <* ArgParser.flagHelp
    ]
    <* ArgParser.flagHelp
    <* ArgParser.flagInfo [ "--version", "-v" ] "Show the current version" "0.0.1" -- TODO: version. Like, with an embedded build meta module

{-

    quiet       = CLI.switch "quiet" 'q' "Suppress all spago logging"
    verbose     = CLI.switch "verbose" 'v' "Enable additional debug logging, e.g. printing `purs` commands"
    veryVerbose = CLI.switch "very-verbose" 'V' "Enable more verbosity: timestamps and source locations"
    noColor     = Opts.switch (Opts.long "no-color" <> Opts.help "Log without ANSI color escape sequences")

TODO: add flag for overriding the cache location

-}

{-

    buildOptions  = BuildOptions <$> watch <*> clearScreen <*> allowIgnored <*> sourcePaths <*> srcMapFlag <*> noInstall
                    <*> pursArgs <*> depsOnly <*> beforeCommands <*> thenCommands <*> elseCommands

-}

globalArgsParser :: ArgParser GlobalArgs
globalArgsParser =
  ArgParser.fromRecord
    {
    }

fetchArgsParser :: ArgParser FetchArgs
fetchArgsParser =
  ArgParser.fromRecord
    { packages:
        ArgParser.anyNotFlag "PACKAGE"
          "Package name to add as dependency"
          # ArgParser.many
    }

installArgsParser :: ArgParser InstallArgs
installArgsParser = fetchArgsParser

buildArgsParser :: ArgParser BuildArgs
buildArgsParser = ArgParser.fromRecord {}

bundleArgsParser :: ArgParser BundleArgs
bundleArgsParser =
  ArgParser.fromRecord
    { minify:
        ArgParser.flag [ "--minify" ]
          "Minify the bundle"
          # ArgParser.boolean
          # ArgParser.optional
    , entrypoint:
        ArgParser.argument [ "--entrypoint" ]
          "The module to bundle as the entrypoint"
          # ArgParser.optional
    , outfile:
        ArgParser.argument [ "--outfile" ]
          "Destination path for the bundle"
          # ArgParser.optional
    , platform:
        ArgParser.argument [ "--platform" ]
          "The bundle platform. 'node' or 'browser'"
          # ArgParser.optional
    }

parseArgs :: Effect (Either ArgParser.ArgError Command)
parseArgs = do
  cliArgs <- Array.drop 2 <$> Process.argv
  pure $ ArgParser.parseArgs "spago"
    "PureScript package manager and build tool"
    argParser
    cliArgs

main :: Effect Unit
main =
  parseArgs >>= case _ of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right cmd -> launchAff_ case cmd of
      Fetch args -> do
        { env, packageNames } <- mkFetchEnv args
        void $ runSpago env (Fetch.run packageNames)
      Install args -> do
        { env, packageNames } <- mkFetchEnv args
        -- TODO: --no-fetch flag
        dependencies <- runSpago env (Fetch.run packageNames)
        env' <- mkBuildEnv env dependencies
        let options = { depsOnly: true }
        runSpago env' (Build.run options)
      Build args -> do
        { env, packageNames } <- mkFetchEnv { packages: mempty }
        -- TODO: --no-fetch flag
        dependencies <- runSpago env (Fetch.run packageNames)
        buildEnv <- mkBuildEnv env dependencies
        let options = { depsOnly: false }
        runSpago buildEnv (Build.run options)
      Bundle args -> do
        { env, packageNames } <- mkFetchEnv { packages: mempty }
        -- TODO: --no-fetch flag
        dependencies <- runSpago env (Fetch.run packageNames)
        -- TODO: --no-build flag
        buildEnv <- mkBuildEnv env dependencies
        let options = { depsOnly: false }
        runSpago buildEnv (Build.run options)
        { bundleEnv, bundleOptions } <- mkBundleEnv buildEnv args
        runSpago bundleEnv (Bundle.run bundleOptions)

mkBundleEnv :: forall a. Build.BuildEnv a -> BundleArgs -> Aff { bundleEnv :: Bundle.BundleEnv (), bundleOptions :: Bundle.BundleOptions }
mkBundleEnv buildEnv bundleArgs = do
  logShow bundleArgs
  let bundleEnv = { esbuild: "esbuild" } -- TODO which esbuild
  -- TODO we should also lookup some of these options in the config
  -- which is why we are not adding default on the command line, but leaving everything optional
  let minify = fromMaybe false bundleArgs.minify
  let entrypoint = fromMaybe "main.js" bundleArgs.entrypoint
  let outfile = fromMaybe "index.js" bundleArgs.outfile
  let platform = fromMaybe Bundle.PlatformNode (Bundle.parsePlatform =<< bundleArgs.platform)
  let bundleOptions = { minify, entrypoint, outfile, platform }
  pure { bundleEnv, bundleOptions }

mkBuildEnv :: forall a. Fetch.FetchEnv a -> Map PackageName Package -> Aff (Build.BuildEnv (Fetch.FetchEnvRow a))
mkBuildEnv fetchEnv dependencies = do
  -- FIXME: find executables in path, parse compiler version, etc etc
  let buildEnv = { purs: "purs", git: "git", dependencies }
  pure $ Record.union buildEnv fetchEnv

mkFetchEnv :: FetchArgs -> Aff { env :: Fetch.FetchEnv (), packageNames :: Array PackageName }
mkFetchEnv args = do
  let { right: packageNames, left: failedPackageNames } = partitionMap PackageName.parse (Array.fromFoldable args.packages)
  unless (Array.null failedPackageNames) do
    crash $ "Failed to parse some package name: " <> show failedPackageNames

  log $ "CWD: " <> Paths.cwd

  -- Take care of the caches
  FS.mkdirp Paths.globalCachePath
  FS.mkdirp Paths.localCachePath
  FS.mkdirp Paths.localCachePackagesPath
  log $ "Global cache: " <> show Paths.globalCachePath
  log $ "Local cache: " <> show Paths.localCachePath
  let registryPath = Path.concat [ Paths.globalCachePath, "registry" ]
  let registryIndexPath = Path.concat [ Paths.globalCachePath, "registry-index" ]

  -- we make a Ref for the Index so that we can memoize the lookup of packages
  -- and we don't have to read it all together
  indexRef <- liftEffect $ Ref.new (Map.empty :: Map PackageName (Map Version Manifest))
  let
    getManifestFromIndex :: PackageName -> Version -> Aff (Maybe Manifest)
    getManifestFromIndex name version = do
      indexMap <- liftEffect (Ref.read indexRef)
      case Map.lookup name indexMap of
        Just meta -> pure (Map.lookup version meta)
        Nothing -> do
          -- if we don't have it we try reading it from file
          log $ "Reading package from Index: " <> show name
          maybeManifests <- Index.readPackage registryIndexPath name
          let manifests = map (\m@(Manifest m') -> Tuple m'.version m) $ fromMaybe [] $ map NonEmptyArray.toUnfoldable maybeManifests
          let versions = Map.fromFoldable manifests
          liftEffect (Ref.write (Map.insert name versions indexMap) indexRef)
          pure (Map.lookup version versions)

  -- same deal for the metadata files
  metadataRef <- liftEffect $ Ref.new (Map.empty :: Map PackageName Metadata)
  let
    getMetadata :: PackageName -> Aff (Either String Metadata)
    getMetadata name = do
      metadataMap <- liftEffect (Ref.read metadataRef)
      case Map.lookup name metadataMap of
        Just meta -> pure (Right meta)
        Nothing -> do
          -- if we don't have it we try reading it from file
          let metadataFilePath = Registry.API.metadataFile registryPath name
          log $ "Reading metadata from file: " <> metadataFilePath
          (RegistryJson.readJsonFile metadataFilePath) >>= case _ of
            Left e -> pure (Left e)
            Right m -> do
              -- and memoize it
              liftEffect (Ref.write (Map.insert name m metadataMap) metadataRef)
              pure (Right m)

  -- clone the registry and index repo, or update them
  try (Git.fetchRepo { git: "https://github.com/purescript/registry-index.git", ref: "main" } registryIndexPath) >>= case _ of
    Right _ -> pure unit
    Left _err -> do
      log "Couldn't refresh the registry-index, will proceed anyways"
  try (Git.fetchRepo { git: "https://github.com/purescript/registry-preview.git", ref: "main" } registryPath) >>= case _ of
    Right _ -> pure unit
    Left _err -> do
      log "Couldn't refresh the registry, will proceed anyways"

  config <- Config.readConfig "spago.yaml"

  -- read in the package set
  -- TODO: try to parse that field, it might be a URL instead of a version number
  log "Reading the package set"
  let packageSetPath = Path.concat [ registryPath, "package-sets", config.packages_db.set <> ".json" ]
  liftAff (RegistryJson.readJsonFile packageSetPath) >>= case _ of
    Left err -> crash $ "Couldn't read the package set: " <> err
    Right (Registry.PackageSet registryPackageSet) -> do
      log "Read the package set from the registry"

      -- Mix in the package set the ExtraPackages from the config
      -- Note: if there are duplicate packages we prefer the ones from the extra_packages
      let
        packageSet = Map.union
          (map PackageSet.GitPackage (fromMaybe Map.empty config.packages_db.extra_packages))
          (map PackageSet.Version registryPackageSet.packages)

      pure
        { packageNames
        , env:
            { getManifestFromIndex
            , getMetadata
            , config
            , packageSet
            }
        }
