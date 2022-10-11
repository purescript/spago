module Main where

import Spago.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.List as List
import Data.Map as Map
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Flags as Flags
import Node.Process as Process
import Psa (ErrorCode, StatVerbosity)
import Registry.API as Registry.API
import Registry.Index as Index
import Registry.Json as RegistryJson
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Manifest(..), Metadata)
import Registry.Version (Version)
import Spago.BuildInfo as BuildInfo
import Spago.Command.Build as Build
import Spago.Command.Bundle as Bundle
import Spago.Commands.Fetch as Fetch
import Spago.Commands.Sources as Sources
import Spago.Config (BundleConfig, Package, Platform(..))
import Spago.Config as Config
import Spago.FS as FS
import Spago.Git as Git
import Spago.Log (LogVerbosity(..), supportsColor)
import Spago.Paths as Paths

type GlobalArgs =
  { noColor :: Boolean
  , quiet :: Boolean
  , verbose :: Boolean
  }

type FetchArgs =
  { packages :: List String
  , selectedPackage :: Maybe String
  }

type PsaArgs =
  { censorWarnings :: Boolean
  , censorLib :: Boolean
  , censorSrc :: Boolean
  , censorCodes :: Set ErrorCode
  , filterCodes :: Set ErrorCode
  , statVerbosity :: StatVerbosity
  , libDirs :: Maybe (Array String)
  , strict :: Boolean
  , ansi :: Boolean
  , showSource :: Boolean
  , stash :: Boolean
  , stashFile :: String
  }

type InstallArgs =
  { packages :: List String
  , selectedPackage :: Maybe String
  , pursArgs :: List String
  , psaArgs :: PsaArgs
  }

type BuildArgs =
  { selectedPackage :: Maybe String
  , pursArgs :: List String
  , psaArgs :: PsaArgs
  }

type SourcesArgs =
  { selectedPackage :: Maybe String
  }

type BundleArgs =
  { minify :: Maybe Boolean
  , entrypoint :: Maybe FilePath
  , outfile :: Maybe FilePath
  , platform :: Maybe String
  , selectedPackage :: Maybe String
  , pursArgs :: List String
  , psaArgs :: PsaArgs
  }

data SpagoCmd = SpagoCmd GlobalArgs Command

data Command
  = Fetch FetchArgs
  | Install InstallArgs
  | Build BuildArgs
  | Bundle BundleArgs
  | Sources SourcesArgs

argParser :: ArgParser SpagoCmd
argParser =
  ArgParser.choose "command"
    [ ArgParser.command [ "fetch" ]
        "Downloads all of the project's dependencies"
        do
          (SpagoCmd <$> globalArgsParser <*> (Fetch <$> fetchArgsParser) <* ArgParser.flagHelp)
    , ArgParser.command [ "install" ]
        "Compile the project's dependencies"
        do
          (SpagoCmd <$> globalArgsParser <*> (Install <$> installArgsParser) <* ArgParser.flagHelp)
    , ArgParser.command [ "build" ]
        "Compile the project"
        do
          (SpagoCmd <$> globalArgsParser <*> (Build <$> buildArgsParser) <* ArgParser.flagHelp)
    , ArgParser.command [ "bundle" ]
        "Bundle the project in a single file"
        do
          (SpagoCmd <$> globalArgsParser <*> (Bundle <$> bundleArgsParser) <* ArgParser.flagHelp)
    , ArgParser.command [ "sources" ]
        "List all the source paths (globs) for the dependencies of the project"
        do
          (SpagoCmd <$> globalArgsParser <*> (Sources <$> sourcesArgsParser) <* ArgParser.flagHelp)
    ]
    <* ArgParser.flagHelp
    <* ArgParser.flagInfo [ "--version" ] "Show the current version" BuildInfo.currentSpagoVersion

{-

TODO: add flag for overriding the cache location

    buildOptions  = BuildOptions <$> watch <*> clearScreen <*> allowIgnored <*> sourcePaths <*> srcMapFlag <*> noInstall
                    <*> pursArgs <*> depsOnly <*> beforeCommands <*> thenCommands <*> elseCommands

-}

-- TODO: veryVerbose = CLI.switch "very-verbose" 'V' "Enable more verbosity: timestamps and source locations"
globalArgsParser :: ArgParser GlobalArgs
globalArgsParser =
  ArgParser.fromRecord
    { quiet: Flags.quiet
    , verbose: Flags.verbose
    , noColor: Flags.noColor
    }

fetchArgsParser :: ArgParser FetchArgs
fetchArgsParser =
  ArgParser.fromRecord
    { packages: Flags.packages
    , selectedPackage: Flags.selectedPackage
    }

sourcesArgsParser :: ArgParser SourcesArgs
sourcesArgsParser =
  ArgParser.fromRecord
    { selectedPackage: Flags.selectedPackage
    }

installArgsParser :: ArgParser InstallArgs
installArgsParser =
  ArgParser.fromRecord
    { packages: Flags.packages
    , selectedPackage: Flags.selectedPackage
    , pursArgs: Flags.pursArgs
    , psaArgs: psaArgsParser
    }

buildArgsParser :: ArgParser BuildArgs
buildArgsParser = ArgParser.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , psaArgs: psaArgsParser
  }

bundleArgsParser :: ArgParser BundleArgs
bundleArgsParser =
  ArgParser.fromRecord
    { minify: Flags.minify
    , entrypoint: Flags.entrypoint
    , outfile: Flags.outfile
    , platform: Flags.platform
    , selectedPackage: Flags.selectedPackage
    , pursArgs: Flags.pursArgs
    , psaArgs: psaArgsParser
    }

psaArgsParser :: ArgParser PsaArgs
psaArgsParser = ArgParser.fromRecord
  { censorWarnings: Flags.psaCensorWarnings
  , censorLib: Flags.psaCensorLib
  , censorSrc: Flags.psaCensorSrc
  , censorCodes: Flags.psaCensorCodes
  , filterCodes: Flags.psaFilterCodes
  , statVerbosity: Flags.psaStatVerbosity
  , libDirs: Flags.psaLibDirs
  , strict: Flags.psaStrict
  , ansi: Flags.psaAnsi
  , showSource: Flags.psaShowSource
  , stash: Flags.psaStash
  , stashFile: Flags.psaStashFile
  }

parseArgs :: Effect (Either ArgParser.ArgError SpagoCmd)
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
    Right c -> Aff.launchAff_ case c of
      SpagoCmd globalArgs command -> do
        logOptions <- mkLogOptions globalArgs
        runSpago { logOptions } case command of
          Sources args -> do
            { env } <- mkFetchEnv { packages: mempty, selectedPackage: args.selectedPackage }
            void $ runSpago env Sources.run
          Fetch args -> do
            { env, packageNames } <- mkFetchEnv args
            void $ runSpago env (Fetch.run packageNames)
          Install args@{ packages, selectedPackage } -> do
            { env, packageNames } <- mkFetchEnv { packages, selectedPackage }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run packageNames)
            env' <- runSpago env (mkBuildEnv dependencies)
            let options = { depsOnly: true, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago env' (Build.run options)
          Build args -> do
            { env, packageNames } <- mkFetchEnv { packages: mempty, selectedPackage: args.selectedPackage }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run packageNames)
            buildEnv <- runSpago env (mkBuildEnv dependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago buildEnv (Build.run options)
          Bundle args -> do
            { env, packageNames } <- mkFetchEnv { packages: mempty, selectedPackage: args.selectedPackage }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run packageNames)
            -- TODO: --no-build flag
            buildEnv <- runSpago env (mkBuildEnv dependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago buildEnv (Build.run options)
            { bundleEnv, bundleOptions } <- runSpago env (mkBundleEnv args)
            runSpago bundleEnv (Bundle.run bundleOptions)

mkLogOptions :: GlobalArgs -> Aff LogOptions
mkLogOptions { noColor, quiet, verbose } = do
  supports <- liftEffect supportsColor
  let color = and [ supports, not noColor ]
  let
    verbosity =
      if quiet then
        LogQuiet
      else if verbose then
        LogVerbose
      else LogNormal
  pure { color, verbosity }

mkBundleEnv :: forall a. BundleArgs -> Spago (Fetch.FetchEnv a) { bundleEnv :: (Bundle.BundleEnv ()), bundleOptions :: Bundle.BundleOptions }
mkBundleEnv bundleArgs = do
  { workspace, logOptions } <- ask
  logDebug $ "Bundle args: " <> show bundleArgs

  selected <- case workspace.selected of
    Just s -> pure s
    Nothing ->
      let
        workspacePackageNames = map _.package.name (Config.getWorkspacePackages workspace.packageSet)
      in
        die [ toDoc "No package was selected for bundling. Please select (with -p) one of the following packages:", indent (toDoc workspacePackageNames) ]

  logDebug $ "Selected package to bundle: " <> show selected.package.name

  -- the reason why we don't have a default on the CLI is that we look some of these
  -- up in the config - though the flags take precedence
  let
    bundleConf :: forall x. (BundleConfig -> Maybe x) -> Maybe x
    bundleConf f = selected.package.bundle >>= f
  -- TODO: there should be no defaults here actually?
  let minify = fromMaybe false (bundleArgs.minify <|> bundleConf _.minify)
  let entrypoint = fromMaybe "main.js" (bundleArgs.entrypoint <|> bundleConf _.entrypoint)
  let outfile = fromMaybe "index.js" (bundleArgs.outfile <|> bundleConf _.outfile)
  let
    platform = fromMaybe PlatformNode
      ( (Config.parsePlatform =<< bundleArgs.platform)
          <|> bundleConf _.platform
      )
  let bundleOptions = { minify, entrypoint, outfile, platform }
  let bundleEnv = { esbuild: "esbuild", logOptions, workspace, selected } -- TODO: which esbuild
  pure { bundleOptions, bundleEnv }

mkBuildEnv :: forall a. Map PackageName Package -> Spago (Fetch.FetchEnv a) (Build.BuildEnv ())
mkBuildEnv dependencies = do
  { logOptions, workspace } <- ask
  -- FIXME: find executables in path, parse compiler version, etc etc
  pure { logOptions, purs: "purs", git: "git", dependencies, workspace }

mkFetchEnv :: forall a. FetchArgs -> Spago (LogEnv a) { env :: Fetch.FetchEnv (), packageNames :: Array PackageName }
mkFetchEnv args = do
  let { right: packageNames, left: failedPackageNames } = partitionMap PackageName.parse (Array.fromFoldable args.packages)
  unless (Array.null failedPackageNames) do
    die $ "Failed to parse some package name: " <> show failedPackageNames

  -- TODO: refactor this
  maybeSelectedPackage <- case map PackageName.parse args.selectedPackage of
    Nothing -> pure Nothing
    Just (Left _err) -> die $ "Failed to parse selected package name, was: " <> show args.selectedPackage
    Just (Right p) -> pure (Just p)

  logDebug $ "CWD: " <> Paths.cwd

  -- Take care of the caches
  liftAff do
    FS.mkdirp Paths.globalCachePath
    FS.mkdirp Paths.localCachePath
    FS.mkdirp Paths.localCachePackagesPath
  logDebug $ "Global cache: " <> show Paths.globalCachePath
  logDebug $ "Local cache: " <> show Paths.localCachePath

  -- we make a Ref for the Index so that we can memoize the lookup of packages
  -- and we don't have to read it all together
  indexRef <- liftEffect $ Ref.new (Map.empty :: Map PackageName (Map Version Manifest))
  let
    getManifestFromIndex :: PackageName -> Version -> Spago (LogEnv ()) (Maybe Manifest)
    getManifestFromIndex name version = do
      indexMap <- liftEffect (Ref.read indexRef)
      case Map.lookup name indexMap of
        Just meta -> pure (Map.lookup version meta)
        Nothing -> do
          -- if we don't have it we try reading it from file
          logDebug $ "Reading package from Index: " <> show name
          maybeManifests <- liftAff $ Index.readPackage Paths.registryIndexPath name
          let manifests = map (\m@(Manifest m') -> Tuple m'.version m) $ fromMaybe [] $ map NonEmptyArray.toUnfoldable maybeManifests
          let versions = Map.fromFoldable manifests
          liftEffect (Ref.write (Map.insert name versions indexMap) indexRef)
          pure (Map.lookup version versions)

  -- same deal for the metadata files
  metadataRef <- liftEffect $ Ref.new (Map.empty :: Map PackageName Metadata)
  let
    getMetadata :: PackageName -> Spago (LogEnv ()) (Either String Metadata)
    getMetadata name = do
      metadataMap <- liftEffect (Ref.read metadataRef)
      case Map.lookup name metadataMap of
        Just meta -> pure (Right meta)
        Nothing -> do
          -- if we don't have it we try reading it from file
          let metadataFilePath = Registry.API.metadataFile Paths.registryPath name
          logDebug $ "Reading metadata from file: " <> metadataFilePath
          liftAff (RegistryJson.readJsonFile metadataFilePath) >>= case _ of
            Left e -> pure (Left e)
            Right m -> do
              -- and memoize it
              liftEffect (Ref.write (Map.insert name m metadataMap) metadataRef)
              pure (Right m)

  -- clone the registry and index repo, or update them
  logInfo "Refreshing the Registry Index..."
  -- TODO: we will want to keep track how old the latest pull was - here we just wait on the fibers, but if the last
  -- pull was recent then we might just want to move on
  parallelise
    [ try (Git.fetchRepo { git: "https://github.com/purescript/registry-index.git", ref: "main" } Paths.registryIndexPath) >>= case _ of
        Right _ -> pure unit
        Left _err -> logWarn "Couldn't refresh the registry-index, will proceed anyways"
    , try (Git.fetchRepo { git: "https://github.com/purescript/registry.git", ref: "main" } Paths.registryPath) >>= case _ of
        Right _ -> pure unit
        Left _err -> logWarn "Couldn't refresh the registry, will proceed anyways"
    ]

  workspace <- Config.readWorkspace maybeSelectedPackage

  { logOptions } <- ask
  pure
    { packageNames
    , env:
        { getManifestFromIndex
        , getMetadata
        , workspace
        , logOptions
        }
    }
