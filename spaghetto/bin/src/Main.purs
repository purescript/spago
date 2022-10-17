module Main (main) where

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
import Spago.Commands.Registry as Registry
import Spago.Commands.Run as Run
import Spago.Commands.Sources as Sources
import Spago.Commands.Test as Test
import Spago.Config (BundleConfig, Package, Platform(..), RunConfig)
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

type InstallArgs =
  { packages :: List String
  , selectedPackage :: Maybe String
  , pursArgs :: List String
  , output :: Maybe String
  }

type BuildArgs =
  { selectedPackage :: Maybe String
  , pursArgs :: List String
  , output :: Maybe String
  }

type RunArgs =
  { selectedPackage :: Maybe String
  , pursArgs :: List String
  , output :: Maybe String
  , execArgs :: Maybe (Array String)
  , main :: Maybe String
  }

type SourcesArgs =
  { selectedPackage :: Maybe String
  }

type RegistrySearchArgs =
  { package :: String
  }

type RegistryInfoArgs =
  { package :: String
  , maybeVersion :: Maybe String
  }

type BundleArgs =
  { minify :: Boolean
  , entrypoint :: Maybe FilePath
  , outfile :: Maybe FilePath
  , platform :: Maybe String
  , selectedPackage :: Maybe String
  , pursArgs :: List String
  , output :: Maybe String
  }

data SpagoCmd = SpagoCmd GlobalArgs Command

data Command
  = Fetch FetchArgs
  | Install InstallArgs
  | Build BuildArgs
  | Bundle BundleArgs
  | Run RunArgs
  | Test RunArgs
  | Sources SourcesArgs
  | RegistrySearch RegistrySearchArgs
  | RegistryInfo RegistryInfoArgs

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
    , ArgParser.command [ "run" ]
        "Run the project"
        do
          (SpagoCmd <$> globalArgsParser <*> (Run <$> runArgsParser) <* ArgParser.flagHelp)
    , ArgParser.command [ "test" ]
        "Test the project"
        do
          (SpagoCmd <$> globalArgsParser <*> (Test <$> runArgsParser) <* ArgParser.flagHelp)
    , ArgParser.command [ "bundle" ]
        "Bundle the project in a single file"
        do
          (SpagoCmd <$> globalArgsParser <*> (Bundle <$> bundleArgsParser) <* ArgParser.flagHelp)
    , ArgParser.command [ "sources" ]
        "List all the source paths (globs) for the dependencies of the project"
        do
          (SpagoCmd <$> globalArgsParser <*> (Sources <$> sourcesArgsParser) <* ArgParser.flagHelp)
    , ArgParser.command [ "registry" ]
        "Commands to interact with the Registry"
        do
          ArgParser.choose "registry-subcommand"
            [ ArgParser.command [ "search" ]
                "Search for package names in the Registry"
                (SpagoCmd <$> globalArgsParser <*> (RegistrySearch <$> registrySearchArgsParser) <* ArgParser.flagHelp)
            , ArgParser.command [ "info" ]
                "Query the Registry for information about packages and versions"
                (SpagoCmd <$> globalArgsParser <*> (RegistryInfo <$> registryInfoArgsParser) <* ArgParser.flagHelp)
            ] <* ArgParser.flagHelp
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
    , output: Flags.output
    }

buildArgsParser :: ArgParser BuildArgs
buildArgsParser = ArgParser.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , output: Flags.output
  }

runArgsParser :: ArgParser RunArgs
runArgsParser = ArgParser.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , output: Flags.output
  , execArgs: Flags.backendArgs
  , main: Flags.moduleName
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
    , output: Flags.output
    }

registrySearchArgsParser :: ArgParser RegistrySearchArgs
registrySearchArgsParser =
  ArgParser.fromRecord
    { package: Flags.package
    }

registryInfoArgsParser :: ArgParser RegistryInfoArgs
registryInfoArgsParser = ado
  package <- Flags.package
  maybeVersion <- Flags.maybeVersion
  in { package, maybeVersion }

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
    Left err -> Console.error $ ArgParser.printArgError err
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
          RegistrySearch { package } -> do
            env <- mkRegistryEnv
            void $ runSpago env (Registry.search package)
          RegistryInfo args -> do
            env <- mkRegistryEnv
            void $ runSpago env (Registry.info args)
          Install args@{ packages, selectedPackage, output, pursArgs } -> do
            { env, packageNames } <- mkFetchEnv { packages, selectedPackage }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run packageNames)
            let buildArgs = { selectedPackage, pursArgs, output }
            env' <- runSpago env (mkBuildEnv buildArgs dependencies)
            let options = { depsOnly: true, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago env' (Build.run options)
          Build args -> do
            { env, packageNames } <- mkFetchEnv { packages: mempty, selectedPackage: args.selectedPackage }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run packageNames)
            buildEnv <- runSpago env (mkBuildEnv args dependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago buildEnv (Build.run options)
          Bundle args@{ selectedPackage, pursArgs, output } -> do
            { env, packageNames } <- mkFetchEnv { packages: mempty, selectedPackage }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run packageNames)
            -- TODO: --no-build flag
            let buildArgs = { selectedPackage, pursArgs, output }
            buildEnv <- runSpago env (mkBuildEnv buildArgs dependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago buildEnv (Build.run options)
            bundleEnv <- runSpago env (mkBundleEnv args)
            runSpago bundleEnv Bundle.run
          Run args@{ selectedPackage, pursArgs, output } -> do
            { env, packageNames } <- mkFetchEnv { packages: mempty, selectedPackage }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run packageNames)
            -- TODO: --no-build flag
            let buildArgs = { selectedPackage, pursArgs, output }
            buildEnv <- runSpago env (mkBuildEnv buildArgs dependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago buildEnv (Build.run options)
            runEnv <- runSpago env (mkRunEnv args { isTest: false })
            runSpago runEnv Run.run
          Test args@{ selectedPackage, pursArgs, output } -> do
            { env, packageNames } <- mkFetchEnv { packages: mempty, selectedPackage }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run packageNames)
            -- TODO: --no-build flag
            let buildArgs = { selectedPackage, pursArgs, output }
            buildEnv <- runSpago env (mkBuildEnv buildArgs dependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago buildEnv (Build.run options)
            runEnv <- runSpago env (mkRunEnv args { isTest: true })
            runSpago runEnv Test.run

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

mkBundleEnv :: forall a. BundleArgs -> Spago (Fetch.FetchEnv a) (Bundle.BundleEnv ())
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
  let minify = Array.any (_ == true) [ bundleArgs.minify, fromMaybe false (_.minify =<< selected.package.bundle) ]
  let entrypoint = fromMaybe "main.js" (bundleArgs.entrypoint <|> bundleConf _.entrypoint)
  let outfile = fromMaybe "index.js" (bundleArgs.outfile <|> bundleConf _.outfile)
  let
    platform = fromMaybe PlatformNode
      ( (Config.parsePlatform =<< bundleArgs.platform)
          <|> bundleConf _.platform
      )
  let bundleOptions = { minify, entrypoint, outfile, platform }
  let newWorkspace = workspace { output = bundleArgs.output <|> workspace.output }
  let bundleEnv = { esbuild: "esbuild", logOptions, workspace: newWorkspace, selected, bundleOptions } -- TODO: which esbuild
  pure bundleEnv

mkRunEnv :: forall a. RunArgs -> { isTest :: Boolean } -> Spago (Fetch.FetchEnv a) (Run.RunEnv ())
mkRunEnv runArgs { isTest } = do
  { workspace, logOptions } <- ask
  logDebug $ "Run args: " <> show runArgs

  selected <- case workspace.selected of
    Just s -> pure s
    Nothing ->
      let
        workspacePackages = Config.getWorkspacePackages workspace.packageSet
      in
        -- If there's only one test package, select that one
        case isTest, Array.filter (_.hasTests) workspacePackages of
          true, [ singlePkg ] -> pure singlePkg
          _, _ -> do
            logDebug $ show $ Array.filter (_.hasTests) workspacePackages
            die
              [ toDoc "No package was selected for running. Please select (with -p) one of the following packages:"
              , indent (toDoc $ map _.package.name workspacePackages)
              ]

  logDebug $ "Selected package to run: " <> show selected.package.name

  -- the reason why we don't have a default on the CLI is that we look some of these
  -- up in the config - though the flags take precedence
  let
    runConf :: forall x. (RunConfig -> Maybe x) -> Maybe x
    runConf f = selected.package.run >>= f

    moduleName = fromMaybe (if isTest then "Test.Main" else "Main") (runArgs.main <|> runConf _.main)
    execArgs = fromMaybe [] (runArgs.execArgs <|> runConf _.execArgs)

    runOptions =
      { moduleName
      , execArgs
      , sourceDir: Paths.cwd
      , executeDir: selected.path
      , successMessage: Nothing
      , failureMessage: "Running failed."
      }
  let newWorkspace = workspace { output = runArgs.output <|> workspace.output }
  let runEnv = { logOptions, workspace: newWorkspace, selected, runOptions }
  pure runEnv

mkBuildEnv :: forall a. BuildArgs -> Map PackageName Package -> Spago (Fetch.FetchEnv a) (Build.BuildEnv ())
mkBuildEnv buildArgs dependencies = do
  { logOptions, workspace } <- ask
  -- FIXME: find executables in path, parse compiler version, etc etc
  let newWorkspace = workspace { output = buildArgs.output <|> workspace.output }
  pure { logOptions, purs: "purs", git: "git", dependencies, workspace: newWorkspace }

mkFetchEnv :: forall a. FetchArgs -> Spago (LogEnv a) { env :: Fetch.FetchEnv (), packageNames :: Array PackageName }
mkFetchEnv args = do
  { getManifestFromIndex, getMetadata, logOptions } <- mkRegistryEnv

  let { right: packageNames, left: failedPackageNames } = partitionMap PackageName.parse (Array.fromFoldable args.packages)
  unless (Array.null failedPackageNames) do
    die $ "Failed to parse some package name: " <> show failedPackageNames

  -- TODO: refactor this
  maybeSelectedPackage <- case map PackageName.parse args.selectedPackage of
    Nothing -> pure Nothing
    Just (Left _err) -> die $ "Failed to parse selected package name, was: " <> show args.selectedPackage
    Just (Right p) -> pure (Just p)

  workspace <- Config.readWorkspace maybeSelectedPackage

  pure
    { packageNames
    , env:
        { getManifestFromIndex
        , getMetadata
        , workspace
        , logOptions
        }
    }

mkRegistryEnv :: forall a. Spago (LogEnv a) (Registry.RegistryEnv ())
mkRegistryEnv = do
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

  { logOptions } <- ask
  pure
    { getManifestFromIndex
    , getMetadata
    , logOptions
    }
