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
import Node.Path as Path
import Node.Process as Process
import Registry.API as Registry.API
import Registry.Index as Index
import Registry.Json as RegistryJson
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Prelude (stripPureScriptPrefix)
import Registry.Schema (Manifest(..), Metadata)
import Registry.Version (Version)
import Registry.Version as Version
import Spago.Bin.Flags as Flags
import Spago.BuildInfo as BuildInfo
import Spago.Command.Build as Build
import Spago.Command.Bundle as Bundle
import Spago.Command.Fetch as Fetch
import Spago.Command.Init as Init
import Spago.Command.Registry as Registry
import Spago.Command.Run as Run
import Spago.Command.Sources as Sources
import Spago.Command.Test as Test
import Spago.Config (BundleConfig, BundlePlatform(..), BundleType(..), Package, RunConfig, TestConfig)
import Spago.Config as Config
import Spago.FS as FS
import Spago.Git as Git
import Spago.Log (LogVerbosity(..), supportsColor)
import Spago.Paths as Paths
import Spago.Purs as Purs

type GlobalArgs =
  { noColor :: Boolean
  , quiet :: Boolean
  , verbose :: Boolean
  }

type InitArgs =
  { setVersion :: Maybe String
  }

type FetchArgs =
  { packages :: List String
  , selectedPackage :: Maybe String
  }

type InstallArgs =
  { packages :: List String
  , selectedPackage :: Maybe String
  , pursArgs :: List String
  , backendArgs :: List String
  , output :: Maybe String
  }

type BuildArgs =
  { selectedPackage :: Maybe String
  , pursArgs :: List String
  , backendArgs :: List String
  , output :: Maybe String
  }

type RunArgs =
  { selectedPackage :: Maybe String
  , output :: Maybe String
  , pursArgs :: List String
  , backendArgs :: List String
  , execArgs :: Maybe (Array String)
  , main :: Maybe String
  }

type TestArgs =
  { selectedPackage :: Maybe String
  , output :: Maybe String
  , pursArgs :: List String
  , backendArgs :: List String
  , execArgs :: Maybe (Array String)
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
  , module :: Maybe String
  , outfile :: Maybe FilePath
  , platform :: Maybe String
  , selectedPackage :: Maybe String
  , pursArgs :: List String
  , backendArgs :: List String
  , output :: Maybe String
  , type :: Maybe String
  }

data SpagoCmd = SpagoCmd GlobalArgs Command

data Command
  = Init InitArgs
  | Fetch FetchArgs
  | Install InstallArgs
  | Build BuildArgs
  | Bundle BundleArgs
  | Run RunArgs
  | Test TestArgs
  | Sources SourcesArgs
  | RegistrySearch RegistrySearchArgs
  | RegistryInfo RegistryInfoArgs

argParser :: ArgParser SpagoCmd
argParser =
  ArgParser.choose "command"
    [ ArgParser.command [ "init" ]
        "Initialise a new project"
        do
          (SpagoCmd <$> globalArgsParser <*> (Init <$> initArgsParser) <* ArgParser.flagHelp)
    , ArgParser.command [ "fetch" ]
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
          (SpagoCmd <$> globalArgsParser <*> (Test <$> testArgsParser) <* ArgParser.flagHelp)
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

initArgsParser :: ArgParser InitArgs
initArgsParser =
  ArgParser.fromRecord
    { setVersion: Flags.maybeSetVersion
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
    , backendArgs: Flags.backendArgs
    , output: Flags.output
    }

buildArgsParser :: ArgParser BuildArgs
buildArgsParser = ArgParser.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , backendArgs: Flags.backendArgs
  , output: Flags.output
  }

runArgsParser :: ArgParser RunArgs
runArgsParser = ArgParser.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , backendArgs: Flags.backendArgs
  , execArgs: Flags.execArgs
  , output: Flags.output
  , main: Flags.moduleName
  }

testArgsParser :: ArgParser TestArgs
testArgsParser = ArgParser.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , backendArgs: Flags.backendArgs
  , execArgs: Flags.execArgs
  , output: Flags.output
  }

bundleArgsParser :: ArgParser BundleArgs
bundleArgsParser =
  ArgParser.fromRecord
    { minify: Flags.minify
    , module: Flags.entrypoint
    , type: Flags.bundleType
    , outfile: Flags.outfile
    , platform: Flags.platform
    , selectedPackage: Flags.selectedPackage
    , pursArgs: Flags.pursArgs
    , backendArgs: Flags.backendArgs
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
          Init args -> do
            purs <- Purs.getPurs
            -- Figure out the package name from the current dir
            logDebug [ show Paths.cwd, show (Path.basename Paths.cwd) ]
            packageName <- case PackageName.parse (stripPureScriptPrefix (Path.basename Paths.cwd)) of
              Left err -> die [ "Could not figure out a name for the new package. Error:", show err ]
              Right p -> pure p
            setVersion <- for args.setVersion $ Version.parseVersion Version.Lenient >>> case _ of
              Left err -> die [ "Could not parse provided set version. Error:", show err ]
              Right v -> pure v
            logDebug [ "Got packageName and setVersion:", show packageName, show setVersion ]
            let initOpts = { packageName, setVersion }
            void $ runSpago { logOptions, purs } $ Init.run initOpts
          Fetch args -> do
            { env, packageNames } <- mkFetchEnv args
            void $ runSpago env (Fetch.run packageNames)
          RegistrySearch { package } -> do
            env <- mkRegistryEnv
            void $ runSpago env (Registry.search package)
          RegistryInfo args -> do
            env <- mkRegistryEnv
            void $ runSpago env (Registry.info args)
          Install args@{ packages, selectedPackage, output, pursArgs, backendArgs } -> do
            { env, packageNames } <- mkFetchEnv { packages, selectedPackage }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run packageNames)
            let buildArgs = { selectedPackage, pursArgs, backendArgs, output }
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
          Bundle args@{ selectedPackage, pursArgs, backendArgs, output } -> do
            { env, packageNames } <- mkFetchEnv { packages: mempty, selectedPackage }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run packageNames)
            -- TODO: --no-build flag
            let buildArgs = { selectedPackage, pursArgs, backendArgs, output }
            buildEnv <- runSpago env (mkBuildEnv buildArgs dependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago buildEnv (Build.run options)
            bundleEnv <- runSpago env (mkBundleEnv args)
            runSpago bundleEnv Bundle.run
          Run args@{ selectedPackage, pursArgs, backendArgs, output } -> do
            { env, packageNames } <- mkFetchEnv { packages: mempty, selectedPackage }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run packageNames)
            -- TODO: --no-build flag
            let buildArgs = { selectedPackage, pursArgs, backendArgs, output }
            buildEnv <- runSpago env (mkBuildEnv buildArgs dependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago buildEnv (Build.run options)
            runEnv <- runSpago env (mkRunEnv args)
            runSpago runEnv Run.run
          Test args@{ selectedPackage, pursArgs, backendArgs, output } -> do
            { env, packageNames } <- mkFetchEnv { packages: mempty, selectedPackage }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run packageNames)
            -- TODO: --no-build flag
            let buildArgs = { selectedPackage, pursArgs, backendArgs, output }
            buildEnv <- runSpago env (mkBuildEnv buildArgs dependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago buildEnv (Build.run options)
            testEnv <- runSpago env (mkTestEnv args)
            runSpago testEnv Test.run

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
  let entrypoint = fromMaybe "Main" (bundleArgs.module <|> bundleConf _.module)
  let outfile = fromMaybe "index.js" (bundleArgs.outfile <|> bundleConf _.outfile)
  let
    platform = fromMaybe BundleBrowser
      ( (Config.parsePlatform =<< bundleArgs.platform)
          <|> bundleConf _.platform
      )
  let
    bundleType = fromMaybe BundleApp
      ( (Config.parseBundleType =<< bundleArgs.type)
          <|> bundleConf _.type
      )
  let bundleOptions = { minify, module: entrypoint, outfile, platform, type: bundleType }
  let newWorkspace = workspace { output = bundleArgs.output <|> workspace.output }
  let bundleEnv = { esbuild: "esbuild", logOptions, workspace: newWorkspace, selected, bundleOptions }
  pure bundleEnv

mkRunEnv :: forall a. RunArgs -> Spago (Fetch.FetchEnv a) (Run.RunEnv ())
mkRunEnv runArgs = do
  { workspace, logOptions } <- ask
  logDebug $ "Run args: " <> show runArgs

  node <- Run.getNode

  selected <- case workspace.selected of
    Just s -> pure s
    Nothing ->
      let
        workspacePackages = Config.getWorkspacePackages workspace.packageSet
      in
        -- If there's only one package, select that one
        case workspacePackages of
          [ singlePkg ] -> pure singlePkg
          _ -> do
            logDebug $ show workspacePackages
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

    moduleName = fromMaybe "Main" (runArgs.main <|> runConf _.main)
    execArgs = fromMaybe [] (runArgs.execArgs <|> runConf _.execArgs)

    runOptions =
      { moduleName
      , execArgs
      , sourceDir: Paths.cwd
      , executeDir: Paths.cwd
      , successMessage: Nothing
      , failureMessage: "Running failed."
      }
  let newWorkspace = workspace { output = runArgs.output <|> workspace.output }
  let runEnv = { logOptions, workspace: newWorkspace, selected, node, runOptions }
  pure runEnv

mkTestEnv :: forall a. TestArgs -> Spago (Fetch.FetchEnv a) (Test.TestEnv ())
mkTestEnv testArgs = do
  { workspace, logOptions } <- ask
  logDebug $ "Test args: " <> show testArgs

  node <- Run.getNode

  let
    mkSelectedTest selected =
      -- the reason why we don't have a default on the CLI is that we look some of these
      -- up in the config - though the flags take precedence
      let
        testConf :: forall x. (TestConfig -> Maybe x) -> Maybe x
        testConf f = selected.package.test >>= f

        moduleName = fromMaybe "Test.Main" (testConf (_.main >>> Just))
        execArgs = fromMaybe [] (testArgs.execArgs <|> testConf _.execArgs)
      in
        { moduleName
        , execArgs
        , selected
        }

  -- Build a NonEmptyList of selected packages - we can run more than one test suite
  selectedPackages <- case workspace.selected of
    Just s -> pure (NonEmptyArray.singleton (mkSelectedTest s))
    Nothing ->
      let
        workspacePackages = Config.getWorkspacePackages workspace.packageSet
      in
        case Array.uncons (Array.filter (_.hasTests) workspacePackages) of
          Just { head, tail } -> pure $ map mkSelectedTest $ NonEmptyArray.cons' head tail
          Nothing -> die "No package found to test."

  logDebug $ "Selected packages to test: " <> show (map _.selected.package.name selectedPackages)

  let newWorkspace = workspace { output = testArgs.output <|> workspace.output }
  let testEnv = { logOptions, workspace: newWorkspace, selectedPackages, node }
  pure testEnv

mkBuildEnv :: forall a. BuildArgs -> Map PackageName Package -> Spago (Fetch.FetchEnv a) (Build.BuildEnv ())
mkBuildEnv buildArgs dependencies = do
  { logOptions, workspace, git } <- ask
  purs <- Purs.getPurs
  let
    newWorkspace = workspace
      { output = buildArgs.output <|> workspace.output
      -- Override the backend args from the config if they are passed in through a flag
      , backend = map
          ( \b -> case List.null buildArgs.backendArgs of
              true -> b
              false -> b { args = Just (Array.fromFoldable buildArgs.backendArgs) }
          )
          workspace.backend
      }
  pure { logOptions, purs, git, dependencies, workspace: newWorkspace }

mkFetchEnv :: forall a. FetchArgs -> Spago (LogEnv a) { env :: Fetch.FetchEnv (), packageNames :: Array PackageName }
mkFetchEnv args = do
  { getManifestFromIndex, getMetadata, logOptions, git } <- mkRegistryEnv

  let { right: packageNames, left: failedPackageNames } = partitionMap PackageName.parse (Array.fromFoldable args.packages)
  unless (Array.null failedPackageNames) do
    die $ "Failed to parse some package name: " <> show failedPackageNames

  maybeSelectedPackage <- for args.selectedPackage $ PackageName.parse >>> case _ of
    Right p -> pure p
    Left _err -> die $ "Failed to parse selected package name, was: " <> show args.selectedPackage

  workspace <- runSpago { logOptions, git } do
    Config.readWorkspace maybeSelectedPackage

  pure
    { packageNames
    , env:
        { getManifestFromIndex
        , getMetadata
        , workspace
        , logOptions
        , git
        }
    }

mkRegistryEnv :: forall a. Spago (LogEnv a) (Registry.RegistryEnv ())
mkRegistryEnv = do
  logDebug $ "CWD: " <> Paths.cwd

  -- Take care of the caches
  FS.mkdirp Paths.globalCachePath
  FS.mkdirp Paths.localCachePath
  FS.mkdirp Paths.localCachePackagesPath
  logDebug $ "Global cache: " <> show Paths.globalCachePath
  logDebug $ "Local cache: " <> show Paths.localCachePath

  -- Make sure we have git
  git <- Git.getGit

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
  { logOptions } <- ask
  -- TODO: we will want to keep track how old the latest pull was - here we just wait on the fibers, but if the last
  -- pull was recent then we might just want to move on
  runSpago { logOptions, git } $ parallelise
    [ try (Git.fetchRepo { git: "https://github.com/purescript/registry-index.git", ref: "main" } Paths.registryIndexPath) >>= case _ of
        Right _ -> pure unit
        Left _err -> logWarn "Couldn't refresh the registry-index, will proceed anyways"
    , try (Git.fetchRepo { git: "https://github.com/purescript/registry.git", ref: "main" } Paths.registryPath) >>= case _ of
        Right _ -> pure unit
        Left _err -> logWarn "Couldn't refresh the registry, will proceed anyways"
    ]

  pure
    { getManifestFromIndex
    , getMetadata
    , logOptions
    , git
    }
