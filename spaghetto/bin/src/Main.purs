module Main (main) where

import Spago.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Control.Alt (alt)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.Argonaut.Common as CA.Common
import Data.JSDate as JSDate
import Data.List as List
import Data.Map as Map
import Data.Set.NonEmpty (NonEmptySet)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.FS.Stats (Stats(..))
import Node.Path as Path
import Node.Process as Process
import Record as Record
import Registry.Constants as Registry.Constants
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Spago.Bin.Flags as Flags
import Spago.BuildInfo as BuildInfo
import Spago.Command.Build as Build
import Spago.Command.Bundle as Bundle
import Spago.Command.Fetch as Fetch
import Spago.Command.Init as Init
import Spago.Command.Ls (LsDepsArgs, LsPackagesArgs)
import Spago.Command.Ls as Ls
import Spago.Command.Publish as Publish
import Spago.Command.Registry as Registry
import Spago.Command.Run as Run
import Spago.Command.Sources as Sources
import Spago.Command.Test as Test
import Spago.Config (BundleConfig, BundlePlatform(..), BundleType(..), Package, RunConfig, TestConfig)
import Spago.Config as Config
import Spago.Core.Config as Core
import Spago.FS as FS
import Spago.Git as Git
import Spago.Json as Json
import Spago.Log (LogVerbosity(..))
import Spago.Paths as Paths
import Spago.Purs (Purs)
import Spago.Purs as Purs
import Unsafe.Coerce (unsafeCoerce)

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
  , ensureRanges :: Boolean
  }

type InstallArgs =
  { packages :: List String
  , selectedPackage :: Maybe String
  , pursArgs :: List String
  , backendArgs :: List String
  , output :: Maybe String
  , pedanticPackages :: Boolean
  , ensureRanges :: Boolean
  , jsonErrors :: Boolean
  , psaArgs :: PsaArgs
  }

type PsaArgs =
  { strict :: Maybe Boolean
  , censorWarnings :: Maybe Boolean
  , censorLib :: Maybe Boolean
  , censorSrc :: Maybe Boolean
  , showSource :: Maybe Core.ShowSourceCode
  , censorCodes :: Maybe (NonEmptySet String)
  , filterCodes :: Maybe (NonEmptySet String)
  , statVerbosity :: Maybe Core.StatVerbosity
  , stashFile :: Maybe (Either Boolean String)
  }

type BuildArgs a =
  { selectedPackage :: Maybe String
  , pursArgs :: List String
  , backendArgs :: List String
  , output :: Maybe String
  , pedanticPackages :: Boolean
  , ensureRanges :: Boolean
  , jsonErrors :: Boolean
  , psaArgs :: PsaArgs
  | a
  }

-- TODO: more repl arguments: dependencies, repl-package
type ReplArgs =
  { selectedPackage :: Maybe String
  , pursArgs :: List String
  , backendArgs :: List String
  }

type RunArgs =
  { selectedPackage :: Maybe String
  , output :: Maybe String
  , pedanticPackages :: Boolean
  , pursArgs :: List String
  , backendArgs :: List String
  , execArgs :: Maybe (Array String)
  , main :: Maybe String
  , ensureRanges :: Boolean
  , jsonErrors :: Boolean
  , psaArgs :: PsaArgs
  }

type TestArgs =
  { selectedPackage :: Maybe String
  , output :: Maybe String
  , pedanticPackages :: Boolean
  , pursArgs :: List String
  , backendArgs :: List String
  , execArgs :: Maybe (Array String)
  , jsonErrors :: Boolean
  , psaArgs :: PsaArgs
  }

type SourcesArgs =
  { selectedPackage :: Maybe String
  , json :: Boolean
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
  , pedanticPackages :: Boolean
  , type :: Maybe String
  , ensureRanges :: Boolean
  , jsonErrors :: Boolean
  , psaArgs :: PsaArgs
  }

type PublishArgs =
  { selectedPackage :: Maybe String
  , psaArgs :: PsaArgs
  }

data SpagoCmd a = SpagoCmd GlobalArgs (Command a)

data Command a
  = Init InitArgs
  | Fetch FetchArgs
  | Install InstallArgs
  | Build (BuildArgs a)
  | Bundle BundleArgs
  | Repl ReplArgs
  | Run RunArgs
  | Test TestArgs
  | Sources SourcesArgs
  | RegistrySearch RegistrySearchArgs
  | RegistryInfo RegistryInfoArgs
  | LsDeps LsDepsArgs
  | LsPackages LsPackagesArgs
  | Publish PublishArgs

argParser :: ArgParser (SpagoCmd ())
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
    , ArgParser.command [ "repl" ]
        "Start a REPL"
        do
          (SpagoCmd <$> globalArgsParser <*> (Repl <$> replArgsParser) <* ArgParser.flagHelp)
    , ArgParser.command [ "publish" ]
        "Publish a package"
        do
          (SpagoCmd <$> globalArgsParser <*> (Publish <$> publishArgsParser) <* ArgParser.flagHelp)
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
    , ArgParser.command [ "ls" ] "List packages or dependencies" do
        ArgParser.choose "ls-subcommand"
          [ ArgParser.command [ "packages" ]
              "List packages available in the local package set"
              (SpagoCmd <$> globalArgsParser <*> (LsPackages <$> lsPackagesArgsParser) <* ArgParser.flagHelp)
          , ArgParser.command [ "deps" ]
              "List dependencies of the project"
              (SpagoCmd <$> globalArgsParser <*> (LsDeps <$> lsDepsArgsParser) <* ArgParser.flagHelp)
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
    , ensureRanges: Flags.ensureRanges
    }

sourcesArgsParser :: ArgParser SourcesArgs
sourcesArgsParser =
  ArgParser.fromRecord
    { selectedPackage: Flags.selectedPackage
    , json: Flags.json
    }

installArgsParser :: ArgParser InstallArgs
installArgsParser =
  ArgParser.fromRecord
    { packages: Flags.packages
    , selectedPackage: Flags.selectedPackage
    , pursArgs: Flags.pursArgs
    , backendArgs: Flags.backendArgs
    , output: Flags.output
    , pedanticPackages: Flags.pedanticPackages
    , ensureRanges: Flags.ensureRanges
    , jsonErrors: Flags.jsonErrors
    , psaArgs: psaArgsParser
    }

psaArgsParser :: ArgParser PsaArgs
psaArgsParser = ArgParser.fromRecord
  { strict: Flags.psaStrict
  , censorWarnings: Flags.psaCensorWarnings
  , censorLib: Flags.psaCensorLib
  , censorSrc: Flags.psaCensorSrc
  , showSource: Flags.psaShowSource
  , censorCodes: Flags.psaCensorCodes
  , filterCodes: Flags.psaFilterCodes
  , statVerbosity: Flags.psaStatVerbosity
  , stashFile: Flags.psaStashFile
  }

buildArgsParser :: ArgParser (BuildArgs ())
buildArgsParser = ArgParser.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , backendArgs: Flags.backendArgs
  , output: Flags.output
  , pedanticPackages: Flags.pedanticPackages
  , ensureRanges: Flags.ensureRanges
  , jsonErrors: Flags.jsonErrors
  , psaArgs: psaArgsParser
  }

replArgsParser :: ArgParser ReplArgs
replArgsParser = ArgParser.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , backendArgs: Flags.backendArgs
  }

runArgsParser :: ArgParser RunArgs
runArgsParser = ArgParser.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , backendArgs: Flags.backendArgs
  , execArgs: Flags.execArgs
  , output: Flags.output
  , pedanticPackages: Flags.pedanticPackages
  , main: Flags.moduleName
  , ensureRanges: Flags.ensureRanges
  , jsonErrors: Flags.jsonErrors
  , psaArgs: psaArgsParser
  }

testArgsParser :: ArgParser TestArgs
testArgsParser = ArgParser.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , backendArgs: Flags.backendArgs
  , execArgs: Flags.execArgs
  , output: Flags.output
  , pedanticPackages: Flags.pedanticPackages
  , jsonErrors: Flags.jsonErrors
  , psaArgs: psaArgsParser
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
    , pedanticPackages: Flags.pedanticPackages
    , ensureRanges: Flags.ensureRanges
    , jsonErrors: Flags.jsonErrors
    , psaArgs: psaArgsParser
    }

publishArgsParser :: ArgParser PublishArgs
publishArgsParser =
  ArgParser.fromRecord
    { selectedPackage: Flags.selectedPackage
    , psaArgs: psaArgsParser
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

lsPackagesArgsParser :: ArgParser LsPackagesArgs
lsPackagesArgsParser = ArgParser.fromRecord
  { json: Flags.json
  }

lsDepsArgsParser :: ArgParser LsDepsArgs
lsDepsArgsParser = ArgParser.fromRecord
  { json: Flags.json
  , transitive: Flags.transitive
  , selectedPackage: Flags.selectedPackage
  }

parseArgs :: Effect (Either ArgParser.ArgError (SpagoCmd ()))
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
            { env } <- mkFetchEnv { packages: mempty, selectedPackage: args.selectedPackage, ensureRanges: false }
            void $ runSpago env (Sources.run { json: args.json })
          Init args -> do
            purs <- Purs.getPurs
            -- Figure out the package name from the current dir
            logDebug [ show Paths.cwd, show (Path.basename Paths.cwd) ]
            packageName <- case PackageName.parse (PackageName.stripPureScriptPrefix (Path.basename Paths.cwd)) of
              Left err -> die [ "Could not figure out a name for the new package. Error:", show err ]
              Right p -> pure p
            setVersion <- for args.setVersion $ parseLenientVersion >>> case _ of
              Left err -> die [ "Could not parse provided set version. Error:", show err ]
              Right v -> pure v
            logDebug [ "Got packageName and setVersion:", PackageName.print packageName, unsafeStringify setVersion ]
            let initOpts = { packageName, setVersion }
            -- Fetch the registry here so we can select the right package set later
            void mkRegistryEnv
            void $ runSpago { logOptions, purs } $ Init.run initOpts
          Fetch args -> do
            { env, fetchOpts } <- mkFetchEnv args
            void $ runSpago env (Fetch.run fetchOpts)
          RegistrySearch { package } -> do
            env <- mkRegistryEnv
            void $ runSpago env (Registry.search package)
          RegistryInfo args -> do
            env <- mkRegistryEnv
            void $ runSpago env (Registry.info args)
          Install args@{ packages, selectedPackage, ensureRanges } -> do
            { env, fetchOpts } <- mkFetchEnv { packages, selectedPackage, ensureRanges }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run fetchOpts)
            env' <- runSpago env (mkBuildEnv args dependencies)
            let options = { depsOnly: true, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago env' (Build.run options)
          Build args@{ selectedPackage, ensureRanges } -> do
            { env, fetchOpts } <- mkFetchEnv { packages: mempty, selectedPackage, ensureRanges }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run fetchOpts)
            buildEnv <- runSpago env (mkBuildEnv args dependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago buildEnv (Build.run options)
          Publish { selectedPackage, psaArgs } -> do
            { env, fetchOpts } <- mkFetchEnv { packages: mempty, selectedPackage, ensureRanges: false }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run fetchOpts)
            let
              buildArgs =
                { selectedPackage
                , psaArgs
                , pursArgs: mempty
                , backendArgs: mempty
                , output: mempty
                , pedanticPackages: false
                , ensureRanges: false
                , jsonErrors: false
                }
            { purs, psaConfig } <- runSpago env (mkBuildEnv buildArgs dependencies)
            publishEnv <- runSpago env (mkPublishEnv dependencies purs psaConfig)
            void $ runSpago publishEnv (Publish.publish {})
          Repl args -> do
            -- TODO implement
            pure unit
          Bundle args@{ selectedPackage, ensureRanges } -> do
            { env, fetchOpts } <- mkFetchEnv { packages: mempty, selectedPackage, ensureRanges }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run fetchOpts)
            -- TODO: --no-build flag
            buildEnv <- runSpago env (mkBuildEnv args dependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago buildEnv (Build.run options)
            bundleEnv <- runSpago env (mkBundleEnv args)
            runSpago bundleEnv Bundle.run
          Run args@{ selectedPackage, ensureRanges } -> do
            { env, fetchOpts } <- mkFetchEnv { packages: mempty, selectedPackage, ensureRanges }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run fetchOpts)
            -- TODO: --no-build flag
            buildEnv <- runSpago env (mkBuildEnv args dependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago buildEnv (Build.run options)
            runEnv <- runSpago env (mkRunEnv args)
            runSpago runEnv Run.run
          Test args@{ selectedPackage } -> do
            { env, fetchOpts } <- mkFetchEnv { packages: mempty, selectedPackage, ensureRanges: false }
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run fetchOpts)
            -- TODO: --no-build flag
            buildEnv <- runSpago env (mkBuildEnv (Record.union args { ensureRanges: false }) dependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs }
            runSpago buildEnv (Build.run options)
            testEnv <- runSpago env (mkTestEnv args)
            runSpago testEnv Test.run
          LsPackages args -> do
            let fetchArgs = { packages: mempty, selectedPackage: Nothing, ensureRanges: false }
            { env: env@{ workspace }, fetchOpts } <- mkFetchEnv fetchArgs
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run fetchOpts)
            let lsEnv = { workspace, dependencies, logOptions }
            runSpago lsEnv (Ls.listPackageSet args)
          LsDeps { selectedPackage, json, transitive } -> do
            let fetchArgs = { packages: mempty, selectedPackage, ensureRanges: false }
            { env, fetchOpts } <- mkFetchEnv fetchArgs
            -- TODO: --no-fetch flag
            dependencies <- runSpago env (Fetch.run fetchOpts)
            lsEnv <- runSpago env (mkLsEnv dependencies)
            runSpago lsEnv (Ls.listPackages { json, transitive })

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

  logDebug $ "Selected package to bundle: " <> PackageName.print selected.package.name

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
  let
    newWorkspace = workspace
      { buildOptions
          { output = bundleArgs.output <|> workspace.buildOptions.output
          , pedanticPackages = bundleArgs.pedanticPackages || workspace.buildOptions.pedanticPackages
          }
      }
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
            logDebug $ unsafeStringify workspacePackages
            die
              [ toDoc "No package was selected for running. Please select (with -p) one of the following packages:"
              , indent (toDoc $ map _.package.name workspacePackages)
              ]

  logDebug $ "Selected package to run: " <> PackageName.print selected.package.name

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
  let newWorkspace = workspace { buildOptions { output = runArgs.output <|> workspace.buildOptions.output } }
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

  logDebug $ "Selected packages to test: " <> Json.stringifyJson (CA.Common.nonEmptyArray PackageName.codec) (map _.selected.package.name selectedPackages)

  let newWorkspace = workspace { buildOptions { output = testArgs.output <|> workspace.buildOptions.output } }
  let testEnv = { logOptions, workspace: newWorkspace, selectedPackages, node }
  pure testEnv

mkBuildEnv :: forall a b. BuildArgs b -> Map PackageName Package -> Spago (Fetch.FetchEnv a) (Build.BuildEnv ())
mkBuildEnv buildArgs dependencies = do
  { logOptions, workspace, git } <- ask
  purs <- Purs.getPurs
  let
    newWorkspace = workspace
      { buildOptions
          { output = buildArgs.output <|> workspace.buildOptions.output
          , pedanticPackages = buildArgs.pedanticPackages || workspace.buildOptions.pedanticPackages
          }
      -- Override the backend args from the config if they are passed in through a flag
      , backend = map
          ( \b -> case List.null buildArgs.backendArgs of
              true -> b
              false -> b { args = Just (Array.fromFoldable buildArgs.backendArgs) }
          )
          workspace.backend
      }

    psaConfig :: Core.PsaConfig
    psaConfig =
      { strict: alt buildArgs.psaArgs.strict $ config >>= _.strict
      , censorWarnings: alt buildArgs.psaArgs.censorWarnings $ config >>= _.censorWarnings
      , censorLib: alt buildArgs.psaArgs.censorWarnings $ config >>= _.censorLib
      , censorSrc: alt buildArgs.psaArgs.censorSrc $ config >>= _.censorSrc
      , showSource: alt buildArgs.psaArgs.showSource $ config >>= _.showSource
      , censorCodes: alt buildArgs.psaArgs.censorCodes $ config >>= _.censorCodes
      , filterCodes: alt buildArgs.psaArgs.filterCodes $ config >>= _.filterCodes
      , statVerbosity: alt buildArgs.psaArgs.statVerbosity $ config >>= _.statVerbosity
      , stashFile: alt buildArgs.psaArgs.stashFile $ config >>= _.stashFile
      }
      where
      config :: Maybe Core.PsaConfig
      config = case workspace.selected of
        Just p -> p.package.build >>= _.psaOptions
        Nothing -> workspace.buildOptions.psaOptions

  pure { logOptions, purs, git, dependencies, workspace: newWorkspace, psaConfig }

mkPublishEnv :: forall a. Map PackageName Package -> Purs -> Config.PsaConfig -> Spago (Fetch.FetchEnv a) (Publish.PublishEnv a)
mkPublishEnv dependencies purs psaConfig = do
  env <- ask
  selected <- case env.workspace.selected of
    Just s -> pure s
    Nothing ->
      let
        workspacePackages = Config.getWorkspacePackages env.workspace.packageSet
      in
        -- If there's only one package, select that one
        case workspacePackages of
          [ singlePkg ] -> pure singlePkg
          _ -> do
            logDebug $ unsafeStringify workspacePackages
            die
              [ toDoc "No package was selected for publishing. Please select (with -p) one of the following packages:"
              , indent (toDoc $ map _.package.name workspacePackages)
              ]
  pure (Record.union { purs, selected, dependencies, psaConfig } env)

mkFetchEnv :: forall a. FetchArgs -> Spago (LogEnv a) { env :: Fetch.FetchEnv (), fetchOpts :: Fetch.FetchOpts }
mkFetchEnv args = do
  let { right: packageNames, left: failedPackageNames } = partitionMap PackageName.parse (Array.fromFoldable args.packages)
  unless (Array.null failedPackageNames) do
    die $ "Failed to parse some package name: " <> show failedPackageNames

  maybeSelectedPackage <- for args.selectedPackage $ PackageName.parse >>> case _ of
    Right p -> pure p
    Left _err -> die $ "Failed to parse selected package name, was: " <> show args.selectedPackage

  env <- mkRegistryEnv
  workspace <- runSpago env (Config.readWorkspace maybeSelectedPackage)
  let fetchOpts = { packages: packageNames, ensureRanges: args.ensureRanges }
  pure { fetchOpts, env: Record.union { workspace } env }

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
    getCachedIndex :: Effect ManifestIndex
    getCachedIndex = map unsafeCoerce $ Ref.read indexRef

    getManifestFromIndex :: PackageName -> Version -> Spago (LogEnv ()) (Maybe Manifest)
    getManifestFromIndex name version = do
      indexMap <- liftEffect (Ref.read indexRef)
      case Map.lookup name indexMap of
        Just meta -> pure (Map.lookup version meta)
        Nothing -> do
          -- if we don't have it we try reading it from file
          logDebug $ "Reading package from Index: " <> PackageName.print name
          maybeManifests <- liftAff $ ManifestIndex.readEntryFile Paths.registryIndexPath name
          manifests <- map (map (\m@(Manifest m') -> Tuple m'.version m)) case maybeManifests of
            Right ms -> pure $ NonEmptyArray.toUnfoldable ms
            Left err -> do
              logWarn $ "Could not read package manifests from index, proceeding anyways. Error: " <> err
              pure []
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
          let metadataFilePath = Path.concat [ Paths.registryPath, Registry.Constants.metadataDirectory, PackageName.print name <> ".json" ]
          logDebug $ "Reading metadata from file: " <> metadataFilePath
          liftAff (FS.readJsonFile Metadata.codec metadataFilePath) >>= case _ of
            Left e -> pure (Left e)
            Right m -> do
              -- and memoize it
              liftEffect (Ref.write (Map.insert name m metadataMap) metadataRef)
              pure (Right m)

  { logOptions } <- ask
  -- we keep track of how old the latest pull was - if the last pull was recent enough
  -- we just move on, otherwise run the fibers
  whenM shouldFetchRegistryRepos do
    -- clone the registry and index repo, or update them
    logInfo "Refreshing the Registry Index..."
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
    , getCachedIndex
    , getMetadata
    , logOptions
    , git
    }

mkLsEnv :: forall a. Map PackageName Package -> Spago (Fetch.FetchEnv a) Ls.LsEnv
mkLsEnv dependencies = do
  { logOptions, workspace } <- ask
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
            logDebug $ unsafeStringify workspacePackages
            die
              [ toDoc "No package was selected. Please select (with -p) one of the following packages:"
              , indent (toDoc $ map _.package.name workspacePackages)
              ]
  pure { logOptions, workspace, dependencies, selected }

shouldFetchRegistryRepos :: forall a. Spago (LogEnv a) Boolean
shouldFetchRegistryRepos = do
  let freshRegistryCanary = Path.concat [ Paths.globalCachePath, "fresh-registry-canary.txt" ]
  FS.stat freshRegistryCanary >>= case _ of
    Left err -> do
      -- If the stat fails the file probably does not exist
      logDebug [ "Could not stat " <> freshRegistryCanary, show err ]
      -- in which case we touch it and fetch
      touch freshRegistryCanary
      pure true
    Right (Stats { mtime }) -> do
      -- it does exist here, see if it's old enough, and fetch if it is
      now <- liftEffect $ JSDate.now
      let minutes = 15.0
      let staleAfter = 1000.0 * 60.0 * minutes -- need this in millis
      let isOldEnough = (JSDate.getTime now) > (JSDate.getTime mtime + staleAfter)
      if isOldEnough then do
        logDebug "Registry is old enough, refreshing canary"
        touch freshRegistryCanary
        pure true
      else do
        logDebug "Registry index is fresh enough, moving on..."
        pure false
  where
  touch path = do
    FS.ensureFileSync path
    FS.writeTextFile path ""

foreign import supportsColor :: Effect Boolean
