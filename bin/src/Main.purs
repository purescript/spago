module Main (main) where

import Spago.Prelude

import Control.Monad.Reader as Reader
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Codec.Argonaut.Common as CA.Common
import Data.Foldable as Foldable
import Data.JSDate as JSDate
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Set.NonEmpty (NonEmptySet)
import Data.String as String
import Effect.Aff as Aff
import Effect.Ref as Ref
import Node.FS.Stats (Stats(..))
import Node.Path as Path
import Node.Process as Process
import Options.Applicative (CommandFields, Mod, Parser, ParserPrefs(..))
import Options.Applicative as O
import Optparse as Optparse
import Record as Record
import Registry.Constants as Registry.Constants
import Registry.ManifestIndex as ManifestIndex
import Registry.Metadata as Metadata
import Registry.PackageName as PackageName
import Spago.Bin.Flags as Flags
import Spago.Command.Build as Build
import Spago.Command.Bundle as Bundle
import Spago.Command.Docs as Docs
import Spago.Command.Fetch as Fetch
import Spago.Command.Init as Init
import Spago.Command.Ls (LsDepsArgs, LsPackagesArgs)
import Spago.Command.Ls as Ls
import Spago.Command.Publish as Publish
import Spago.Command.Registry (RegistryInfoArgs, RegistrySearchArgs, RegistryPackageSetsArgs)
import Spago.Command.Registry as RegistryCmd
import Spago.Command.Repl as Repl
import Spago.Command.Run as Run
import Spago.Command.Sources as Sources
import Spago.Command.Test as Test
import Spago.Config (BundleConfig, BundlePlatform(..), BundleType(..), RunConfig, TestConfig, PackageMap)
import Spago.Config as Config
import Spago.Core.Config as Core
import Spago.Db as Db
import Spago.Esbuild as Esbuild
import Spago.FS as FS
import Spago.Generated.BuildInfo as BuildInfo
import Spago.Git as Git
import Spago.Json as Json
import Spago.Log (LogVerbosity(..))
import Spago.Paths as Paths
import Spago.Purs as Purs
import Spago.Registry as Registry
import Unsafe.Coerce as UnsafeCoerce

type GlobalArgs =
  { noColor :: Boolean
  , quiet :: Boolean
  , verbose :: Boolean
  }

type InitArgs =
  { setVersion :: Maybe String
  , name :: Maybe String
  , useSolver :: Boolean
  }

type FetchArgs =
  { packages :: List String
  , selectedPackage :: Maybe String
  , ensureRanges :: Boolean
  , testDeps :: Boolean
  }

type InstallArgs =
  { packages :: List String
  , selectedPackage :: Maybe String
  , pursArgs :: List String
  , backendArgs :: List String
  , output :: Maybe String
  , pedanticPackages :: Boolean
  , ensureRanges :: Boolean
  , testDeps :: Boolean
  }

type BuildArgs a =
  { selectedPackage :: Maybe String
  , pursArgs :: List String
  , backendArgs :: List String
  , output :: Maybe String
  , pedanticPackages :: Boolean
  , ensureRanges :: Boolean
  , jsonErrors :: Boolean
  , strict :: Maybe Boolean
  , censorBuildWarnings :: Maybe Core.CensorBuildWarnings
  , showSource :: Maybe Core.ShowSourceCode
  , censorCodes :: Maybe (NonEmptySet String)
  , filterCodes :: Maybe (NonEmptySet String)
  , statVerbosity :: Maybe Core.StatVerbosity
  , persistWarnings :: Maybe Boolean
  | a
  }

type DocsArgs =
  { docsFormat :: Purs.DocsFormat
  , depsOnly :: Boolean
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
  , strict :: Maybe Boolean
  , censorBuildWarnings :: Maybe Core.CensorBuildWarnings
  , showSource :: Maybe Core.ShowSourceCode
  , censorCodes :: Maybe (NonEmptySet String)
  , filterCodes :: Maybe (NonEmptySet String)
  , statVerbosity :: Maybe Core.StatVerbosity
  , persistWarnings :: Maybe Boolean
  }

type TestArgs =
  { selectedPackage :: Maybe String
  , output :: Maybe String
  , pedanticPackages :: Boolean
  , pursArgs :: List String
  , backendArgs :: List String
  , execArgs :: Maybe (Array String)
  , strict :: Maybe Boolean
  , censorBuildWarnings :: Maybe Core.CensorBuildWarnings
  , showSource :: Maybe Core.ShowSourceCode
  , censorCodes :: Maybe (NonEmptySet String)
  , filterCodes :: Maybe (NonEmptySet String)
  , statVerbosity :: Maybe Core.StatVerbosity
  , persistWarnings :: Maybe Boolean
  }

type SourcesArgs =
  { selectedPackage :: Maybe String
  , json :: Boolean
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
  , strict :: Maybe Boolean
  , censorBuildWarnings :: Maybe Core.CensorBuildWarnings
  , showSource :: Maybe Core.ShowSourceCode
  , censorCodes :: Maybe (NonEmptySet String)
  , filterCodes :: Maybe (NonEmptySet String)
  , statVerbosity :: Maybe Core.StatVerbosity
  , persistWarnings :: Maybe Boolean
  }

type PublishArgs =
  { selectedPackage :: Maybe String
  }

data SpagoCmd a = SpagoCmd GlobalArgs (Command a)

data Command a
  = Init InitArgs
  | Fetch FetchArgs
  | Install InstallArgs
  | Build (BuildArgs a)
  | Docs DocsArgs
  | Bundle BundleArgs
  | Repl ReplArgs
  | Run RunArgs
  | Test TestArgs
  | Sources SourcesArgs
  | RegistrySearch RegistrySearchArgs
  | RegistryInfo RegistryInfoArgs
  | RegistryPackageSets RegistryPackageSetsArgs
  | LsDeps LsDepsArgs
  | LsPackages LsPackagesArgs
  | Publish PublishArgs

commandParser :: forall (a :: Row Type). String -> Parser (Command a) -> String -> Mod CommandFields (SpagoCmd a)
commandParser command_ parser_ description_ =
  O.command command_
    ( O.info
        (SpagoCmd <$> globalArgsParser <*> parser_)
        (O.progDesc description_)
    )

argParser :: Parser (SpagoCmd ())
argParser =
  O.hsubparser $ Foldable.fold
    [ commandParser "init" (Init <$> initArgsParser) "Initialise a new project"
    , commandParser "fetch" (Fetch <$> fetchArgsParser) "Downloads all of the project's dependencies"
    , commandParser "install" (Install <$> installArgsParser) "Compile the project's dependencies"
    , commandParser "build" (Build <$> buildArgsParser) "Compile the project"
    , commandParser "run" (Run <$> runArgsParser) "Run the project"
    , commandParser "test" (Test <$> testArgsParser) "Test the project"
    , commandParser "bundle" (Bundle <$> bundleArgsParser) "Bundle the project in a single file"
    , commandParser "sources" (Sources <$> sourcesArgsParser) "List all the source paths (globs) for the dependencies of the project"
    , commandParser "repl" (Repl <$> replArgsParser) "Start a REPL"
    , commandParser "publish" (Publish <$> publishArgsParser) "Publish a package"

    , commandParser "docs" (Docs <$> docsArgsParser) "Generate docs for the project and its dependencies"
    , O.command "registry"
        ( O.info
            ( O.hsubparser $ Foldable.fold
                [ commandParser "search" (RegistrySearch <$> registrySearchArgsParser) "Search for package names in the Registry"
                , commandParser "info" (RegistryInfo <$> registryInfoArgsParser) "Query the Registry for information about packages and versions"
                , commandParser "package-sets" (RegistryPackageSets <$> registryPackageSetsArgsParser) "List the available package sets"
                ]
            )
            (O.progDesc "Commands to interact with the Registry")
        )
    , O.command "ls"
        ( O.info
            ( O.hsubparser $ Foldable.fold
                [ commandParser "packages" (LsPackages <$> lsPackagesArgsParser) "List packages available in the local package set"
                , commandParser "deps" (LsDeps <$> lsDepsArgsParser) "List dependencies of the project"
                ]
            )
            (O.progDesc "List packages or dependencies")
        )
    ]

{-

TODO: add flag for overriding the cache location

    buildOptions  = BuildOptions <$> watch <*> clearScreen <*> allowIgnored <*> sourcePaths <*> srcMapFlag <*> noInstall
                    <*> pursArgs <*> depsOnly <*> beforeCommands <*> thenCommands <*> elseCommands

-}

-- https://stackoverflow.com/questions/45395369/how-to-get-console-log-line-numbers-shown-in-nodejs
-- TODO: veryVerbose = CLI.switch "very-verbose" 'V' "Enable more verbosity: timestamps and source locations"

globalArgsParser :: Parser GlobalArgs
globalArgsParser =
  Optparse.fromRecord
    { quiet: Flags.quiet
    , verbose: Flags.verbose
    , noColor: Flags.noColor
    }

initArgsParser :: Parser InitArgs
initArgsParser =
  Optparse.fromRecord
    { setVersion: Flags.maybeSetVersion
    , name: Flags.maybePackageName
    , useSolver: Flags.useSolver
    }

fetchArgsParser :: Parser FetchArgs
fetchArgsParser =
  Optparse.fromRecord
    { packages: Flags.packages
    , selectedPackage: Flags.selectedPackage
    , ensureRanges: Flags.ensureRanges
    , testDeps: Flags.testDeps
    }

sourcesArgsParser :: Parser SourcesArgs
sourcesArgsParser =
  Optparse.fromRecord
    { selectedPackage: Flags.selectedPackage
    , json: Flags.json
    }

installArgsParser :: Parser InstallArgs
installArgsParser =
  Optparse.fromRecord
    { packages: Flags.packages
    , selectedPackage: Flags.selectedPackage
    , pursArgs: Flags.pursArgs
    , backendArgs: Flags.backendArgs
    , output: Flags.output
    , pedanticPackages: Flags.pedanticPackages
    , ensureRanges: Flags.ensureRanges
    , testDeps: Flags.testDeps
    }

buildArgsParser :: Parser (BuildArgs ())
buildArgsParser = Optparse.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , backendArgs: Flags.backendArgs
  , output: Flags.output
  , pedanticPackages: Flags.pedanticPackages
  , ensureRanges: Flags.ensureRanges
  , jsonErrors: Flags.jsonErrors
  , strict: Flags.strict
  , censorBuildWarnings: Flags.censorBuildWarnings
  , showSource: Flags.showSource
  , censorCodes: Flags.censorCodes
  , filterCodes: Flags.filterCodes
  , statVerbosity: Flags.statVerbosity
  , persistWarnings: Flags.persistWarnings
  }

replArgsParser :: Parser ReplArgs
replArgsParser = Optparse.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , backendArgs: Flags.backendArgs
  }

runArgsParser :: Parser RunArgs
runArgsParser = Optparse.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , backendArgs: Flags.backendArgs
  , execArgs: Flags.execArgs
  , output: Flags.output
  , pedanticPackages: Flags.pedanticPackages
  , main: Flags.moduleName
  , ensureRanges: Flags.ensureRanges
  , strict: Flags.strict
  , censorBuildWarnings: Flags.censorBuildWarnings
  , showSource: Flags.showSource
  , censorCodes: Flags.censorCodes
  , filterCodes: Flags.filterCodes
  , statVerbosity: Flags.statVerbosity
  , persistWarnings: Flags.persistWarnings
  }

testArgsParser :: Parser TestArgs
testArgsParser = Optparse.fromRecord
  { selectedPackage: Flags.selectedPackage
  , pursArgs: Flags.pursArgs
  , backendArgs: Flags.backendArgs
  , execArgs: Flags.execArgs
  , output: Flags.output
  , pedanticPackages: Flags.pedanticPackages
  , strict: Flags.strict
  , censorBuildWarnings: Flags.censorBuildWarnings
  , showSource: Flags.showSource
  , censorCodes: Flags.censorCodes
  , filterCodes: Flags.filterCodes
  , statVerbosity: Flags.statVerbosity
  , persistWarnings: Flags.persistWarnings
  }

bundleArgsParser :: Parser BundleArgs
bundleArgsParser =
  Optparse.fromRecord
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
    , strict: Flags.strict
    , censorBuildWarnings: Flags.censorBuildWarnings
    , showSource: Flags.showSource
    , censorCodes: Flags.censorCodes
    , filterCodes: Flags.filterCodes
    , statVerbosity: Flags.statVerbosity
    , persistWarnings: Flags.persistWarnings
    }

publishArgsParser :: Parser PublishArgs
publishArgsParser =
  Optparse.fromRecord
    { selectedPackage: Flags.selectedPackage
    }

docsArgsParser :: Parser DocsArgs
docsArgsParser = Optparse.fromRecord
  -- TODO: --deps-only
  -- , depsOnly: Flags.depsOnly
  { depsOnly: pure false :: Parser Boolean
  , docsFormat: parseFormat <$>
      Maybe.optional
        ( O.strOption
            ( O.long "format"
                <> O.short 'f'
                <> O.metavar "FORMAT"
                <> O.help "Docs output format (markdown | html | etags | ctags)"
            )
        )
  }
  where
  parseFormat :: Maybe String -> Purs.DocsFormat
  parseFormat val = fromMaybe Purs.Html
    $ val
    >>= Purs.parseDocsFormat

registrySearchArgsParser :: Parser RegistrySearchArgs
registrySearchArgsParser =
  Optparse.fromRecord
    { package: Flags.package
    , json: Flags.json
    }

registryInfoArgsParser :: Parser RegistryInfoArgs
registryInfoArgsParser =
  Optparse.fromRecord
    { package: Flags.package
    , json: Flags.json
    }

registryPackageSetsArgsParser :: Parser RegistryPackageSetsArgs
registryPackageSetsArgsParser =
  Optparse.fromRecord
    { json: Flags.json
    , latest: Flags.latest
    }

lsPackagesArgsParser :: Parser LsPackagesArgs
lsPackagesArgsParser = Optparse.fromRecord
  { json: Flags.json
  }

lsDepsArgsParser :: Parser LsDepsArgs
lsDepsArgsParser = Optparse.fromRecord
  { json: Flags.json
  , transitive: Flags.transitive
  , selectedPackage: Flags.selectedPackage
  }

data Cmd a = Cmd'SpagoCmd (SpagoCmd a) | Cmd'VersionCmd Boolean

parseArgs :: Effect (Cmd ())
parseArgs = do
  O.customExecParser
    ( O.defaultPrefs # \(ParserPrefs p) -> ParserPrefs
        ( p
            { prefShowHelpOnError = true
            , prefShowHelpOnEmpty = true
            }
        )
    )
    ( O.info
        ( O.helper <*>
            ( (Cmd'SpagoCmd <$> argParser) <|>
                (Cmd'VersionCmd <$> (O.switch (O.long "version" <> O.short 'v' <> O.help "Show the current version")))
            )
        )
        (O.progDesc "PureScript package manager and build tool")
    )

main :: Effect Unit
main =
  parseArgs >>=
    \c -> Aff.launchAff_ case c of
      Cmd'SpagoCmd (SpagoCmd globalArgs command) -> do
        logOptions <- mkLogOptions globalArgs
        runSpago { logOptions } case command of
          Sources args -> do
            { env } <- mkFetchEnv { packages: mempty, selectedPackage: args.selectedPackage, ensureRanges: false, testDeps: false }
            void $ runSpago env (Sources.run { json: args.json })
          Init args@{ useSolver } -> do
            -- Figure out the package name from the current dir
            let candidateName = fromMaybe (String.take 50 $ Path.basename Paths.cwd) args.name
            logDebug [ show Paths.cwd, show candidateName ]
            packageName <- case PackageName.parse (PackageName.stripPureScriptPrefix candidateName) of
              Left err -> die [ "Could not figure out a name for the new package. Error:", show err ]
              Right p -> pure p
            setVersion <- for args.setVersion $ parseLenientVersion >>> case _ of
              Left err -> die [ "Could not parse provided set version. Error:", show err ]
              Right v -> pure v
            logDebug [ "Got packageName and setVersion:", PackageName.print packageName, unsafeStringify setVersion ]
            let initOpts = { packageName, setVersion, useSolver }
            -- Fetch the registry here so we can select the right package set later
            env <- mkRegistryEnv
            void $ runSpago env $ Init.run initOpts
            logInfo "Set up a new Spago project."
            logInfo "Try running `spago run`"
          Fetch args -> do
            { env, fetchOpts } <- mkFetchEnv args
            void $ runSpago env (Fetch.run fetchOpts)
          RegistrySearch args -> do
            env <- mkRegistryEnv
            void $ runSpago env (RegistryCmd.search args)
          RegistryInfo args -> do
            env <- mkRegistryEnv
            void $ runSpago env (RegistryCmd.info args)
          RegistryPackageSets args -> do
            env <- mkRegistryEnv
            void $ runSpago env (RegistryCmd.packageSets args)
          Install args@{ packages, selectedPackage, ensureRanges, testDeps } -> do
            { env, fetchOpts } <- mkFetchEnv { packages, selectedPackage, ensureRanges, testDeps }
            -- TODO: --no-fetch flag
            packageDependencies <- runSpago env (Fetch.run fetchOpts)
            let
              buildArgs = Record.merge
                args
                { censorBuildWarnings: Nothing :: Maybe Core.CensorBuildWarnings
                , censorCodes: Nothing :: Maybe (NonEmptySet String)
                , filterCodes: Nothing :: Maybe (NonEmptySet String)
                , statVerbosity: Nothing :: Maybe Core.StatVerbosity
                , showSource: Nothing :: Maybe Core.ShowSourceCode
                , strict: Nothing :: Maybe Boolean
                , persistWarnings: Nothing :: Maybe Boolean
                }
            env' <- runSpago env (mkBuildEnv buildArgs packageDependencies)
            let options = { depsOnly: true, pursArgs: List.toUnfoldable args.pursArgs, jsonErrors: false }
            runSpago env' (Build.run options)
          Build args@{ selectedPackage, ensureRanges, jsonErrors } -> do
            { env, fetchOpts } <- mkFetchEnv { packages: mempty, selectedPackage, ensureRanges, testDeps: false }
            -- TODO: --no-fetch flag
            packageDependencies <- runSpago env (Fetch.run fetchOpts)
            buildEnv <- runSpago env (mkBuildEnv args packageDependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs, jsonErrors }
            runSpago buildEnv (Build.run options)
          Publish { selectedPackage } -> do
            { env, fetchOpts } <- mkFetchEnv { packages: mempty, selectedPackage, ensureRanges: false, testDeps: false }
            -- TODO: --no-fetch flag
            packageDependencies <- runSpago env (Fetch.run fetchOpts)
            publishEnv <- runSpago env (mkPublishEnv packageDependencies)
            void $ runSpago publishEnv (Publish.publish {})

          Repl args@{ selectedPackage } -> do
            packages <- FS.exists "spago.yaml" >>= case _ of
              true -> do
                -- if we have a config then we assume it's a workspace, and we can run a repl in the project
                pure mempty -- TODO newPackages
              false -> do
                -- otherwise we are running a "global repl" and need to make a tmp folder and invent a config
                logWarn "No configuration found, creating a temporary project to run a repl in..."
                tmpDir <- mkTemp
                FS.mkdirp tmpDir
                logDebug $ "Creating repl project in temp dir: " <> tmpDir
                liftEffect $ Process.chdir tmpDir
                env <- mkRegistryEnv
                void $ runSpago env $ Init.run
                  { setVersion: Nothing
                  , packageName: UnsafeCoerce.unsafeCoerce "repl"
                  , useSolver: true
                  }
                pure $ List.fromFoldable [ "effect", "console" ] -- TODO newPackages
            { env, fetchOpts } <- mkFetchEnv
              { packages
              , selectedPackage
              , ensureRanges: false
              , testDeps: false
              }
            packageDependencies <- runSpago env (Fetch.run fetchOpts)
            supportPackages <- runSpago env (Fetch.replSupportPackage env.workspace.packageSet)
            replEnv <- runSpago env (mkReplEnv args packageDependencies supportPackages)
            void $ runSpago replEnv Repl.run

          Bundle args@{ selectedPackage, ensureRanges } -> do
            { env, fetchOpts } <- mkFetchEnv { packages: mempty, selectedPackage, ensureRanges, testDeps: false }
            -- TODO: --no-fetch flag
            packageDependencies <- runSpago env (Fetch.run fetchOpts)
            -- TODO: --no-build flag
            buildEnv <- runSpago env (mkBuildEnv args packageDependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs, jsonErrors: false }
            runSpago buildEnv (Build.run options)
            bundleEnv <- runSpago env (mkBundleEnv args)
            runSpago bundleEnv Bundle.run
          Run args@{ selectedPackage, ensureRanges } -> do
            { env, fetchOpts } <- mkFetchEnv { packages: mempty, selectedPackage, ensureRanges, testDeps: false }
            -- TODO: --no-fetch flag
            packageDependencies <- runSpago env (Fetch.run fetchOpts)
            -- TODO: --no-build flag
            buildEnv <- runSpago env (mkBuildEnv args packageDependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs, jsonErrors: false }
            runSpago buildEnv (Build.run options)
            runEnv <- runSpago env (mkRunEnv args buildEnv)
            runSpago runEnv Run.run
          Test args@{ selectedPackage } -> do
            { env, fetchOpts } <- mkFetchEnv { packages: mempty, selectedPackage, ensureRanges: false, testDeps: false }
            -- TODO: --no-fetch flag
            packageDependencies <- runSpago env (Fetch.run fetchOpts)
            -- TODO: --no-build flag
            buildEnv <- runSpago env (mkBuildEnv (Record.union args { ensureRanges: false }) packageDependencies)
            let options = { depsOnly: false, pursArgs: List.toUnfoldable args.pursArgs, jsonErrors: false }
            runSpago buildEnv (Build.run options)
            testEnv <- runSpago env (mkTestEnv args buildEnv)
            runSpago testEnv Test.run
          LsPackages args -> do
            let fetchArgs = { packages: mempty, selectedPackage: Nothing, ensureRanges: false, testDeps: false }
            { env: env@{ workspace }, fetchOpts } <- mkFetchEnv fetchArgs
            -- TODO: --no-fetch flag
            packageDependencies <- runSpago env (Fetch.run fetchOpts)
            let lsEnv = { workspace, packageDependencies, logOptions }
            runSpago lsEnv (Ls.listPackageSet args)
          LsDeps { selectedPackage, json, transitive } -> do
            let fetchArgs = { packages: mempty, selectedPackage, ensureRanges: false, testDeps: false }
            { env, fetchOpts } <- mkFetchEnv fetchArgs
            -- TODO: --no-fetch flag
            packageDependencies <- runSpago env (Fetch.run fetchOpts)
            lsEnv <- runSpago env (mkLsEnv packageDependencies)
            runSpago lsEnv (Ls.listPackages { json, transitive })
          Docs args -> do
            { env, fetchOpts } <- mkFetchEnv { packages: mempty, selectedPackage: Nothing, ensureRanges: false, testDeps: true }
            -- TODO: --no-fetch flag
            packageDependencies <- runSpago env (Fetch.run fetchOpts)
            docsEnv <- runSpago env (mkDocsEnv args packageDependencies)
            runSpago docsEnv Docs.run

      Cmd'VersionCmd v -> do when v printVersion
  where
  printVersion = do
    logOptions <- mkLogOptions { noColor: false, quiet: false, verbose: false }
    runSpago { logOptions } do
      logInfo BuildInfo.buildInfo.spagoVersion

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
  let bundleOptions = { minify, module: entrypoint, outfile, platform, type: bundleType, extraArgs: fromMaybe [] (bundleConf _.extra_args) }
  let
    newWorkspace = workspace
      { buildOptions
          { output = bundleArgs.output <|> workspace.buildOptions.output
          , pedanticPackages = bundleArgs.pedanticPackages || workspace.buildOptions.pedanticPackages
          }
      }
  esbuild <- Esbuild.getEsbuild
  let bundleEnv = { esbuild, logOptions, workspace: newWorkspace, selected, bundleOptions }
  pure bundleEnv

mkRunEnv :: forall a b. RunArgs -> Build.BuildEnv b -> Spago (Fetch.FetchEnv a) (Run.RunEnv ())
mkRunEnv runArgs { packageDependencies, purs } = do
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
  let runEnv = { logOptions, workspace: newWorkspace, selected, node, runOptions, packageDependencies, purs }
  pure runEnv

mkTestEnv :: forall a b. TestArgs -> Build.BuildEnv b -> Spago (Fetch.FetchEnv a) (Test.TestEnv ())
mkTestEnv testArgs { packageDependencies, purs } = do
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
  let testEnv = { logOptions, workspace: newWorkspace, selectedPackages, node, packageDependencies, purs }
  pure testEnv

mkBuildEnv
  :: forall r
   . { backendArgs :: List String
     , output :: Maybe String
     , pedanticPackages :: Boolean
     , censorBuildWarnings :: Maybe Core.CensorBuildWarnings
     , censorCodes :: Maybe (NonEmptySet String)
     , filterCodes :: Maybe (NonEmptySet String)
     , statVerbosity :: Maybe Core.StatVerbosity
     , showSource :: Maybe Core.ShowSourceCode
     , strict :: Maybe Boolean
     , persistWarnings :: Maybe Boolean
     | r
     }
  -> Map PackageName PackageMap
  -> Spago (Fetch.FetchEnv ()) (Build.BuildEnv ())
mkBuildEnv buildArgs packageDependencies = do
  { logOptions, workspace, git } <- ask
  purs <- Purs.getPurs
  let
    newWorkspace = workspace
      { buildOptions
          { output = buildArgs.output <|> workspace.buildOptions.output
          , pedanticPackages = buildArgs.pedanticPackages || workspace.buildOptions.pedanticPackages
          , censorBuildWarnings = buildArgs.censorBuildWarnings <|> workspace.buildOptions.censorBuildWarnings
          , censorCodes = buildArgs.censorCodes <|> workspace.buildOptions.censorCodes
          , filterCodes = buildArgs.filterCodes <|> workspace.buildOptions.filterCodes
          , statVerbosity = buildArgs.statVerbosity <|> workspace.buildOptions.statVerbosity
          , showSource = buildArgs.showSource <|> workspace.buildOptions.showSource
          , strict = buildArgs.strict <|> workspace.buildOptions.strict
          , persistWarnings = buildArgs.persistWarnings <|> workspace.buildOptions.persistWarnings
          }
      -- Override the backend args from the config if they are passed in through a flag
      , backend = map
          ( \b -> case List.null buildArgs.backendArgs of
              true -> b
              false -> b { args = Just (Array.fromFoldable buildArgs.backendArgs) }
          )
          workspace.backend
      }

  pure
    { logOptions
    , purs
    , git
    , packageDependencies
    , workspace: newWorkspace
    }

mkPublishEnv :: forall a. Map PackageName PackageMap -> Spago (Fetch.FetchEnv a) (Publish.PublishEnv a)
mkPublishEnv packageDependencies = do
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
  pure (Record.union { selected, packageDependencies } env)

mkReplEnv :: forall a. ReplArgs -> Map PackageName PackageMap -> PackageMap -> Spago (Fetch.FetchEnv a) (Repl.ReplEnv ())
mkReplEnv replArgs packageDependencies supportPackage = do
  { workspace, logOptions } <- ask
  logDebug $ "Repl args: " <> show replArgs

  purs <- Purs.getPurs

  selected <- case workspace.selected of
    Just s -> pure $ Right s
    Nothing -> pure $ Left $ Config.getWorkspacePackages workspace.packageSet

  pure
    { purs
    , packageDependencies
    , supportPackage
    , depsOnly: false
    , logOptions
    , pursArgs: Array.fromFoldable replArgs.pursArgs
    , selected
    }

mkFetchEnv :: forall a. FetchArgs -> Spago (LogEnv a) { env :: Fetch.FetchEnv (), fetchOpts :: Fetch.FetchOpts }
mkFetchEnv args = do
  let
    parsePackageName p = case PackageName.parse p of
      Right pkg -> Right pkg
      Left err -> Left ("- Could not parse package " <> show p <> ": " <> err)
  let { right: packageNames, left: failedPackageNames } = partitionMap parsePackageName (Array.fromFoldable args.packages)
  unless (Array.null failedPackageNames) do
    die $ [ toDoc "Failed to parse some package name: " ] <> map (indent <<< toDoc) failedPackageNames

  maybeSelectedPackage <- for args.selectedPackage $ PackageName.parse >>> case _ of
    Right p -> pure p
    Left _err -> die $ "Failed to parse selected package name, was: " <> show args.selectedPackage

  env <- mkRegistryEnv
  workspace <- runSpago env (Config.readWorkspace maybeSelectedPackage)
  let fetchOpts = { packages: packageNames, ensureRanges: args.ensureRanges, isTest: args.testDeps }
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

  -- Make sure we have git and purs
  git <- Git.getGit
  purs <- Purs.getPurs

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
      [ Git.fetchRepo { git: "https://github.com/purescript/registry-index.git", ref: "main" } Paths.registryIndexPath >>= case _ of
          Right _ -> pure unit
          Left _err -> logWarn "Couldn't refresh the registry-index, will proceed anyways"
      , Git.fetchRepo { git: "https://github.com/purescript/registry.git", ref: "main" } Paths.registryPath >>= case _ of
          Right _ -> pure unit
          Left _err -> logWarn "Couldn't refresh the registry, will proceed anyways"
      ]

  -- Now that we are up to date with the Registry we init/refresh the database
  db <- liftEffect $ Db.connect
    { database: Db.databasePath
    , logger: \str -> Reader.runReaderT (logDebug $ "DB: " <> str) { logOptions }
    }
  Registry.updatePackageSetsDb db

  pure
    { getManifestFromIndex
    , getMetadata
    , logOptions
    , purs
    , git
    , db
    }

mkLsEnv :: forall a. Map PackageName PackageMap -> Spago (Fetch.FetchEnv a) Ls.LsEnv
mkLsEnv packageDependencies = do
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
  pure { logOptions, workspace, packageDependencies, selected }

mkDocsEnv :: forall a. DocsArgs -> Map PackageName PackageMap -> Spago (Fetch.FetchEnv a) Docs.DocsEnv
mkDocsEnv args packageDependencies = do
  { logOptions, workspace } <- ask
  purs <- Purs.getPurs
  let
    env :: Docs.DocsEnv
    env = { purs, logOptions, workspace, packageDependencies, depsOnly: args.depsOnly, docsFormat: args.docsFormat }
  pure env

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
        logDebug "Registry index is old, refreshing canary"
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
