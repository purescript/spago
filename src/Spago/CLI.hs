module Spago.CLI
  ( module Spago.CLI
  , CLI.options
  ) where

import Spago.Prelude

import qualified Data.Text           as Text
import qualified Options.Applicative as Opts
import qualified Turtle              as CLI
import qualified System.Environment  as Env

import qualified Spago.Purs          as Purs
import qualified Spago.Config        as Config
import qualified Spago.Version

import           Spago.Build         (BuildOptions (..), DepsOnly (..), ExtraArg (..),
                                      ModuleName (..), NoBuild (..), NoInstall (..), NoSearch (..),
                                      OpenDocs (..), PathType (..), ShareOutput (..),
                                      SourcePath (..), TargetPath (..), Watch (..),
                                      WithSrcMap (..))
import           Spago.Dhall         (TemplateComments (..))
import           Spago.DryRun        (DryRun (..))
import           Spago.GlobalCache   (CacheFlag (..), getGlobalCacheDir)
import           Spago.Packages      (CheckModulesUnique (..), JsonFlag (..), Force(..))
import           Spago.Purs          (DocsFormat(..))
import           Spago.Version       (VersionBump (..))
import           Spago.Watch         (ClearScreen (..))

import           Spago.Env
import           Spago.Types

data GlobalOptions = GlobalOptions
  { globalQuiet       :: Bool
  , globalVerbose     :: Bool
  , globalVeryVerbose :: Bool
  , globalUseColor    :: Bool
  , globalUsePsa      :: UsePsa
  , globalJobs        :: Maybe Int
  , globalConfigPath  :: Maybe Text
  }

data IncludeTransitive = IncludeTransitive | NoIncludeTransitive


-- | Commands that this program handles
data Command
  
  -- | Initialize a new project
  = Init Force TemplateComments

  -- | Install (download) dependencies defined in spago.dhall
  | Install (Maybe CacheFlag) [PackageName]

  -- | Get source globs of dependencies in spago.dhall
  | Sources

  -- | List available packages
  | ListPackages JsonFlag

  -- | List dependencies of the project
  | ListDeps JsonFlag IncludeTransitive

  -- | Bump and tag a new version in preparation for release.
  | BumpVersion DryRun VersionBump

  -- | Save a GitHub token to cache, to authenticate to various GitHub things
  | Login

  -- | Upgrade the package-set to the latest release
  | PackageSetUpgrade

  -- | Freeze the package-set so it will be cached
  | Freeze

  -- | Runs `purescript-docs-search search`.
  | Search

  -- | Returns info about paths used by Spago
  | Path (Maybe PathType) BuildOptions

  -- | Show version
  | Version

  -- ### Build commands - i.e. they all call Purs at some point

    -- | Build the project
  | Build BuildOptions

  -- | Start a REPL
  | Repl (Maybe CacheFlag) [PackageName] [SourcePath] [ExtraArg] DepsOnly

  -- | Generate documentation for the project and its dependencies
  | Docs (Maybe DocsFormat) [SourcePath] DepsOnly NoSearch OpenDocs

  -- | Run the project with some module, default Main
  | Run (Maybe ModuleName) BuildOptions [ExtraArg]
 
  -- | Test the project with some module, default Test.Main
  | Test (Maybe ModuleName) BuildOptions [ExtraArg]

  -- | Bundle the project into an executable
  | BundleApp (Maybe ModuleName) (Maybe TargetPath) NoBuild BuildOptions

  -- | Bundle a module into a CommonJS module
  | BundleModule (Maybe ModuleName) (Maybe TargetPath) NoBuild BuildOptions
 
  -- | Verify that a single package is consistent with the Package Set
  | Verify (Maybe CacheFlag) PackageName

  -- | Verify that the Package Set is correct
  | VerifySet (Maybe CacheFlag) CheckModulesUnique

  -- ### Legacy commands

  -- | Bundle the project into an executable (replaced by BundleApp)
  | Bundle

  -- | Bundle a module into a CommonJS module (replaced by BundleModule)
  | MakeModule

  -- | List available packages (deprecated, old version of ListPackages)
  | ListPackagesOld





-- | Given the global CLI options, it creates the Env for the Spago context
--   and runs the app
runWithEnv :: GlobalOptions -> RIO Env a -> IO a
runWithEnv GlobalOptions{..} app = do
  let verbose = not globalQuiet && (globalVerbose || globalVeryVerbose)

  -- https://github.com/purescript/spago/issues/579
  maybeTerm <- Env.lookupEnv "TERM"
  let termDumb = maybeTerm == Just "dumb" || maybeTerm == Just "win"
  let useColor = globalUseColor && not termDumb

  let logHandle = stderr
  logOptions' <- logOptionsHandle logHandle verbose
  let logOptions
        = setLogUseTime globalVeryVerbose
        $ setLogUseLoc globalVeryVerbose
        $ setLogUseColor useColor
        $ setLogVerboseFormat True logOptions'
  withLogFunc logOptions $ \logFunc -> do
    let logFunc' :: LogFunc
        logFunc' = if globalQuiet
          then mkLogFunc $ \_ _ _ _ -> pure ()
          else logFunc

    let configPath = fromMaybe Config.defaultPath globalConfigPath

    globalCache <- runRIO logFunc' $ do
      logDebug "Running `getGlobalCacheDir`"
      getGlobalCacheDir

    let env = Env
          { envLogFunc = logFunc'
          , envUsePsa = globalUsePsa
          , envJobs = fromMaybe 20 globalJobs
          , envConfigPath = configPath
          , envGlobalCache = globalCache
          }
    runRIO env app


runWithBuildEnv 
  :: HasEnv env
  => RIO BuildEnv a 
  -> RIO env a
runWithBuildEnv app = do
  -- first we decide if we _want_ to use psa, then if we _can_
  usePsa <- view usePsaL
  pursCandidate <- case usePsa of
    NoPsa -> pure "purs"
    UsePsa -> findExecutable "psa" >>= \case
      Just _  -> pure "psa"
      Nothing -> pure "purs"

  buildEnvGlobal <- view envL
  buildEnvPurs <- findExecutableOrDie pursCandidate

  runRIO BuildEnv{..} app


-- data InstallConfig =
--  {  -- cacheFlag

--  }

-- BuildConfig
{-
General:
- sourcepaths
- purs-args
- deps-only
- output
- backend

Test:
- entrypoint

Run:
- entrypoint

Bundle:
- entrypoint
- target



-}


parser :: CLI.Parser (Command, GlobalOptions)
parser = do
  opts <- globalOptions
  command
    <-  projectCommands
    <|> packagesCommands
    <|> packageSetCommands
    <|> publishCommands
    <|> otherCommands
    <|> oldCommands
  pure (command, opts)
  where
    cacheFlag =
      let wrap = \case
            "skip" -> Just SkipCache
            "update" -> Just NewCache
            _ -> Nothing
      in CLI.optional $ CLI.opt wrap "global-cache" 'c' "Configure the global caching behaviour: skip it with `skip` or force update with `update`"

    beforeCommands = many $ CLI.opt Just "before" 'b' "Commands to run before a build."
    thenCommands   = many $ CLI.opt Just "then" 't' "Commands to run following a successfull build."
    elseCommands   = many $ CLI.opt Just "else" 'e' "Commands to run following an unsuccessfull build."
    versionBump = CLI.arg Spago.Version.parseVersionBump "bump" "How to bump the version. Acceptable values: 'major', 'minor', 'patch', or a version (e.g. 'v1.2.3')."

    quiet       = CLI.switch "quiet" 'q' "Suppress all spago logging"
    verbose     = CLI.switch "verbose" 'v' "Enable additional debug logging, e.g. printing `purs` commands"
    veryVerbose = CLI.switch "very-verbose" 'V' "Enable more verbosity: timestamps and source locations"
    noColor     = CLI.switch "no-color" 'C' "Log without ANSI color escape sequences"

    -- Note: the first constructor is the default when the flag is not provided
    force       = bool NoForce Force <$> CLI.switch "force" 'f' "Overwrite any project found in the current directory"
    watch       = bool BuildOnce Watch <$> CLI.switch "watch" 'w' "Watch for changes in local files and automatically rebuild"
    noInstall   = bool DoInstall NoInstall <$> CLI.switch "no-install" 'n' "Don't run the automatic installation of packages"
    depsOnly    = bool AllSources DepsOnly <$> CLI.switch "deps-only" 'd' "Only use sources from dependencies, skipping the project sources."
    noSearch    = bool AddSearch NoSearch <$> CLI.switch "no-search" 'S' "Do not make the documentation searchable"
    clearScreen = bool NoClear DoClear <$> CLI.switch "clear-screen" 'l' "Clear the screen on rebuild (watch mode only)"
    noBuild     = bool DoBuild NoBuild <$> CLI.switch "no-build" 's' "Skip build step"
    srcMapFlag  = bool WithoutSrcMap WithSrcMap <$> CLI.switch "source-maps" 'x' "Whether to generate source maps for the bundle"
    jsonFlag    = bool JsonOutputNo JsonOutputYes <$> CLI.switch "json" 'j' "Produce JSON output"
    dryRun      = bool DryRun NoDryRun <$> CLI.switch "no-dry-run" 'f' "Actually perform side-effects (the default is to describe what would be done)"
    usePsa      = bool UsePsa NoPsa <$> CLI.switch "no-psa" 'P' "Don't build with `psa`, but use `purs`"
    openDocs    = bool NoOpenDocs DoOpenDocs <$> CLI.switch "open" 'o' "Open generated documentation in browser (for HTML format only)"
    noComments  = bool WithComments NoComments <$> CLI.switch "no-comments" 'C' "Generate package.dhall and spago.dhall files without tutorial comments"
    configPath  = CLI.optional $ CLI.optText "config" 'x' "Optional config path to be used instead of the default spago.dhall"
    chkModsUniq = bool DoCheckModulesUnique NoCheckModulesUnique <$> CLI.switch "no-check-modules-unique" 'M' "Skip checking whether modules names are unique across all packages."
    transitive  = bool NoIncludeTransitive IncludeTransitive <$> CLI.switch "transitive" 't' "Include transitive dependencies"

    mainModule  = CLI.optional $ CLI.opt (Just . ModuleName) "main" 'm' "Module to be used as the application's entry point"
    toTarget    = CLI.optional $ CLI.opt (Just . TargetPath) "to" 't' "The target file path"
    docsFormat  = CLI.optional $ CLI.opt Purs.parseDocsFormat "format" 'f' "Docs output format (markdown | html | etags | ctags)"
    jobsLimit   = CLI.optional (CLI.optInt "jobs" 'j' "Limit the amount of jobs that can run concurrently")
    nodeArgs         = many $ CLI.opt (Just . ExtraArg) "node-args" 'a' "Argument to pass to node (run/test only)"
    replPackageNames = many $ CLI.opt (Just . PackageName) "dependency" 'D' "Package name to add to the REPL as dependency"
    sourcePaths      = many $ CLI.opt (Just . SourcePath) "path" 'p' "Source path to include"

    packageName     = CLI.arg (Just . PackageName) "package" "Specify a package name. You can list them with `ls packages`"
    packageNames    = many $ CLI.arg (Just . PackageName) "package" "Package name to add as dependency"
    pursArgs        = many $ CLI.opt (Just . ExtraArg) "purs-args" 'u' "Argument to pass to purs"
    -- See https://github.com/purescript/spago/pull/526 for why this flag is disabled
    useSharedOutput = bool NoShareOutput NoShareOutput <$> CLI.switch "no-share-output" 'S' "DEPRECATED: Disable using a shared output folder in location of root packages.dhall"
    buildOptions  = BuildOptions <$> cacheFlag <*> watch <*> clearScreen <*> sourcePaths <*> srcMapFlag <*> noInstall
                    <*> pursArgs <*> depsOnly <*> useSharedOutput
                    <*> beforeCommands <*> thenCommands <*> elseCommands

    -- Note: by default we limit concurrency to 20
    globalOptions = GlobalOptions <$> quiet <*> verbose <*> veryVerbose <*> (not <$> noColor) <*> usePsa
                    <*> jobsLimit <*> configPath


    initProject =
      ( "init"
      , "Initialize a new sample project, or migrate a psc-package one"
      , Init <$> force <*> noComments
      )

    build =
      ( "build"
      , "Install the dependencies and compile the current package"
      , Build <$> buildOptions
      )

    repl =
      ( "repl"
      , "Start a REPL"
      , Repl <$> cacheFlag <*> replPackageNames <*> sourcePaths <*> pursArgs <*> depsOnly
      )

    test =
      ( "test"
      , "Test the project with some module, default Test.Main"
      , Test <$> mainModule <*> buildOptions <*> nodeArgs
      )

    run =
      ( "run"
      , "Runs the project with some module, default Main"
      , Run <$> mainModule <*> buildOptions <*> nodeArgs
      )

    bundleApp =
      ( "bundle-app"
      , "Bundle the project into an executable"
      , BundleApp <$> mainModule <*> toTarget <*> noBuild <*> buildOptions
      )

    bundleModule =
      ( "bundle-module"
      , "Bundle the project into a CommonJS module"
      , BundleModule <$> mainModule <*> toTarget <*> noBuild <*> buildOptions
      )

    docs =
      ( "docs"
      , "Generate docs for the project and its dependencies"
      , Docs <$> docsFormat <*> sourcePaths <*> depsOnly <*> noSearch <*> openDocs
      )

    search =
      ( "search"
      , "Start a search REPL to find definitions matching names and types"
      , pure Search
      )

    pathSubcommand
      =   CLI.subcommand "output" "Output path for compiled code"
            (Path (Just OutputFolder) <$> buildOptions)
      <|> (Path Nothing <$> buildOptions)

    path =
      ( "path"
      , "Display paths used by the project"
      , pathSubcommand
      )

    listPackages
      = CLI.subcommand "packages" "List packages available in the local package set"
        (ListPackages <$> jsonFlag)

    listDeps
      = CLI.subcommand "deps" "List dependencies of the project"
        (ListDeps <$> jsonFlag <*> transitive)

    list =
      ( "ls"
      , "List command. Supports: `packages`, `deps`"
      , listPackages <|> listDeps
      )

    install =
      ( "install"
      , "Install (download) all dependencies listed in spago.dhall"
      , Install <$> cacheFlag <*> packageNames
      )

    sources =
      ( "sources"
      , "List all the source paths (globs) for the dependencies of the project"
      , pure Sources
      )

    verify =
      ( "verify"
      , "Verify that a single package is consistent with the Package Set"
      , Verify <$> cacheFlag <*> packageName
      )

    verifySet =
      ( "verify-set"
      , "Verify that the whole Package Set builds correctly"
      , VerifySet <$> cacheFlag <*> chkModsUniq
      )

    upgradeSet =
      ( "upgrade-set"
      , "Upgrade the upstream in packages.dhall to the latest package-sets release"
      , pure PackageSetUpgrade
      )

    freeze =
      ( "freeze"
      , "Recompute the hashes for the package-set"
      , pure Freeze
      )

    login =
      ( "login"
      , "Save the GitHub token to the global cache - set it with the SPAGO_GITHUB_TOKEN env variable"
      , pure Login
      )

    bumpVersion =
      ( "bump-version"
      , "Bump and tag a new version, and generate bower.json, in preparation for release."
      , BumpVersion <$> dryRun <*> versionBump
      )

    otherCommands = CLI.subcommandGroup "Other commands:"
      [ version
      ]

    version =
      ( "version"
      , "Show spago version"
      , pure Version
      )

    packagesFilter = CLI.optional $ CLI.opt (const Nothing) "filter" 'f' "Filter packages: direct deps with `direct`, transitive ones with `transitive`"
    listPackagesOld =
      Opts.command "list-packages" $ Opts.info (ListPackagesOld <$ packagesFilter <* jsonFlag) mempty

    bundle =
      Opts.command "bundle" $ Opts.info (Bundle <$ mainModule <* toTarget <* noBuild <* buildOptions) mempty

    makeModule =
      Opts.command "make-module" $ Opts.info (MakeModule <$ mainModule <* toTarget <* noBuild <* buildOptions) mempty


    projectCommands = CLI.subcommandGroup "Project commands:"
      [ initProject
      , build
      , repl
      , test
      , run
      , bundleApp
      , bundleModule
      , docs
      , search
      , path
      , sources
      ]
    packagesCommands = CLI.subcommandGroup "Packages commands:"
      [ install
      , list
      ]
    packageSetCommands = CLI.subcommandGroup "Package set commands:"
      [ verify
      , verifySet
      , upgradeSet
      , freeze
      ]
    publishCommands = CLI.subcommandGroup "Publish commands:"
      [ login
      , bumpVersion
      ]
    oldCommands = Opts.subparser $ Opts.internal <> bundle <> makeModule <> listPackagesOld


echo :: MonadIO m => String -> m ()
echo = CLI.echo . CLI.unsafeTextToLine . Text.pack