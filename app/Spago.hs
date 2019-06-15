module Spago (main) where

import           Spago.Prelude

import qualified Data.Text           as Text
import           Data.Version        (showVersion)
import qualified GHC.IO.Encoding
import qualified Options.Applicative as Opts
import qualified Paths_spago         as Pcli
import qualified System.Environment  as Env
import qualified Turtle              as CLI

import           Spago.Build         (BuildOptions (..), ExtraArg (..), ModuleName (..),
                                      NoBuild (..), SourcePath (..), TargetPath (..), Watch (..),
                                      WithMain (..))
import qualified Spago.Build
import           Spago.GlobalCache   (CacheFlag (..))
import           Spago.Messages      as Messages
import           Spago.Packages      (PackageName (..), PackagesFilter (..), JsonFlag(..))
import qualified Spago.Packages
import qualified Spago.PscPackage    as PscPackage


-- | Commands that this program handles
data Command

  -- | ### Commands for working with Spago projects
  --
  -- | Initialize a new project
  = Init Bool

  -- | Install (download) dependencies defined in spago.dhall
  | Install (Maybe Int) (Maybe CacheFlag) [PackageName]

  -- | Get source globs of dependencies in spago.dhall
  | Sources

  -- | Start a REPL.
  | Repl [SourcePath] [ExtraArg]

  -- | Generate documentation for the project and its dependencies
  | Docs [SourcePath]

  -- | Build the project paths src/ and test/ plus the specified source paths
  | Build BuildOptions

  -- | List available packages
  | ListPackages (Maybe PackagesFilter) JsonFlag

  -- | Verify that a single package is consistent with the Package Set
  | Verify (Maybe Int) (Maybe CacheFlag) PackageName

    -- | Verify that the Package Set is correct
  | VerifySet (Maybe Int) (Maybe CacheFlag)

  -- | Test the project with some module, default Test.Main
  | Test (Maybe ModuleName) BuildOptions

  -- | Run the project with some module, default Main
  | Run (Maybe ModuleName) BuildOptions

  -- | Bundle the project into an executable
  --   Builds the project before bundling
  | BundleApp (Maybe ModuleName) (Maybe TargetPath) NoBuild BuildOptions

  -- | Bundle a module into a CommonJS module
  --   Builds the project before bundling
  | BundleModule (Maybe ModuleName) (Maybe TargetPath) NoBuild BuildOptions

  -- | Upgrade the package-set to the latest release
  | PackageSetUpgrade

  -- | Freeze the package-set so it will be cached
  | Freeze

  -- | ### Commands for working with Psc-Package
  --
  --   Do the boilerplate of the local project setup to override and add arbitrary packages
  --   See the package-sets docs about this here:
  --   https://github.com/purescript/package-sets
  | PscPackageLocalSetup Bool

  -- | Do the Ins-Dhall-ation of the local project setup, equivalent to:
  --   ```sh
  --   NAME='local'
  --   TARGET=.psc-package/$NAME/.set/packages.json
  --   mktree -p .psc-package/$NAME/.set
  --   dhall-to-json --pretty <<< './packages.dhall' > $TARGET
  --   echo wrote packages.json to $TARGET
  --   ```
  | PscPackageInsDhall

  -- | Deletes the .psc-package folder
  | PscPackageClean


  -- | Show version
  | Version


  -- | Bundle the project into an executable (replaced by BundleApp)
  | Bundle

  -- | Bundle a module into a CommonJS module (replaced by BundleModule)
  | MakeModule

parser :: CLI.Parser (Command, GlobalOptions)
parser = do
  opts <- globalOptions
  command <- projectCommands <|> packageSetCommands <|> pscPackageCommands <|> otherCommands <|> oldCommands
  pure (command, opts)
  where
    force       = CLI.switch "force" 'f' "Overwrite any project found in the current directory"
    verbose     = CLI.switch "verbose" 'v' "Enable additional debug logging, e.g. printing `purs` commands"
    watchBool   = CLI.switch "watch" 'w' "Watch for changes in local files and automatically rebuild"
    noBuildBool = CLI.switch "no-build" 's' "Skip build step"
    watch = do
      res <- watchBool
      pure $ case res of
        True  -> Watch
        False -> BuildOnce
    noBuild = do
      res <- noBuildBool
      pure $ case res of
        True  -> NoBuild
        False -> DoBuild
    jsonFlagBool = CLI.switch "json" 'j' "Produce JSON output"
    jsonFlag = do
      res <- jsonFlagBool
      pure $ case res of
        True  -> JsonOutputYes
        False -> JsonOutputNo
    cacheFlag =
      let wrap = \case
            "skip" -> Just SkipCache
            "update" -> Just NewCache
            _ -> Nothing
      in CLI.optional $ CLI.opt wrap "global-cache" 'c' "Configure the global caching behaviour: skip it with `skip` or force update with `update`"
    mainModule  = CLI.optional (CLI.opt (Just . ModuleName) "main" 'm' "Module to be used as the application's entry point")
    toTarget    = CLI.optional (CLI.opt (Just . TargetPath) "to" 't' "The target file path")
    limitJobs   = CLI.optional (CLI.optInt "jobs" 'j' "Limit the amount of jobs that can run concurrently")
    sourcePaths = CLI.many (CLI.opt (Just . SourcePath) "path" 'p' "Source path to include")
    packageName = CLI.arg (Just . PackageName) "package" "Specify a package name. You can list them with `list-packages`"
    packageNames = CLI.many $ CLI.arg (Just . PackageName) "package" "Package name to add as dependency"
    passthroughArgs = many $ CLI.arg (Just . ExtraArg) " ..any `purs compile` option" "Options passed through to `purs compile`; use -- to separate"
    buildOptions = BuildOptions <$> limitJobs <*> cacheFlag <*> watch <*> sourcePaths <*> passthroughArgs
    globalOptions = GlobalOptions <$> verbose
    packagesFilter =
      let wrap = \case
            "direct"     -> Just DirectDeps
            "transitive" -> Just TransitiveDeps
            _            -> Nothing
      in CLI.optional $ CLI.opt wrap "filter" 'f' "Filter packages: direct deps with `direct`, transitive ones with `transitive`"

    projectCommands = CLI.subcommandGroup "Project commands:"
      [ initProject
      , build
      , repl
      , test
      , run
      , bundleApp
      , bundleModule
      , docs
      ]

    initProject =
      ( "init"
      , "Initialize a new sample project, or migrate a psc-package one"
      , Init <$> force
      )

    build =
      ( "build"
      , "Install the dependencies and compile the current package"
      , Build <$> buildOptions
      )

    repl =
      ( "repl"
      , "Start a REPL"
      , Repl <$> sourcePaths <*> passthroughArgs
      )

    test =
      ( "test"
      , "Test the project with some module, default Test.Main"
      , Test <$> mainModule <*> buildOptions
      )

    run =
      ( "run"
      , "Runs the project with some module, default Main"
      , Run <$> mainModule <*> buildOptions
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
      , Docs <$> sourcePaths
      )


    packageSetCommands = CLI.subcommandGroup "Package set commands:"
      [ install
      , sources
      , listPackages
      , verify
      , verifySet
      , packageSetUpgrade
      , freeze
      ]

    install =
      ( "install"
      , "Install (download) all dependencies listed in spago.dhall"
      , Install <$> limitJobs <*> cacheFlag <*> packageNames
      )

    sources =
      ( "sources"
      , "List all the source paths (globs) for the dependencies of the project"
      , pure Sources
      )

    listPackages =
      ( "list-packages"
      , "List packages available in your packages.dhall"
      , ListPackages <$> packagesFilter <*> jsonFlag
      )

    verify =
      ( "verify"
      , "Verify that a single package is consistent with the Package Set"
      , Verify <$> limitJobs <*> cacheFlag <*> packageName
      )

    verifySet =
      ( "verify-set"
      , "Verify that the whole Package Set builds correctly"
      , VerifySet <$> limitJobs <*> cacheFlag
      )

    packageSetUpgrade =
      ( "package-set-upgrade"
      , "Upgrade the upstream in packages.dhall to the latest package-sets release"
      , pure PackageSetUpgrade
      )

    freeze =
      ( "freeze"
      , "Recompute the hashes for the package-set"
      , pure Freeze
      )


    pscPackageCommands = CLI.subcommandGroup "Psc-Package compatibility commands:"
      [ pscPackageLocalSetup
      , pscPackageInsDhall
      , pscPackageClean
      ]

    pscPackageLocalSetup =
      ( "psc-package-local-setup"
      , "Setup a local package set by creating a new packages.dhall"
      , PscPackageLocalSetup <$> force
      )

    pscPackageInsDhall =
      ( "psc-package-insdhall"
      , "Insdhall the local package set from packages.dhall"
      , pure PscPackageInsDhall
      )

    pscPackageClean =
      ( "psc-package-clean"
      , "Clean cached packages by deleting the .psc-package folder"
      , pure PscPackageClean
      )


    otherCommands = CLI.subcommandGroup "Other commands:"
      [ version
      ]

    version =
      ( "version"
      , "Show spago version"
      , pure Version
      )


    oldCommands = Opts.subparser $ Opts.internal <> bundle <> makeModule

    bundle =
      Opts.command "bundle" $ Opts.info (Bundle <$ mainModule <* toTarget <* noBuild <* buildOptions) mempty

    makeModule =
      Opts.command "make-module" $ Opts.info (MakeModule <$ mainModule <* toTarget <* noBuild <* buildOptions) mempty


main :: IO ()
main = do
  -- We always want to run in UTF8 anyways
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
  -- Stop `git` from asking for input, not gonna happen
  -- We just fail instead. Source:
  -- https://serverfault.com/questions/544156
  Env.setEnv "GIT_TERMINAL_PROMPT" "0"

  -- | Print out Spago version
  let printVersion = CLI.echo $ CLI.unsafeTextToLine $ Text.pack $ showVersion Pcli.version

  (command, globalOptions) <- CLI.options "Spago - manage your PureScript projects" parser
  (flip runReaderT) globalOptions $
    case command of
      Init force                            -> Spago.Packages.initProject force
      Install limitJobs cacheConfig packageNames
        -> Spago.Packages.install limitJobs cacheConfig packageNames
      ListPackages packagesFilter jsonFlag  -> Spago.Packages.listPackages packagesFilter jsonFlag
      Sources                               -> Spago.Packages.sources
      Verify limitJobs cacheConfig package  -> Spago.Packages.verify limitJobs cacheConfig (Just package)
      VerifySet limitJobs cacheConfig       -> Spago.Packages.verify limitJobs cacheConfig Nothing
      PackageSetUpgrade                     -> Spago.Packages.upgradePackageSet
      Freeze                                -> Spago.Packages.freeze
      Build buildOptions                    -> Spago.Build.build buildOptions Nothing
      Test modName buildOptions             -> Spago.Build.test modName buildOptions
      Run modName buildOptions              -> Spago.Build.run modName buildOptions
      Repl paths pursArgs                   -> Spago.Build.repl paths pursArgs
      BundleApp modName tPath shouldBuild buildOptions
        -> Spago.Build.bundleApp WithMain modName tPath shouldBuild buildOptions
      BundleModule modName tPath shouldBuild buildOptions
        -> Spago.Build.bundleModule modName tPath shouldBuild buildOptions
      Docs sourcePaths                      -> Spago.Build.docs sourcePaths
      Version                               -> printVersion
      PscPackageLocalSetup force            -> liftIO $ PscPackage.localSetup force
      PscPackageInsDhall                    -> liftIO $ PscPackage.insDhall
      PscPackageClean                       -> liftIO $ PscPackage.clean
      Bundle                                -> die Messages.bundleCommandRenamed
      MakeModule                            -> die Messages.makeModuleCommandRenamed
