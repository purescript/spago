module Main (main) where

import           Spago.Prelude

import qualified Data.Text          as Text
import           Data.Version       (showVersion)
import qualified GHC.IO.Encoding
import qualified Paths_spago        as Pcli
import qualified System.Environment as Env
import qualified Turtle             as CLI

import           Spago.Build        (BuildOptions (..), ExtraArg (..), ModuleName (..),
                                     NoBuild (..), SourcePath (..), TargetPath (..), Watch (..),
                                     WithMain (..))
import qualified Spago.Build
import           Spago.Packages     (PackageName (..), PackagesFilter (..))
import qualified Spago.Packages
import qualified Spago.PscPackage   as PscPackage


-- | Commands that this program handles
data Command

  -- | ### Commands for working with Spago projects
  --
  -- | Initialize a new project
  = Init Bool

  -- | Install (download) dependencies defined in spago.dhall
  | Install (Maybe Int) [PackageName]

  -- | Get source globs of dependencies in spago.dhall
  | Sources

  -- | Start a REPL.
  | Repl [SourcePath] [ExtraArg]

  -- | Generate documentation for the project and its dependencies
  | Docs [SourcePath]

  -- | Build the project paths src/ and test/ plus the specified source paths
  | Build BuildOptions

  -- | List available packages
  | ListPackages (Maybe PackagesFilter)

  -- | Verify that a single package is consistent with the Package Set
  | Verify (Maybe Int) PackageName

    -- | Verify that the Package Set is correct
  | VerifySet (Maybe Int)

  -- | Test the project with some module, default Test.Main
  | Test (Maybe ModuleName) BuildOptions

  -- | Run the project with some module, default Main
  | Run (Maybe ModuleName) BuildOptions

  -- | Bundle the project, with optional main and target path arguments
  --   Builds the project before bundling
  | Bundle (Maybe ModuleName) (Maybe TargetPath) NoBuild BuildOptions

  -- | Bundle a module into a CommonJS module
  --   Builds the project before bundling
  | MakeModule (Maybe ModuleName) (Maybe TargetPath) NoBuild BuildOptions

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


parser :: CLI.Parser (Command, GlobalOptions)
parser = do
  opts <- globalOptions
  command <- projectCommands <|> packageSetCommands <|> pscPackageCommands <|> otherCommands
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
    mainModule  = CLI.optional (CLI.opt (Just . ModuleName) "main" 'm' "The main module to bundle")
    toTarget    = CLI.optional (CLI.opt (Just . TargetPath) "to" 't' "The target file path")
    limitJobs   = CLI.optional (CLI.optInt "jobs" 'j' "Limit the amount of jobs that can run concurrently")
    sourcePaths = CLI.many (CLI.opt (Just . SourcePath) "path" 'p' "Source path to include")
    packageName = CLI.arg (Just . PackageName) "package" "Specify a package name. You can list them with `list-packages`"
    packageNames = CLI.many $ CLI.arg (Just . PackageName) "package" "Package name to add as dependency"
    passthroughArgs = many $ CLI.arg (Just . ExtraArg) " ..any `purs compile` option" "Options passed through to `purs compile`; use -- to separate"
    buildOptions = BuildOptions <$> limitJobs <*> watch <*> sourcePaths <*> passthroughArgs
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
      , bundle
      , makeModule
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

    bundle =
      ( "bundle"
      , "Bundle the project, with optional main and target path arguments"
      , Bundle <$> mainModule <*> toTarget <*> noBuild <*> buildOptions
      )

    makeModule =
      ( "make-module"
      , "Bundle a module into a CommonJS module"
      , MakeModule <$> mainModule <*> toTarget <*> noBuild <*> buildOptions
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
      , Install <$> limitJobs <*> packageNames
      )

    sources =
      ( "sources"
      , "List all the source paths (globs) for the dependencies of the project"
      , pure Sources
      )

    listPackages =
      ( "list-packages"
      , "List packages available in your packages.dhall"
      , ListPackages <$> packagesFilter
      )

    verify =
      ( "verify"
      , "Verify that a single package is consistent with the Package Set"
      , Verify <$> limitJobs <*> packageName
      )

    verifySet =
      ( "verify-set"
      , "Verify that the whole Package Set builds correctly"
      , VerifySet <$> limitJobs
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
      Install limitJobs packageNames        -> Spago.Packages.install limitJobs packageNames
      ListPackages packagesFilter           -> Spago.Packages.listPackages packagesFilter
      Sources                               -> Spago.Packages.sources
      Verify limitJobs package              -> Spago.Packages.verify limitJobs (Just package)
      VerifySet limitJobs                   -> Spago.Packages.verify limitJobs Nothing
      PackageSetUpgrade                     -> Spago.Packages.upgradePackageSet
      Freeze                                -> Spago.Packages.freeze
      Build buildOptions                    -> Spago.Build.build buildOptions Nothing
      Test modName buildOptions             -> Spago.Build.test modName buildOptions
      Run modName buildOptions              -> Spago.Build.run modName buildOptions
      Repl paths pursArgs                   -> Spago.Build.repl paths pursArgs
      Bundle modName tPath shouldBuild buildOptions
        -> Spago.Build.bundle WithMain modName tPath shouldBuild buildOptions
      MakeModule modName tPath shouldBuild buildOptions
        -> Spago.Build.makeModule modName tPath shouldBuild buildOptions
      Docs sourcePaths                      -> Spago.Build.docs sourcePaths
      Version                               -> printVersion
      PscPackageLocalSetup force            -> liftIO $ PscPackage.localSetup force
      PscPackageInsDhall                    -> liftIO $ PscPackage.insDhall
      PscPackageClean                       -> liftIO $ PscPackage.clean
