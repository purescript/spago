{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import qualified Data.Text          as Text
import           Data.Version       (showVersion)
import qualified GHC.IO.Encoding
import qualified Paths_spago        as Pcli
import qualified System.Environment as Env
import qualified Turtle             as T

import           Spago.Build        (ExtraArg (..), ModuleName (..), SourcePath (..),
                                     TargetPath (..), WithMain (..), Watch (..))
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

  -- | Build the project paths src/ and test/
  --   plus the specified source paths
  | Build (Maybe Int) Watch [SourcePath] [ExtraArg]

  -- | List available packages
  | ListPackages (Maybe PackagesFilter)

  -- | Verify that a single package is consistent with the Package Set
  | Verify (Maybe Int) PackageName

    -- | Verify that the Package Set is correct
  | VerifySet (Maybe Int)

  -- | Test the project with some module, default Test.Main
  | Test (Maybe ModuleName) (Maybe Int) Watch [SourcePath] [ExtraArg]

  -- | Run the project with some module, default Main
  | Run (Maybe ModuleName) (Maybe Int) Watch [SourcePath] [ExtraArg]

  -- | Bundle the project, with optional main and target path arguments
  | Bundle (Maybe ModuleName) (Maybe TargetPath)

  -- | Bundle a module into a CommonJS module
  | MakeModule (Maybe ModuleName) (Maybe TargetPath)

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


parser :: T.Parser Command
parser
      = initProject
  T.<|> install
  T.<|> sources
  T.<|> listPackages
  T.<|> verify
  T.<|> verifySet
  T.<|> build
  T.<|> repl
  T.<|> docs
  T.<|> test
  T.<|> run
  T.<|> bundle
  T.<|> makeModule
  T.<|> packageSetUpgrade
  T.<|> freeze
  T.<|> pscPackageLocalSetup
  T.<|> pscPackageInsDhall
  T.<|> pscPackageClean
  T.<|> version
  where
    force       = T.switch "force" 'f' "Overwrite any project found in the current directory"
    watchBool   = T.switch "watch" 'w' "Watch for changes in local files and automatically rebuild"
    watch = do
      res <- watchBool
      pure $ case res of
        True -> Watch
        False -> BuildOnce
    mainModule  = T.optional (T.opt (Just . ModuleName) "main" 'm' "The main module to bundle")
    toTarget    = T.optional (T.opt (Just . TargetPath) "to" 't' "The target file path")
    limitJobs   = T.optional (T.optInt "jobs" 'j' "Limit the amount of jobs that can run concurrently")
    sourcePaths = T.many (T.opt (Just . SourcePath) "path" 'p' "Source path to include")
    packageName = T.arg (Just . PackageName) "package" "Specify a package name. You can list them with `list-packages`"
    packageNames = T.many $ T.arg (Just . PackageName) "package" "Package name to add as dependency"
    passthroughArgs = T.many $ T.arg (Just . ExtraArg) " ..any `purs compile` option" "Options passed through to `purs compile`; use -- to separate"
    packagesFilter =
      let wrap = \case
            "direct"     -> Just DirectDeps
            "transitive" -> Just TransitiveDeps
            _            -> Nothing
      in T.optional $ T.opt wrap "filter" 'f' "Filter packages: direct deps with `direct`, transitive ones with `transitive`"

    pscPackageLocalSetup
      = T.subcommand "psc-package-local-setup" "Setup a local package set by creating a new packages.dhall"
      $ PscPackageLocalSetup <$> force

    pscPackageInsDhall
      = T.subcommand "psc-package-insdhall" "Insdhall the local package set from packages.dhall"
      $ pure PscPackageInsDhall

    pscPackageClean
      = T.subcommand "psc-package-clean" "Clean cached packages by deleting the .psc-package folder"
      $ pure PscPackageClean

    initProject
      = T.subcommand "init" "Initialize a new sample project, or migrate a psc-package one"
      $ Init <$> force

    install
      = T.subcommand "install" "Install (download) all dependencies listed in spago.dhall"
      $ Install <$> limitJobs <*> packageNames

    sources
      = T.subcommand "sources" "List all the source paths (globs) for the dependencies of the project"
      $ pure Sources

    listPackages
      = T.subcommand "list-packages" "List packages available in your packages.dhall"
      $ ListPackages <$> packagesFilter

    verify
      = T.subcommand "verify" "Verify that a single package is consistent with the Package Set"
      $ Verify <$> limitJobs <*> packageName

    verifySet
      = T.subcommand "verify-set" "Verify that the whole Package Set builds correctly"
      $ VerifySet <$> limitJobs

    build
      = T.subcommand "build" "Install the dependencies and compile the current package"
      $ Build <$> limitJobs <*> watch <*> sourcePaths <*> passthroughArgs

    repl
      = T.subcommand "repl" "Start a REPL"
      $ Repl <$> sourcePaths <*> passthroughArgs

    docs
      = T.subcommand "docs" "Generate docs for the project and its dependencies"
      $ Docs <$> sourcePaths

    test
      = T.subcommand "test" "Test the project with some module, default Test.Main"
      $ Test <$> mainModule <*> limitJobs <*> watch <*> sourcePaths <*> passthroughArgs

    run
      = T.subcommand "run" "Runs the project with some module, default Main"
      $ Run <$> mainModule <*> limitJobs <*> watch <*> sourcePaths <*> passthroughArgs

    bundle
      = T.subcommand "bundle" "Bundle the project, with optional main and target path arguments"
      $ Bundle <$> mainModule <*> toTarget

    makeModule
      = T.subcommand "make-module" "Bundle a module into a CommonJS module"
      $ MakeModule <$> mainModule <*> toTarget

    packageSetUpgrade
      = T.subcommand "package-set-upgrade" "Upgrade the upstream in packages.dhall to the latest package-sets release"
      $ pure PackageSetUpgrade

    freeze
      = T.subcommand "freeze" "Recompute the hashes for the package-set"
      $ pure Freeze

    version
      = T.subcommand "version" "Show spago version"
      $ pure Version

main :: IO ()
main = do
  -- We always want to run in UTF8 anyways
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
  -- Stop `git` from asking for input, not gonna happen
  -- We just fail instead. Source:
  -- https://serverfault.com/questions/544156
  Env.setEnv "GIT_TERMINAL_PROMPT" "0"

  -- | Print out Spago version
  let printVersion = T.echo $ T.unsafeTextToLine $ Text.pack $ showVersion Pcli.version

  command <- T.options "Spago - manage your PureScript projects" parser
  case command of
    Init force                            -> Spago.Packages.initProject force
    Install limitJobs packageNames        -> Spago.Packages.install limitJobs packageNames
    ListPackages packagesFilter           -> Spago.Packages.listPackages packagesFilter
    Sources                               -> Spago.Packages.sources
    Verify limitJobs package              -> Spago.Packages.verify limitJobs (Just package)
    VerifySet limitJobs                   -> Spago.Packages.verify limitJobs Nothing
    PackageSetUpgrade                     -> Spago.Packages.upgradePackageSet
    Freeze                                -> Spago.Packages.freeze
    Build limitJobs watch paths pursArgs  -> Spago.Build.build limitJobs watch paths pursArgs
    Test modName limitJobs watch paths pursArgs
                                          -> Spago.Build.test modName limitJobs watch paths pursArgs
    Run modName limitJobs watch paths pursArgs
                                          -> Spago.Build.run modName limitJobs watch paths pursArgs
    Repl paths pursArgs                   -> Spago.Build.repl paths pursArgs
    Bundle modName tPath                  -> Spago.Build.bundle WithMain modName tPath
    MakeModule modName tPath              -> Spago.Build.makeModule modName tPath
    Docs sourcePaths                      -> Spago.Build.docs sourcePaths
    Version                               -> printVersion
    PscPackageLocalSetup force            -> PscPackage.localSetup force
    PscPackageInsDhall                    -> PscPackage.insDhall
    PscPackageClean                       -> PscPackage.clean
