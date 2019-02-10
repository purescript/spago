module Main (main) where

import qualified GHC.IO.Encoding
import qualified System.Environment as Env
import qualified Turtle             as T

import qualified PscPackage
import           Spago              (ModuleName (..), PackageName (..), PackagesFilter (..),
                                     PursArg (..), SourcePath (..), TargetPath (..), WithMain (..))
import qualified Spago
import qualified Spago.Config


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
  | Repl [SourcePath] [PursArg]

  -- | Build the project paths src/ and test/
  --   plus the specified source paths
  | Build (Maybe Int) [SourcePath] [PursArg]

  -- | List available packages
  | ListPackages (Maybe PackagesFilter)

  -- | Verify that the Package Set is correct (for a single package or in general)
  | Verify (Maybe Int) (Maybe PackageName)

  -- | Test the project with some module, default Test.Main
  | Test (Maybe ModuleName) (Maybe Int) [SourcePath] [PursArg]

  -- | Bundle the project, with optional main and target path arguments
  | Bundle (Maybe ModuleName) (Maybe TargetPath)

  -- | Bundle a module into a CommonJS module
  | MakeModule (Maybe ModuleName) (Maybe TargetPath)

  -- | Upgrade the package-set to the latest release
  | SpacchettiUpgrade

  -- | ### Commands for working with Psc-Package
  --
  --   Do the boilerplate of the local project setup to override and add arbitrary packages
  --   See the Spacchetti docs about this here:
  --   https://spacchetti.readthedocs.io/en/latest/local-setup.html
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
  T.<|> build
  T.<|> repl
  T.<|> test
  T.<|> bundle
  T.<|> makeModule
  T.<|> spacchettiUpgrade
  T.<|> pscPackageLocalSetup
  T.<|> pscPackageInsDhall
  T.<|> pscPackageClean
  T.<|> version
  where
    force       = T.switch "force" 'f' "Overwrite any project found in the current directory"
    mainModule  = T.optional (T.opt (Just . ModuleName) "main" 'm' "The main module to bundle")
    toTarget    = T.optional (T.opt (Just . TargetPath) "to" 't' "The target file path")
    limitJobs   = T.optional (T.optInt "jobs" 'j' "Limit the amount of jobs that can run concurrently")
    sourcePaths = T.many (T.opt (Just . SourcePath) "path" 'p' "Source path to include")
    packageName = T.optional $ T.arg (Just . PackageName) "package" "Specify the name of a package"
    packageNames = T.many $ T.arg (Just . PackageName) "package" "Package name to add as dependency"
    passthroughArgs = T.many $ T.arg (Just . PursArg) " ..any `purs` option" "Options passed through to `purs`; use -- to separate"
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
      = T.subcommand "init" "Initialize a new sample project"
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
      = T.subcommand "verify" "Verify that the Package Set is correct (for a single package or all)"
      $ Verify <$> limitJobs <*> packageName

    build
      = T.subcommand "build" "Install the dependencies and compile the current package"
      $ Build <$> limitJobs <*> sourcePaths <*> passthroughArgs

    repl
      = T.subcommand "repl" "Start a REPL"
      $ Repl <$> sourcePaths <*> passthroughArgs

    test
      = T.subcommand "test" "Test the project with some module, default Test.Main"
      $ Test <$> mainModule <*> limitJobs <*> sourcePaths <*> passthroughArgs

    bundle
      = T.subcommand "bundle" "Bundle the project, with optional main and target path arguments"
      $ Bundle <$> mainModule <*> toTarget

    makeModule
      = T.subcommand "make-module" "Bundle a module into a CommonJS module"
      $ MakeModule <$> mainModule <*> toTarget

    spacchettiUpgrade
      = T.subcommand "spacchetti-upgrade" "Upgrade \"packages.dhall\" to the latest Spacchetti release"
      $ pure SpacchettiUpgrade

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

  command <- T.options "Spago - manage your PureScript projects" parser
  case command of
    Init force                            -> Spago.initProject force
    Install limitJobs packageNames        -> Spago.install limitJobs packageNames
    ListPackages packagesFilter           -> Spago.listPackages packagesFilter
    Sources                               -> Spago.sources
    Build limitJobs paths pursArgs        -> Spago.build limitJobs paths pursArgs
    Verify limitJobs maybePackage         -> Spago.verify limitJobs maybePackage
    Test modName limitJobs paths pursArgs -> Spago.test modName limitJobs paths pursArgs
    Repl paths pursArgs                   -> Spago.repl paths pursArgs
    Bundle modName tPath                  -> Spago.bundle WithMain modName tPath
    MakeModule modName tPath              -> Spago.makeModule modName tPath
    SpacchettiUpgrade                     -> Spago.Config.upgradeSpacchetti
    Version                               -> Spago.printVersion
    PscPackageLocalSetup force            -> PscPackage.localSetup force
    PscPackageInsDhall                    -> PscPackage.insDhall
    PscPackageClean                       -> PscPackage.clean
