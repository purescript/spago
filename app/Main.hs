module Main (main) where

import qualified GHC.IO.Encoding
import qualified System.Environment as Env
import qualified Turtle             as T

import qualified PscPackage
import           Spago              (ModuleName (..), TargetPath (..), WithMain (..))
import qualified Spago


-- | Commands that this program handles
data Command

  -- | ### Commands for working with Spago projects
  --
  -- | Initialize a new project
  = Init Bool

  -- | Install (download) dependencies defined in spago.dhall
  | Install (Maybe Int)

  -- | Get source globs of dependencies in spago.dhall
  | Sources

  -- | Build the project paths src/ and test/
  --   or the specified target paths
  | Build [TargetPath] [T.Text]

  -- | List available packages
  | ListPackages

  -- | Test the project with some module, default Test.Main
  | Test (Maybe ModuleName) [TargetPath] [T.Text]

  -- | Bundle the project, with optional main and target path arguments
  | Bundle (Maybe ModuleName) (Maybe TargetPath)

  -- | Bundle a module into a CommonJS module
  | MakeModule (Maybe ModuleName) (Maybe TargetPath)


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
  T.<|> build
  T.<|> test
  T.<|> bundle
  T.<|> makeModule
  T.<|> pscPackageLocalSetup
  T.<|> pscPackageInsDhall
  T.<|> pscPackageClean
  T.<|> version
  where
    force       = T.switch "force" 'f' "Overwrite any project found in the current directory"
    mainModule  = T.optional (T.opt (Just . ModuleName) "main" 'm' "The main module to bundle")
    toTarget    = T.optional (T.opt (Just . TargetPath) "to" 't' "The target file path")
    limitJobs   = T.optional (T.optInt "jobs" 'j' "Limit the amount of jobs that can run concurrently")
    sourcePaths = T.many (T.opt (Just . TargetPath) "path" 'p' "Source path to include")

    passthroughArgs = T.many $ T.argText " ..any `purs` option" "Options passed through to `purs`; use -- to separate"

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
      $ Install <$> limitJobs

    sources
      = T.subcommand "sources" "List all the source paths (globs) for the dependencies of the project"
      $ pure Sources

    listPackages
      = T.subcommand "list-packages" "List packages available in your packages.dhall"
      $ pure ListPackages

    build
      = T.subcommand "build" "Install the dependencies and compile the current package"
      $ Build <$> sourcePaths <*> passthroughArgs

    test
      = T.subcommand "test" "Test the project with some module, default Test.Main"
      $ Test <$> mainModule <*> sourcePaths <*> passthroughArgs

    bundle
      = T.subcommand "bundle" "Bundle the project, with optional main and target path arguments"
      $ Bundle <$> mainModule <*> toTarget

    makeModule
      = T.subcommand "make-module" "Bundle a module into a CommonJS module"
      $ MakeModule <$> mainModule <*> toTarget

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
    Init force                  -> Spago.initProject force
    Install limitJobs           -> Spago.install limitJobs
    ListPackages                -> Spago.listPackages
    Sources                     -> Spago.sources
    Build paths pursArgs        -> Spago.build paths pursArgs
    Test modName paths pursArgs -> Spago.test modName paths pursArgs
    Bundle modName tPath        -> Spago.bundle WithMain modName tPath
    MakeModule modName tPath    -> Spago.makeModule modName tPath
    Version                     -> Spago.printVersion
    PscPackageLocalSetup force  -> PscPackage.localSetup force
    PscPackageInsDhall          -> PscPackage.insDhall
    PscPackageClean             -> PscPackage.clean
