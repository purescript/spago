module Main (main) where

import qualified GHC.IO.Encoding
import qualified Turtle          as T

import qualified PscPackage
import qualified Spago


-- | Commands that this program handles
data Command

  -- | ### Commands for working with Spago projects
  --
  -- | Initialize a new project
  = Init Bool

  -- | Install (download) dependencies defined in spago.dhall
  | Install

  -- | Get source globs of dependencies in spago.dhall
  | Sources

  -- | Build the project paths src/ and test/
  | Build


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
      = Init <$> init'
  T.<|> Install <$ install'
  T.<|> Sources <$ sources'
  T.<|> Build <$ build'
  T.<|> PscPackageLocalSetup <$> localSetup'
  T.<|> PscPackageInsDhall <$ insDhall'
  T.<|> PscPackageClean <$ clean'
  T.<|> Version <$ version'
  where
    localSetup'
      = T.subcommand "psc-package-local-setup"
          "Setup a local package set by creating a new packages.dhall"
      $ T.switch "force" 'f' "Overwrite any project found in the current directory"

    insDhall'
      = T.subcommand "psc-package-insdhall"
          "Insdhall the local package set from packages.dhall"
      $ pure ()

    clean'
      = T.subcommand "psc-package-clean"
          "Clean cached packages by deleting the .psc-package folder"
      $ pure ()

    init'
      = T.subcommand "init"
          "Initialize a new basic project"
      $ T.switch "force" 'f' "Overwrite any project found in the current directory"

    install'
      = T.subcommand "install"
          "Install (download) all dependencies listed in spago.dhall"
      $ pure ()

    sources'
      = T.subcommand "sources"
          "List all the source paths (globs) for the dependencies of the project"
      $ pure ()

    build'
      = T.subcommand "build"
          "Install the dependencies and compile the current package"
      $ pure ()

    version'
      = T.subcommand "version"
          "Show spago version"
      $ pure ()

main :: IO ()
main = do
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
  command <- T.options "Spago - manage your PureScript projects" parser
  case command of
    Init force                 -> Spago.init force
    Install                    -> Spago.install
    Sources                    -> Spago.sources
    Build                      -> Spago.build
    Version                    -> Spago.printVersion
    PscPackageLocalSetup force -> PscPackage.localSetup force
    PscPackageInsDhall         -> PscPackage.insDhall
    PscPackageClean            -> PscPackage.clean
