{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Version (showVersion)
import qualified Paths_spacchetti_cli as Pcli
import qualified Templates
import qualified Turtle as T
import Data.Aeson

-- | Commands that this program handles
data Command
  -- | Do the boilerplate of the local project setup to override and add arbitrary packages
  -- | See the Spacchetti docs about this here: https://spacchetti.readthedocs.io/en/latest/local-setup.html
  = LocalSetup Bool
  -- | Do the Ins-Dhall-ation of the local project setup, equivalent to:
  -- | ```sh
  -- | NAME='local'
  -- | TARGET=.psc-package/$NAME/.set/packages.json
  -- | mkdir -p .psc-package/$NAME/.set
  -- | dhall-to-json --pretty <<< './packages.dhall' > $TARGET
  -- | echo wrote packages.json to $TARGET
  -- | ```
  | InsDhall
  -- | Deletes the .psc-package folder
  | Clean
  -- | Show spacchetti-cli version
  | Version

insDhall :: IO ()
insDhall = do
  isProject <- T.testfile "./packages.dhall"
  T.unless isProject $
    T.die "Missing packages.dhall file. Run `spacchetti local-setup` first."
  T.mktree (T.fromText basePath)
  T.touch (T.fromText packagesJson)
  code <- T.shell ("cat ./packages.dhall | dhall-to-json --pretty > " <> packagesJson) T.empty

  case code of
    T.ExitSuccess -> do
      T.echo $ T.unsafeTextToLine $
        "Wrote packages.json to " <> packagesJson
      T.echo "Now you can run `psc-package install`."
    T.ExitFailure n ->
      T.die ("failed to insdhall: " <> T.repr n)

  where
    basePath = ".psc-package/local/.set/"
    packagesJson = basePath <> "packages.json"

unsafePathToText :: T.FilePath -> T.Text
unsafePathToText p = case T.toText p of
  Left t -> t
  Right t -> t

localSetup :: Bool -> IO ()
localSetup force = do
  -- packages.dhall file
  T.unless force $ do
    hasDhall <- T.testfile packagesDhallPath
    T.when hasDhall $
      T.die $ "Found " <> unsafePathToText packagesDhallPath <> ": there's already a project here. "
           <> "Run `spacchetti local-setup --force` if you're sure you want to overwrite it."
  T.touch packagesDhallPath
  T.writeTextFile packagesDhallPath Templates.packagesDhall

  -- psc-package.json file
  hasPscPackage <- T.testfile pscPackageJsonPath
  if hasPscPackage && not force
    then do
      pscPackage <- T.readTextFile pscPackageJsonPath
      case eitherDecodeStrict $ Text.encodeUtf8 pscPackage of
        Left e -> T.die $ "The existing psc-package.json file is in the wrong format: " <>
          Text.pack e
        Right p -> do
          T.writeTextFile pscPackageJsonPath $
            Templates.encodePscPackage $ p { Templates.set = "local", Templates.source = "" }
          T.echo "An existing psc-package.json file was found and upgraded to spacchetti."
          T.echo $ "It's possible that some of the existing dependencies are not in the default " <>
                   "spacchetti package set."
          T.echo ""

    else do
      T.touch pscPackageJsonPath
      pwd <- T.pwd
      let projectName = case T.toText $ T.filename pwd of
            Left _ -> "my-project"
            Right n -> n
      T.writeTextFile pscPackageJsonPath $ Templates.pscPackageJson projectName


  _ <- T.shell ("dhall format --inplace " <> packagesDhallText) T.empty

  T.echo "Set up local Spacchetti packages. Run `spacchetti insdhall` to generate the package set."

  where
    packagesDhallText = "packages.dhall"
    pscPackageJsonPath = T.fromText "psc-package.json"
    packagesDhallPath = T.fromText packagesDhallText

printVersion :: IO ()
printVersion =
  T.echo $ T.unsafeTextToLine (Text.pack $ showVersion Pcli.version)

clean :: IO ()
clean = do
  let pscDir = "./.psc-package"
  hasDir <- T.testdir pscDir
  if hasDir
    then do
      T.rmtree pscDir
      T.echo "Packages cache was cleaned."
    else T.echo "Nothing to clean here."

parser :: T.Parser Command
parser
      = LocalSetup <$> localSetup
  T.<|> InsDhall <$ insDhall
  T.<|> Clean <$ clean
  T.<|> Version <$ version
  where
    localSetup =
      T.subcommand
      "local-setup" "run project-local Spacchetti setup" $
      T.switch "force" 'f' "Overwrite any project found in the current directory."
    insDhall = T.subcommand "insdhall" "insdhall the local package set" $ pure ()
    clean = T.subcommand "clean" "Clean cached packages by deleting the .psc-package folder" $ pure ()
    version = T.subcommand "version" "Show spacchetti-cli version" $ pure ()

main :: IO ()
main = do
  command <- T.options "Spacchetti CLI" parser
  case command of
    LocalSetup force -> localSetup force
    InsDhall         -> insDhall
    Clean            -> clean
    Version          -> printVersion
