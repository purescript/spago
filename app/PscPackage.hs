module PscPackage where

import           Control.Exception  (SomeException, try)
import qualified Data.Aeson         as JSON
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified Dhall.Format       as Dhall.Format
import qualified Dhall.Pretty       as Dhall.Pretty
import qualified Turtle             as T

import qualified PscPackage.Types   as PscPackage
import           Spago.Dhall        (dhallToJSON)
import qualified Spago.Templates    as Templates


pscPackageBasePathText :: Text
pscPackageBasePathText = ".psc-package/local/.set/"

pscPackageBasePath :: T.FilePath
pscPackageBasePath = T.fromText pscPackageBasePathText

packagesDhallText :: Text
packagesDhallText = "./packages.dhall"

packagesDhallPath :: T.FilePath
packagesDhallPath = T.fromText packagesDhallText

packagesJsonText :: Text
packagesJsonText = pscPackageBasePathText <> "packages.json"

packagesJsonPath :: T.FilePath
packagesJsonPath = T.fromText packagesJsonText


-- | Generates a local `packages.json` from the current `packages.dhall`
insDhall :: IO ()
insDhall = do
  isProject <- T.testfile packagesDhallPath
  T.unless isProject $
    T.die "Missing packages.dhall file. Run `spago psc-package-local-setup` first."
  T.mktree pscPackageBasePath
  T.touch packagesJsonPath

  try (dhallToJSON packagesDhallPath packagesJsonPath) >>= \case
    Right _ -> do
      T.echo $ T.unsafeTextToLine $ "Wrote packages.json to " <> packagesJsonText
      T.echo "Now you can run `psc-package install`."
    Left (err :: SomeException) ->
      T.die ("Failed to insdhall: " <> Text.pack (show err))


-- | Tries to create the `packages.dhall` file. Fails when the file already exists,
--   unless `--force` has been used.
makePackagesDhall :: Bool -> Text -> IO ()
makePackagesDhall force command = do
  T.unless force $ do
    hasPackagesDhall <- T.testfile packagesDhallPath
    T.when hasPackagesDhall $ T.die
       $ "Found " <> packagesDhallText <> ": there's already a project here. "
      <> "Run `spago " <> command <> " --force` if you're sure you want to overwrite it."
  T.touch packagesDhallPath
  T.writeTextFile packagesDhallPath Templates.packagesDhall
  Dhall.Format.format Dhall.Pretty.Unicode (Just $ Text.unpack packagesDhallText)


-- | Tries to create the `psc-package.json` file. Existing dependencies are preserved,
-- | unless `--force` has been used.
makePscPackage :: Bool -> IO ()
makePscPackage force = do
  hasPscPackage <- T.testfile pscPackageJsonPath
  if hasPscPackage && not force
    then do
      pscPackage <- T.readTextFile pscPackageJsonPath
      case JSON.eitherDecodeStrict $ Text.encodeUtf8 pscPackage of
        Left e -> T.die $ "The existing psc-package.json file is in the wrong format: " <>
          Text.pack e
        Right p -> do
          T.writeTextFile pscPackageJsonPath $
            Templates.encodePscPackage $ p { PscPackage.set = "local", PscPackage.source = "" }
          T.echo "An existing psc-package.json file was found and upgraded to use local package sets."
          T.echo $ "It's possible that some of the existing dependencies are not in the default spacchetti package set."

    else do
      T.touch pscPackageJsonPath
      pwd <- T.pwd
      let projectName = case T.toText $ T.filename pwd of
            Left _  -> "my-project"
            Right n -> n
      T.writeTextFile pscPackageJsonPath $ Templates.pscPackageJson projectName

  where
    pscPackageJsonPath = T.fromText "psc-package.json"


-- | Create `packages.dhall` and update `psc-package.json` to use the local set
localSetup :: Bool -> IO ()
localSetup force = do
  makePackagesDhall force "psc-package-local-setup"
  makePscPackage force
  T.echo "Set up local Spacchetti packages."
  T.echo "Run `spago psc-package-insdhall` to generate the package set."


-- | Delete the .psc-package folder
clean :: IO ()
clean = do
  let pscDir = "./.psc-package"
  hasDir <- T.testdir pscDir
  if hasDir
    then do
      T.rmtree pscDir
      T.echo "Packages cache was cleaned."
    else T.echo "Nothing to clean here."
