module Spago.PscPackage where

import           Spago.Prelude

import qualified Data.Aeson               as JSON
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy     as ByteString.Lazy
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT
import qualified Dhall.JSON               as Dhall.JSON
import           System.Directory         (removePathForcibly)
import qualified Turtle                   as T

import qualified Spago.PackageSet         as PackageSet


data PscPackage = PscPackage
  { name    :: Text
  , set     :: Text
  , source  :: Text
  , depends :: [Text]
  }
  deriving (Show, Generic)

instance JSON.ToJSON PscPackage
instance JSON.FromJSON PscPackage

configPathText :: Text
configPathText = "psc-package.json"

configPath :: T.FilePath
configPath = T.fromText configPathText

pscPackageBasePathText :: Text
pscPackageBasePathText = ".psc-package/local/.set/"

pscPackageBasePath :: T.FilePath
pscPackageBasePath = T.fromText pscPackageBasePathText

packagesJsonText :: Text
packagesJsonText = pscPackageBasePathText <> "packages.json"

packagesJsonPath :: T.FilePath
packagesJsonPath = T.fromText packagesJsonText

pscPackageJson :: T.Text -> T.Text
pscPackageJson packageName = encodePscPackage $ PscPackage packageName "local" "" []

encodePscPackage :: PscPackage -> T.Text
encodePscPackage = LT.toStrict . LT.decodeUtf8 . encodePretty


-- | Given a path to a Dhall file and an output path to a JSON file,
--   reads the Dhall, converts it, and writes it as JSON
dhallToJSON :: Spago m => T.Text -> T.FilePath -> m ()
dhallToJSON inputPath outputPath = do
  let config = JSON.Config
               { JSON.confIndent = JSON.Spaces 2
               , JSON.confCompare = compare
               , JSON.confNumFormat = JSON.Generic
               , JSON.confTrailingNewline = False }

  dhall <- readTextFile $ T.fromText inputPath

  jsonVal <- liftIO $ Dhall.JSON.codeToValue Dhall.JSON.NoConversion inputPath dhall

  writeTextFile outputPath
    $ Text.decodeUtf8
    $ ByteString.Lazy.toStrict
    $ JSON.encodePretty' config jsonVal


-- | Generates a local `packages.json` from the current `packages.dhall`
insDhall :: Spago m => m ()
insDhall = do
  isProject <- testfile PackageSet.path
  unless isProject $
    die "Missing packages.dhall file. Run `spago psc-package-local-setup` first."
  mktree pscPackageBasePath
  T.touch packagesJsonPath

  PackageSet.ensureFrozen

  try (dhallToJSON PackageSet.pathText packagesJsonPath) >>= \case
    Right _ -> do
      echo $ "Wrote packages.json to " <> packagesJsonText
      echo "Now you can run `psc-package install`."
    Left (err :: SomeException) ->
      die $ "Failed to insdhall: " <> tshow err


-- | Tries to create the `psc-package.json` file. Existing dependencies are preserved,
-- | unless `--force` has been used.
makePscPackage :: Spago m => Bool -> m ()
makePscPackage force = do
  hasPscPackage <- testfile configPath
  if hasPscPackage && not force
    then do
      pscPackage <- readTextFile configPath
      case JSON.eitherDecodeStrict $ Text.encodeUtf8 pscPackage of
        Left e -> die $ "The existing psc-package.json file is in the wrong format: " <>
          Text.pack e
        Right p -> do
          writeTextFile configPath $
            encodePscPackage $ p { set = "local", source = "" }
          echo "An existing psc-package.json file was found and upgraded to use local package sets."

    else do
      T.touch configPath
      pwd <- T.pwd
      let projectName = case T.toText $ T.filename pwd of
            Left _  -> "my-project"
            Right n -> n
      writeTextFile configPath $ pscPackageJson projectName


-- | Create `packages.dhall` and update `psc-package.json` to use the local set
localSetup :: Spago m => Bool -> m ()
localSetup force = do
  PackageSet.makePackageSetFile force
  makePscPackage force
  echo "Set up local Dhall packages."
  echo "Run `spago psc-package-insdhall` to generate the package set."


-- | Delete the .psc-package folder
clean :: Spago m => m ()
clean = do
  let pscDir = "./.psc-package"
  hasDir <- testdir pscDir
  if hasDir
    then do
      liftIO $ removePathForcibly $ T.encodeString pscDir
      echo "Packages cache was cleaned."
    else echo "Nothing to clean here."
