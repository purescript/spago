module Spago.PscPackage where

import           Prelude

import           Control.Exception        (SomeException, try)
import qualified Data.Aeson               as JSON
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy     as ByteString.Lazy
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.Encoding  as LT
import qualified Dhall.JSON               as Dhall.JSON
import           GHC.Generics             (Generic)
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
dhallToJSON :: T.Text -> T.FilePath -> IO ()
dhallToJSON inputPath outputPath = do
  let config = JSON.Config
               { JSON.confIndent = JSON.Spaces 2
               , JSON.confCompare = compare
               , JSON.confNumFormat = JSON.Generic
               , JSON.confTrailingNewline = False }

  dhall <- T.readTextFile $ T.fromText inputPath

  json <- Dhall.JSON.codeToValue Dhall.JSON.NoConversion inputPath dhall

  T.writeTextFile outputPath
    $ Text.decodeUtf8
    $ ByteString.Lazy.toStrict
    $ JSON.encodePretty' config json


-- | Generates a local `packages.json` from the current `packages.dhall`
insDhall :: IO ()
insDhall = do
  isProject <- T.testfile PackageSet.path
  T.unless isProject $
    T.die "Missing packages.dhall file. Run `spago psc-package-local-setup` first."
  T.mktree pscPackageBasePath
  T.touch packagesJsonPath

  try (dhallToJSON PackageSet.pathText packagesJsonPath) >>= \case
    Right _ -> do
      T.echo $ T.unsafeTextToLine $ "Wrote packages.json to " <> packagesJsonText
      T.echo "Now you can run `psc-package install`."
    Left (err :: SomeException) ->
      T.die ("Failed to insdhall: " <> Text.pack (show err))


-- | Tries to create the `psc-package.json` file. Existing dependencies are preserved,
-- | unless `--force` has been used.
makePscPackage :: Bool -> IO ()
makePscPackage force = do
  hasPscPackage <- T.testfile configPath
  if hasPscPackage && not force
    then do
      pscPackage <- T.readTextFile configPath
      case JSON.eitherDecodeStrict $ Text.encodeUtf8 pscPackage of
        Left e -> T.die $ "The existing psc-package.json file is in the wrong format: " <>
          Text.pack e
        Right p -> do
          T.writeTextFile configPath $
            encodePscPackage $ p { set = "local", source = "" }
          T.echo "An existing psc-package.json file was found and upgraded to use local package sets."

    else do
      T.touch configPath
      pwd <- T.pwd
      let projectName = case T.toText $ T.filename pwd of
            Left _  -> "my-project"
            Right n -> n
      T.writeTextFile configPath $ pscPackageJson projectName


-- | Create `packages.dhall` and update `psc-package.json` to use the local set
localSetup :: Bool -> IO ()
localSetup force = do
  PackageSet.makePackageSetFile force
  makePscPackage force
  T.echo "Set up local Dhall packages."
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
