module Spago.Bower
  ( bowerPath
  , writeBowerJson
  , runBowerInstall
  ) where

import Spago.Prelude

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as Pretty
import qualified Data.ByteString.Lazy       as ByteString
import qualified Data.HashMap.Strict        as HashMap
import           Data.String                (IsString)
import qualified Data.Text                  as Text
import           Data.Text.Lazy             (fromStrict)
import           Data.Text.Lazy.Encoding    (encodeUtf8)
import qualified Distribution.System        as System
import           Distribution.System        (OS (..))
import qualified Turtle
import           Web.Bower.PackageMeta      (PackageMeta (..))
import qualified Web.Bower.PackageMeta      as Bower

import           Spago.Config               (Config (..), PublishConfig (..))
import qualified Spago.Config               as Config
import qualified Spago.Git                  as Git
import qualified Spago.Packages             as Packages
import           Spago.PackageSet           (PackageName (..), Package (..), Repo (..))
import qualified Spago.Templates            as Templates


bowerPath :: IsString t => t
bowerPath = "bower.json"


runBower :: Spago m => [Text] -> m (ExitCode, Text, Text)
runBower args = do
  -- workaround windows issue: https://github.com/haskell/process/issues/140
  cmd <- case System.buildOS of
    Windows -> do
      let bowers = Turtle.inproc "where" ["bower.cmd"] empty
      Turtle.lineToText <$> Turtle.single (Turtle.limit 1 bowers)
    _ ->
      pure "bower"
  Turtle.procStrictWithErr cmd args empty


writeBowerJson :: Spago m => m ()
writeBowerJson = do
  echo $ "Generating " <> bowerPath <> " using the package set versions.."
  config@Config{..} <- Config.ensureConfig
  PublishConfig{..} <- Config.ensurePublishConfig

  bowerName <- mkPackageName name
  bowerDependencies <- mkDependencies config
  template <- templateBowerJson

  let bowerLicense = [license]
      bowerRepository = Just $ Bower.Repository repository "git"
      bowerPkg = template { bowerLicense, bowerRepository, bowerName, bowerDependencies }
      prettyConfig = Pretty.defConfig
        { Pretty.confCompare = Pretty.keyOrder ["name", "license", "repository", "ignore", "dependencies"] <> compare
        , Pretty.confTrailingNewline = True
        }
      bowerJson = Pretty.encodePretty' prettyConfig bowerPkg

  ignored <- Git.isIgnored bowerPath
  when ignored $ do
    die $ bowerPath <> " is being ignored by git - change this before continuing"

  liftIO $ ByteString.writeFile bowerPath bowerJson
  echo $ "Generated " <> bowerPath <> " using the package set"


runBowerInstall :: Spago m => m ()
runBowerInstall = do
  echo "Running `bower install` so `pulp publish` can read resolved versions from it"
  shell "bower install --silent" empty >>= \case
    ExitSuccess   -> pure ()
    ExitFailure _ -> die "Failed to run `bower install` on your package"


templateBowerJson :: Spago m => m Bower.PackageMeta
templateBowerJson = do
  case Aeson.decodeStrict Templates.bowerJson of
    Just t  ->
      pure t
    Nothing ->
      die "Invalid bower.json template (this is a Spago bug)"


mkPackageName :: Spago m => Text -> m Bower.PackageName
mkPackageName spagoName = do
  let psName = "purescript-" <> spagoName
  case Bower.mkPackageName psName of
    Left err ->
      die $ psName <> " is not a valid Bower package name: " <> Bower.showPackageNameError err
    Right name ->
      pure name


-- | If the given version exists in bower, return a shorthand bower
-- | version, otherwise return a URL#version style bower version.
mkBowerVersion :: Spago m => Bower.PackageName -> Text -> Text -> m Bower.VersionRange
mkBowerVersion packageName version repo = do

  let args = ["info", "--json", Bower.runPackageName packageName <> "#" <> version]
  (code, stdout, stderr) <- runBower args

  when (code /= ExitSuccess) $ do
    die $ "Failed to run: `bower " <> Text.intercalate " " args <> "`\n" <> stderr

  info <- case Aeson.decode $ encodeUtf8 $ fromStrict stdout of
    Just (Object obj) -> pure obj
    _ -> die $ "Unable to decode output from `bower " <> Text.intercalate " " args <> "`: " <> stdout

  if HashMap.member "version" info
    then pure $ Bower.VersionRange $ "^" <> version
    else pure $ Bower.VersionRange $ repo <> "#" <> version


mkDependencies :: Spago m => Config -> m [(Bower.PackageName, Bower.VersionRange)]
mkDependencies config = do
  deps <- Packages.getDirectDeps config

  jobs <- getJobs

  withTaskGroup' jobs $ \taskGroup ->
    mapTasks' taskGroup $ mkDependency <$> deps

  where
    mkDependency :: Spago m => (PackageName, Package) -> m (Bower.PackageName, Bower.VersionRange)
    mkDependency (PackageName{..}, Package{..}) =
      case repo of
        Local path ->
          die $ "Unable to create Bower version for local repo: " <> path
        Remote path -> do
          bowerName <- mkPackageName packageName
          bowerVersion <- mkBowerVersion bowerName version path
          pure (bowerName, bowerVersion)

    getJobs = case System.buildOS of
      -- Windows sucks so lets make it slow for them!
      -- (just kidding, its a bug: https://github.com/bower/spec/issues/79)
      Windows -> pure 1
      _       -> asks globalJobs
