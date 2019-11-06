module Spago.Bower
  ( path
  , generateBowerJson
  , runBowerInstall
  ) where

import Spago.Prelude hiding (encodeUtf8)

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
import qualified Spago.Templates            as Templates

import Spago.Types


path :: IsString t => t
path = "bower.json"


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


generateBowerJson :: Spago m => m ByteString.ByteString
generateBowerJson = do
  output "Generating a new Bower config using the package set versions.."
  config@Config{..} <- Config.ensureConfig
  PublishConfig{..} <- throws publishConfig

  bowerName <- mkPackageName name
  bowerDependencies <- mkDependencies config
  template <- templateBowerJson

  let bowerLicense = [publishLicense]
      bowerRepository = Just $ Bower.Repository publishRepository "git"
      bowerPkg = template { bowerLicense, bowerRepository, bowerName, bowerDependencies }
      prettyConfig = Pretty.defConfig
        { Pretty.confCompare = Pretty.keyOrder ["name", "license", "repository", "ignore", "dependencies"] <> compare
        , Pretty.confTrailingNewline = True
        }
      bowerJson = Pretty.encodePretty' prettyConfig bowerPkg

  ignored <- Git.isIgnored path
  when ignored $ do
    die $ path <> " is being ignored by git - change this before continuing"

  output "Generated a valid Bower config using the package set"
  pure bowerJson


runBowerInstall :: Spago m => m ()
runBowerInstall = do
  output "Running `bower install` so `pulp publish` can read resolved versions from it"
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
mkBowerVersion :: Spago m => Bower.PackageName -> Text -> Repo -> m Bower.VersionRange
mkBowerVersion packageName version (Repo repo) = do

  let args = ["info", "--json", Bower.runPackageName packageName <> "#" <> version]
  (code, out, err) <- runBower args

  when (code /= ExitSuccess) $ do
    die $ "Failed to run: `bower " <> Text.intercalate " " args <> "`\n" <> err

  info <- case Aeson.decode $ encodeUtf8 $ fromStrict out of
    Just (Object obj) -> pure obj
    _ -> die $ "Unable to decode output from `bower " <> Text.intercalate " " args <> "`: " <> out

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
      case location of
        Local localPath ->
          die $ "Unable to create Bower version for local repo: " <> localPath
        Remote{..} -> do
          bowerName <- mkPackageName packageName
          bowerVersion <- mkBowerVersion bowerName version repo
          pure (bowerName, bowerVersion)

    getJobs = case System.buildOS of
      -- Windows sucks so lets make it slow for them!
      -- (just kidding, its a bug: https://github.com/bower/spec/issues/79)
      Windows -> pure 1
      _       -> asks globalJobs
