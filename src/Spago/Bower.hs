module Spago.Bower
  ( path
  , generateBowerJson
  , readBowerfile
  , runBowerInstall
  , runBowerInfo
  , runBowerRegister
  ) where

import           Spago.Prelude

import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy     as ByteString
import qualified Data.HashMap.Strict      as HashMap
import           Data.String              (IsString)
import qualified Data.Text                as Text
import           Distribution.System      (OS (..))
import qualified Distribution.System      as System
import qualified Turtle
import           Web.Bower.PackageMeta    (PackageMeta (..))
import qualified Web.Bower.PackageMeta    as Bower

import qualified Spago.Git                as Git
import qualified Spago.Messages           as Messages
import qualified Spago.Packages           as Packages
import qualified Spago.Templates          as Templates

import           Spago.Types


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


generateBowerJson :: Spago m => Config -> m ByteString.ByteString
generateBowerJson config@Config{..} = do
  echo $ "Generating a new Bower config using the package set versions.."
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

  echo $ "Generated a valid Bower config using the package set"
  pure bowerJson


readBowerfile :: Spago m => m Bower.PackageMeta
readBowerfile = do
  content <- readTextFile path
  case eitherDecodeStrict $ encodeUtf8 content of
    Left err          -> die $ Messages.failedToParseFile path err
    Right packageMeta -> pure packageMeta


runBowerInstall :: Spago m => m ()
runBowerInstall = do
  echo "Running `bower install` so `pulp publish` can read resolved versions from it"
  shell "bower install --silent" empty >>= \case
    ExitSuccess   -> pure ()
    ExitFailure _ -> die "Failed to run `bower install` on your package"


runBowerInfo :: Spago m => Bower.PackageName -> Maybe Text -> m (HashMap.HashMap Text Value)
runBowerInfo packageName version = do

  let args =
        [ "info"
        , "--json"
        , Bower.runPackageName packageName <> case version of
            Nothing -> ""
            Just v  -> "#" <> v
        ]
  (code, stdout, stderr) <- runBower args

  when (code /= ExitSuccess) $ do
    die $ "Failed to run: `bower " <> Text.intercalate " " args <> "`\n" <> stderr

  case Aeson.decodeStrict $ encodeUtf8 $  stdout of
    Just (Object obj) -> pure obj
    _ -> die $ "Unable to decode output from `bower " <> Text.intercalate " " args <> "`: " <> stdout


runBowerRegister :: Spago m => Bower.PackageName -> Text -> m ()
runBowerRegister packageName repoUrl = do
  (_code, stdout, _stderr) <- runBower ["register", Bower.runPackageName packageName, repoUrl]
  echo stdout


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

  info <- runBowerInfo packageName $ Just version

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
