module Spago.Bower
  ( path
  , generateBowerJson
  , runBowerInstall
  ) where

import Spago.Prelude hiding (encodeUtf8)
import Spago.Env

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as Pretty
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Text                  as Text
import qualified System.Info
import qualified Turtle
import qualified Web.Bower.PackageMeta      as Bower
import qualified Data.Text.Lazy             as LazyText
import qualified Data.Text.Lazy.Encoding    as LazyEncoding
import qualified Data.Text.Encoding         as Encoding

import           Web.Bower.PackageMeta      (PackageMeta (..))

import qualified Spago.Async                as Async
import qualified Spago.Git                  as Git
import qualified Spago.Packages             as Packages
import qualified Spago.Templates            as Templates


path :: IsString t => t
path = "bower.json"


runBower :: HasBower env => [Text] -> RIO env (ExitCode, Text, Text)
runBower args = do
  BowerCmd bower <- view (the @BowerCmd)
  Turtle.procStrictWithErr bower args empty


generateBowerJson :: HasPublishEnv env => RIO env Text
generateBowerJson = do
  logInfo "Generating a new Bower config using the package set versions.."
  Config{..} <- view (the @Config)
  PublishConfig{..} <- throws publishConfig

  bowerName <- mkPackageName name
  bowerDependencies <- mkDependencies
  template <- templateBowerJson

  let bowerLicense = [publishLicense]
      bowerRepository = Just $ Bower.Repository publishRepository "git"
      bowerPkg = template { bowerLicense, bowerRepository, bowerName, bowerDependencies }
      prettyConfig = Pretty.defConfig
        { Pretty.confCompare = Pretty.keyOrder ["name", "license", "repository", "ignore", "dependencies"] <> compare
        , Pretty.confTrailingNewline = True
        }
      bowerJson = LazyText.toStrict $ LazyEncoding.decodeUtf8 $ Pretty.encodePretty' prettyConfig bowerPkg

  ignored <- Git.isIgnored path
  when ignored $ do
    die [ path <> " is being ignored by git - change this before continuing" ]

  logInfo "Generated a valid Bower config using the package set"
  pure bowerJson


runBowerInstall :: (HasLogFunc env, HasBower env) => RIO env ()
runBowerInstall = do
  logInfo "Running `bower install` so `pulp publish` can read resolved versions from it"
  BowerCmd bower <- view (the @BowerCmd)
  shell (bower <> " install --silent") empty >>= \case
    ExitSuccess   -> pure ()
    ExitFailure _ -> die [ "Failed to run `bower install` on your package" ]


templateBowerJson :: HasLogFunc env => RIO env Bower.PackageMeta
templateBowerJson = do
  case Aeson.decodeStrict (Encoding.encodeUtf8 Templates.bowerJson) of
    Just t  ->
      pure t
    Nothing ->
      die [ "Invalid bower.json template (this is a Spago bug)" ]


mkPackageName :: HasLogFunc env => Text -> RIO env Bower.PackageName
mkPackageName spagoName = do
  let psName = "purescript-" <> spagoName
  case Bower.mkPackageName psName of
    Left err ->
      die [ display $ psName <> " is not a valid Bower package name: " <> Bower.showPackageNameError err ]
    Right name ->
      pure name


-- | If the given version exists in bower, return a shorthand bower
-- | version, otherwise return a URL#version style bower version.
mkBowerVersion
  :: (HasLogFunc env, HasBower env)
  => Bower.PackageName -> Text -> Repo 
  -> RIO env Bower.VersionRange
mkBowerVersion packageName version (Repo repo) = do
  let args = ["info", "--json", Bower.runPackageName packageName <> "#" <> version]
  (code, out, _) <- runBower args
  -- Here `bower info` likely fails because the package is not in the Bower registry.
  -- So we just include the full repo for the package - see #682 for more info
  if (code /= ExitSuccess) then
    pure $ Bower.VersionRange $ repo <> "#" <> version
  else do
    info <- case Aeson.decode $ LazyEncoding.encodeUtf8 $ LazyText.fromStrict out of
      Just (Object obj) -> pure obj
      _ -> die [ display $ "Unable to decode output from `bower " <> Text.intercalate " " args <> "`: ", display out ]

    if HashMap.member "version" info
      then pure $ Bower.VersionRange $ "^" <> version
      else pure $ Bower.VersionRange $ repo <> "#" <> version


mkDependencies
  :: forall env. HasPublishEnv env => RIO env [(Bower.PackageName, Bower.VersionRange)]
mkDependencies = do
  deps <- Packages.getTransitiveTargetDeps

  Jobs jobs <- getJobs

  Async.withTaskGroup jobs $ \taskGroup ->
    Async.mapTasks taskGroup $ mkDependency <$> deps

  where
    mkDependency :: (PackageName, Package) -> RIO env (Bower.PackageName, Bower.VersionRange)
    mkDependency (PackageName{..}, Package{..}) =
      case location of
        Local localPath ->
          die [ "Unable to create Bower version for local repo: " <> display localPath ]
        Remote{..} -> do
          bowerName <- mkPackageName packageName
          bowerVersion <- mkBowerVersion bowerName version repo
          pure (bowerName, bowerVersion)

    getJobs = case System.Info.os of
      -- Windows sucks so lets make it slow for them!
      -- (just kidding, its a bug: https://github.com/bower/spec/issues/79)
      "mingw32" -> pure $ Jobs 1
      _         -> view (the @Jobs)
