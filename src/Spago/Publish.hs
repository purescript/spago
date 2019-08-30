{-# LANGUAGE OverloadedLists #-}
module Spago.Publish (publish) where

import           Spago.Prelude

import qualified Codec.Compression.GZip    as GZip
import qualified Control.Foldl             as Fold
import qualified Data.ByteString.Lazy
import qualified Data.Map                  as Map
import qualified Data.Text                 as Text
import           Data.Versions             (SemVer (..))
import qualified GitHub
import qualified GitHub.Auth
import qualified Network.HTTP.Simple       as Http
import qualified Network.HTTP.Types.Status as Http
import qualified Turtle
import qualified Web.Bower.PackageMeta     as BowerTypes

import qualified Spago.Bower               as Bower
import qualified Spago.Config              as Config
import           Spago.DryRun              (DryAction (..), DryRun (..), runDryActions)
import qualified Spago.Git                 as Git
import qualified Spago.GitHub              as GitHub
import qualified Spago.Purs                as Purs

import           Spago.Types


publish :: Spago m => DryRun -> m ()
publish dryRun = do
  -- Ensure we have all the fields required for publishing in the config
  config@Config{..} <- Config.ensureConfig
  PublishConfig{..} <- throws publishConfig

  -- Compute the Bower config from the current Spago config and
  -- compare it with the current one.
  -- The configs must match and everything should be committed
  newBowerConfig <- Bower.generateBowerJson config
  currentBowerConfig <- liftIO $ Data.ByteString.Lazy.readFile Bower.path
  when (newBowerConfig /= currentBowerConfig) $ do
    liftIO $ Data.ByteString.Lazy.writeFile "bower-spago.json" newBowerConfig
    die $ "The `bower.json` generated from your Spago config doesn't match the committed one.\n"
      <> "You should commit it and cut a new release by running `spago version`\n"
      <> "The new config has been written to `bower-spago.json`"
  Git.requireCleanWorkingTree
  currentVersion <- Git.getCurrentVersion

  githubToken <- GitHub.readToken

  BowerTypes.PackageMeta{..} <- Bower.readBowerfile

  repoUrl <- case bowerRepository of
    Nothing                        -> die "Error: 'repository' key not present in bower.json"
    Just BowerTypes.Repository{..} -> pure repositoryUrl

  -- Generate the resolution file from Bower dependencies pass it to `purs publish`
  resolutionsPath <- mkResolutionsFile
  resolutions <- Purs.publish resolutionsPath

  let pursuitUrl = "https://pursuit.purescript.org/packages/"
        <> BowerTypes.runPackageName bowerName
        <> "/" <> Git.unparseVersion currentVersion

  runDryActions dryRun
    [ pushTagAction bowerName currentVersion
    , registerBowerAction bowerName repoUrl
    , uploadDocsAction githubToken resolutions
    -- TODO: upstreamAction
    ] $ do
    echo "Done 🎉"
    echo $ "You can view your package's documentation at: " <> pursuitUrl


pushTagAction :: Spago m => BowerTypes.PackageName -> SemVer -> DryAction m
pushTagAction packageName currentVersion = DryAction "push all the new tags to the remote repository" $ do
  let tag = Git.unparseVersion currentVersion
  echo $ "Publishing " <> BowerTypes.runPackageName packageName <> " at " <> tag
  Git.pushTag tag


registerBowerAction :: Spago m => BowerTypes.PackageName -> Text -> DryAction m
registerBowerAction packageName repoUrl = DryAction "register the package on the Bower registry if necessary" $ do
  -- Only attempt to register on Bower after a successful push, to avoid
  -- accidental squatting by non-package-owners.
  try (Bower.runBowerInfo packageName Nothing) >>= \case
    Left (_err :: SomeException) -> do
      echo "Registering your package on Bower.."
      Bower.runBowerRegister packageName repoUrl
    Right _manifest -> pure ()


uploadDocsAction :: Spago m => GitHub.Auth -> Text -> DryAction m
uploadDocsAction token resolutions = DryAction "upload the docs for this version on Pursuit" $ do
  echo "Uploading documentation to Pursuit..."
  let gzipped = GZip.compress $ encode resolutions

  request <- Http.parseRequest "https://pursuit.purescript.org/packages"
  response <- Http.httpBS
    $ GitHub.Auth.setAuthRequest token
    $ Http.addRequestHeader "Accept" "application/json"
    $ Http.addRequestHeader "Content-Encoding" "gzip"
    $ Http.setRequestMethod "POST"
    $ Http.setRequestBodyLBS gzipped
    $ request

  if (Http.getResponseStatus response == Http.created201)
    then pure ()
    else die $ "Expected an HTTP 201 response from Pursuit, got: "
         <> tshow (Http.getResponseBody response)


-- Format for .bower.json files written automatically by Bower inside
-- subdirectories of bower_components. This type only contains the fields
-- we care about for extracting the necessary information for passing on
-- to `purs publish`.
data InstalledBowerJson = InstalledBowerJson
  { installedName           :: Text
  , installedVersion        :: Text
  , installedResolutionType :: Text
  }

instance FromJSON InstalledBowerJson where
  parseJSON = withObject "_bower.json" $ \o -> do
    installedName <- o .: "name"
    installedVersion <- o .: "version"
    installedResolutionType <- (o .: "_resolution") >>= (.: "type")
    return InstalledBowerJson{..}


type PursResolutions = Map Text PursResolutionPackage

data PursResolutionPackage = PursResolutionPackage
  { version :: Maybe Text
  , path    :: String
  } deriving (Generic)

instance ToJSON PursResolutionPackage where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }


mkResolutionsFile :: Spago m => m Text
mkResolutionsFile = do
  echo "Creating resolutions file from Bower configurations.."
  bowerPackages :: [FilePath] <- Turtle.fold (Turtle.ls bowerPackagesPath) Fold.list
  bowerJsons <- forM bowerPackages $ \package -> do
    echoDebug $ "Getting generated resolutions for " <> tshow package
    let configPath = Turtle.encodeString package </> ".bower.json"
    eitherContent <- liftIO $ eitherDecodeFileStrict configPath
    case eitherContent of
      Right c  -> pure c
      Left err -> fail err
  let resolutions :: PursResolutions = foldMap toPursResolutions bowerJsons
  assertDirectory ".spago"
  let resolutionsPath = ".spago" </> "purs-resolutions.json"
  liftIO $ encodeFile resolutionsPath resolutions
  pure $ Text.pack resolutionsPath

  where
    bowerPackagesPath :: IsString t => t
    bowerPackagesPath = "bower_components"

    toPursResolutions :: InstalledBowerJson -> Map Text PursResolutionPackage
    toPursResolutions InstalledBowerJson{..}
      = Map.singleton installedName PursResolutionPackage{..}
      where
        version = case installedResolutionType of
          "type" -> Just installedVersion
          _      -> Nothing
        path = bowerPackagesPath </> Text.unpack installedName
