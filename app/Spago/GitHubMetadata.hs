module Spago.GitHubMetadata where

import           Spago.Prelude

import qualified Data.ByteString     as BS
import qualified Data.Time           as Time
import qualified Network.HTTP.Simple as Http

import           Spago.PackageSet    (PackageName (..))


newtype CommitHash = CommitHash Text
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Tag = Tag Text
  deriving (Ord, Eq, Show, Generic, ToJSONKey, FromJSONKey)

data RepoMetadataV1 = RepoMetadataV1
  {  commits :: [CommitHash]
  ,  tags    :: (Map Tag CommitHash)
  } deriving (Show, Generic)

instance FromJSON RepoMetadataV1
instance ToJSON RepoMetadataV1

type ReposMetadataV1 = Map PackageName RepoMetadataV1


getMetadata :: IO (Maybe ReposMetadataV1)
getMetadata = do
  -- check if the metadata is in global cache and younger than 1d
  shouldDownloadMeta <- try (do
      fileExists <- testfile globalPathToMeta
      lastModified <- getModificationTime globalPathToMeta
      now <- Time.getCurrentTime
      -- Note: `NomiNalDiffTime` is 1 second
      let fileIsOldEnough = Time.addUTCTime (24 * 60 * 60) lastModified < now
      pure $ fileExists && fileIsOldEnough
      ) >>= \case
    Right v -> pure v
    Left (_err :: IOException) -> pure False

  case shouldDownloadMeta of
    -- if it is, read from file
    True -> decodeFileStrict globalPathToMeta
    -- if it is not, download it, write it to file, and return it
    False -> do
      metaBS <- (Http.httpBS metaURL >>= pure . Http.getResponseBody)
      let meta = decodeStrict' metaBS
      when (isJust meta) $ do
        BS.writeFile globalPathToMeta metaBS
      pure meta
  where
    globalPathToMeta = undefined
    metaURL = "https://raw.githubusercontent.com/spacchetti/package-sets-metadata/master/metadataV1.json"


-- echo "WARNING: Unable to download GitHub metadata, global cache will be disabled"
