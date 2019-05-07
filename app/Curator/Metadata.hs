module Curator.Metadata where

import Spago.Prelude

import qualified Control.Concurrent.Async.Pool as Async
import qualified Data.Vector as Vector
import qualified GitHub
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Spago.Dhall as Dhall
import qualified Dhall.Map
import qualified Data.Text.Encoding as Encoding
import qualified Control.Retry as Retry

import Data.Typeable (Proxy(..))
import Spago.PackageSet (PackageSet, PackageName(..), Package(..), Repo(..))

newtype CommitHash = CommitHash { unCommitHash :: Text } deriving (Show)
newtype Tag = Tag { unTag :: Text } deriving (Ord, Eq, Show)

data RepoMetadataV1 = RepoMetadataV1
  { commits :: ![CommitHash]
  , tags :: !(Map Tag CommitHash)
  } deriving (Show)

type ReposMetadataV1 = Map PackageName RepoMetadataV1


-- | Call GitHub to get metadata for a single package
fetchRepoMetadata :: Text -> (PackageName, Package) -> IO (Either Text (PackageName, RepoMetadataV1))
fetchRepoMetadata _ (_, Package{ repo = Local _, ..}) = pure $ Left "Cannot fetch local repo"
fetchRepoMetadata token (packageName, Package{ repo = Remote repoUrl, .. }) =
  Retry.recovering (Retry.fullJitterBackoff 1000)  do
  let (owner:repo:_rest)
        = Text.split (=='/')
        $ Text.replace "https://github.com/" ""
        $ Text.dropEnd 4 repoUrl
      auth = GitHub.OAuth $ Encoding.encodeUtf8 token
      ownerN = GitHub.mkName Proxy owner
      repoN = GitHub.mkName Proxy repo

  echo $ "Fetching commit metadata for '" <> owner <> "/" <> repo <> "'.."
  commitsEither <- GitHub.executeRequest auth $ GitHub.commitsForR ownerN repoN GitHub.FetchAll
  echo $ "Fetching tags metadata for '" <> owner <> "/" <> repo <> "'.."
  tagsEither <- GitHub.executeRequest auth $ GitHub.tagsForR ownerN repoN GitHub.FetchAll
  case (commitsEither, tagsEither) of
    (Right commitsVec, Right tagsVec) -> do
      let commits = Vector.toList $ fmap (CommitHash . GitHub.untagName . GitHub.commitSha) commitsVec
      let tags = Map.fromList $ Vector.toList
            $ fmap (\t ->
                      ( Tag $ GitHub.tagName t
                      , CommitHash $ GitHub.branchCommitSha $ GitHub.tagCommit t
                      )) tagsVec
      pure $ Right (packageName, RepoMetadataV1{..})
    _ -> pure $ Left "Failed to fetch"


-- | Call GitHub for all these packages and get metadata for them
fetchReposMetadata :: Text -> PackageSet -> IO ReposMetadataV1
fetchReposMetadata token packageSet = do
  let packages = Map.toList packageSet
  echoStr $ "Fetching metadata for " <> show (length packages) <> " packages"

  Async.withTaskGroup 20 $ \taskGroup -> do
    asyncs <- for packages (Async.async taskGroup . fetchRepoMetadata token)
    reposMeta <- for asyncs Async.wait
    echo "Fetched all metadata."
    pure $ Map.fromList $ catMaybes reposMeta


indexGitHub :: Text -> IO ()
indexGitHub token = do
  -- Get which one is the latest release of package-sets and download it
  echo "Getting latest package-sets release.."
  Right GitHub.Release{..} <- GitHub.executeRequest' $ GitHub.latestReleaseR "purescript" "package-sets"
  echo "Downloading and parsing package set.."
  packageSet <- fetchPackageSet releaseTagName

  mktree "./github-index"
  meta <- fetchReposMetadata token packageSet

  echoStr $ show meta
  -- TODO: write the metadata to file

  echo "Done."



-- | Tries to read in a PackageSet from GitHub
fetchPackageSet :: Text -> IO PackageSet
fetchPackageSet tag = do
  let packageTyp = Dhall.genericAuto :: Dhall.Type Package
  expr <- Dhall.inputExpr ("https://raw.githubusercontent.com/purescript/package-sets/" <> tag <> "/src/packages.dhall")
  Right packageSet <- pure $ case expr of
    Dhall.RecordLit pkgs -> (Map.mapKeys PackageName . Dhall.Map.toMap)
      <$> traverse (Dhall.coerceToType packageTyp) pkgs
    something -> Left $ Dhall.PackagesIsNotRecord something
  pure packageSet
