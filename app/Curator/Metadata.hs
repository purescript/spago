{-# LANGUAGE BangPatterns #-}
module Curator.Metadata where

import           Spago.Prelude

import qualified Control.Concurrent.Async.Pool as Async
import qualified Control.Retry                 as Retry
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Encoding
import qualified Data.Vector                   as Vector
import qualified Dhall.Map
import qualified GitHub
import qualified Spago.Dhall                   as Dhall

import           Data.Aeson.Encode.Pretty      (encodePretty)
import           Spago.GlobalCache
import           Spago.PackageSet              (Package (..), PackageName (..), PackageSet,
                                                Repo (..))


-- | Call GitHub to get metadata for a single package
fetchRepoMetadata :: Text -> (PackageName, Package) -> IO (Maybe (PackageName, RepoMetadataV1))
fetchRepoMetadata _ (_, Package{ repo = Local _, ..}) = pure Nothing
fetchRepoMetadata token (packageName, Package{ repo = Remote repoUrl, .. }) =
  Retry.recoverAll (Retry.fullJitterBackoff 10000 <> Retry.limitRetries 10) $ \Retry.RetryStatus{..} -> do
    let !(owner:repo:_rest)
          = Text.split (=='/')
          $ Text.replace "https://github.com/" ""
          $ case Text.isSuffixOf ".git" repoUrl of
              True  -> Text.dropEnd 4 repoUrl
              False -> repoUrl
        auth = GitHub.OAuth $ Encoding.encodeUtf8 token
        ownerN = GitHub.mkName Proxy owner
        repoN = GitHub.mkName Proxy repo

    echo $ "Retry " <> tshow rsIterNumber <> ": fetching commit metadata for '" <> owner <> "/" <> repo <> "'.."
    Right commitsVec <- GitHub.executeRequest auth $ GitHub.commitsForR ownerN repoN GitHub.FetchAll
    echo $ "Retry " <> tshow rsIterNumber <> ": fetching tags metadata for '" <> owner <> "/" <> repo <> "'.."
    Right tagsVec <- GitHub.executeRequest auth $ GitHub.tagsForR ownerN repoN GitHub.FetchAll
    echo $ "Retry " <> tshow rsIterNumber <> ": fetched commits and tags for '" <> owner <> "/" <> repo <> "'"
    let !commits = Vector.toList $ fmap (CommitHash . GitHub.untagName . GitHub.commitSha) commitsVec
    let !tags = Map.fromList $ Vector.toList
          $ fmap (\t ->
                     ( Tag $ GitHub.tagName t
                     , CommitHash $ GitHub.branchCommitSha $ GitHub.tagCommit t
                     )) tagsVec
    pure $ Just (packageName, RepoMetadataV1{..})


-- | Call GitHub for all these packages and get metadata for them
fetchReposMetadata :: Text -> PackageSet -> IO ReposMetadataV1
fetchReposMetadata token packageSet = do
  let packages = Map.toList packageSet
  echoStr $ "Fetching metadata for " <> show (length packages) <> " packages"

  Async.withTaskGroup 10 $ \taskGroup -> do
    asyncs <- for packages (Async.async taskGroup . fetchRepoMetadata token)
    reposMeta <- for asyncs Async.wait
    echo "Fetched all metadata."
    pure $! Map.fromList $! catMaybes reposMeta


indexGitHub :: Text -> IO ()
indexGitHub token = do
  -- Get which one is the latest release of package-sets and download it
  echo "Getting latest package-sets release.."
  Right GitHub.Release{..} <- GitHub.executeRequest' $ GitHub.latestReleaseR "purescript" "package-sets"
  echo "Downloading and parsing package set.."
  packageSet <- fetchPackageSet releaseTagName

  -- Fetch metadata for all repos from GitHub, in parallel
  meta <- fetchReposMetadata token packageSet

  -- Write the metadata to file
  echo "Writing metadata to file.."
  BSL.writeFile "metadataV1.json" $ encodePretty meta

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
