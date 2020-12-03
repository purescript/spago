module Spago.GlobalCache where

import           Spago.Prelude
import           Spago.Env

import qualified Codec.Archive.Tar      as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Control.Foldl          as Fold
import qualified Control.Retry          as Retry
import qualified Data.ByteString        as BS
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as Text
import qualified Network.HTTP.Simple    as Http
import qualified System.FilePath        as FilePath
import qualified Turtle
import           UnliftIO.Directory     (XdgDirectory(XdgCache), getXdgDirectory)



newtype CommitHash = CommitHash Text
  deriving (Ord, Eq, Show, Read, Generic, Data, ToJSON, FromJSON)

newtype Tag = Tag Text
  deriving (Ord, Eq, Show, Read, Generic, Data, ToJSONKey, FromJSONKey, FromJSON, ToJSON)

data RepoMetadataV1 = RepoMetadataV1
  { commits :: [CommitHash]
  , latest  :: Maybe Tag
  , owner   :: Text
  , tags    :: (Map Tag CommitHash)
  } deriving (Show, Generic)

instance FromJSON RepoMetadataV1
instance ToJSON RepoMetadataV1

type ReposMetadataV1 = Map PackageName RepoMetadataV1



-- | A package is "globally cacheable" if:
--   * it's a GitHub repo
--   * the ref we have is a commit or a tag -- i.e. "immutable enough", so e.g. not a branch
--
--   So here we check that one of the two is true, and if so we run the callback with the
--   URL of the .tar.gz archive on GitHub, otherwise another callback for when it's not
globallyCache
  :: HasLogFunc env 
  => (PackageName, Repo, Text)
  -> FilePath.FilePath
  -> ReposMetadataV1
  -> (FilePath.FilePath -> RIO env ())
  -> RIO env ()
  -> RIO env ()
globallyCache (packageName, Repo url, ref) downloadDir metadata cacheableCallback notCacheableCallback = do
  logDebug $ "Running `globallyCache`: " <> displayShow packageName <> " " <> display url <> " " <> display ref
  case (Text.stripPrefix "https://github.com/" url)
       >>= (Text.stripSuffix ".git")
       >>= (Just . Text.split (== '/')) of
    Just [owner, repo] -> do
      case (isTag <|> isCommit) of
        Nothing -> notCacheableCallback -- TODO: nice error?
        Just _ -> do
          let archiveUrl = "https://github.com/" <> owner <> "/" <> repo <> "/archive/" <> ref <> ".tar.gz"
          logDebug $ "About to fetch tarball for " <> display archiveUrl
          fetchTarball downloadDir archiveUrl
          Turtle.fold (Turtle.ls $ Turtle.decodeString downloadDir) Fold.head >>= \case
            Just resultDir -> do
              cacheableCallback $ Turtle.encodeString resultDir
            Nothing -> do
              die [ "Could not find the result directory when unpacking the archive " <> displayShow archiveUrl ]
      where
    _ -> do
      logDebug $ "Not caching repo because URL doesn't have the form of 'https://github.com/<ORG>/<REPO>.git': " <> display url
      notCacheableCallback -- TODO: error?
  where
    isTag = do
      RepoMetadataV1{..} <- Map.lookup packageName metadata
      void $ Map.lookup (Tag ref) tags
      return ref

    isCommit = do
      RepoMetadataV1{..} <- Map.lookup packageName metadata
      case elem (CommitHash ref) commits of
        True -> return ref
        False -> empty


-- | Download the GitHub Index cache from the `package-sets-metadata` repo
getMetadata :: (HasLogFunc env, HasGlobalCache env) => RIO env ReposMetadataV1
getMetadata = do
  logDebug "Running `getMetadata`"

  GlobalCache globalCacheDir cacheFlag <- view (the @GlobalCache)

  logDebug $ "Global cache directory: " <> displayShow globalCacheDir

  let metaURL = "https://raw.githubusercontent.com/spacchetti/package-sets-metadata/master/metadataV1.json"

      globalPathToMeta = globalCacheDir </> "metadataV1.json"

      maybeToMonoid :: Monoid a => Maybe a -> a
      maybeToMonoid m = case m of
        Nothing -> mempty
        Just a  -> a

      downloadMeta = handleAny
        (\err -> do
            logDebug $ "Metadata fetch failed with exception: " <> display err
            logWarn "Unable to download GitHub metadata, global cache will be disabled"
            pure mempty)
        (do
            metaBS <- Http.getResponseBody `fmap` Http.httpBS metaURL
            case decodeStrict' metaBS of
              Nothing -> do
                logWarn "Unable to parse GitHub metadata, global cache will be disabled"
                pure mempty
              Just meta -> do
                assertDirectory globalCacheDir
                liftIO $ BS.writeFile globalPathToMeta metaBS
                pure meta)

  case cacheFlag of
    -- If we need to skip the cache we just get an empty map
    Just SkipCache -> pure mempty
    -- If we need to download a new cache we can skip checking the local filesystem
    Just NewCache -> do
      logInfo "Downloading a new packages cache metadata from GitHub.."
      downloadMeta
    -- Otherwise we check first
    Nothing -> do
      logInfo "Searching for packages cache metadata.."

      -- Check if the metadata is in global cache and fresher than 1 day
      shouldRefreshFile globalPathToMeta >>= \case
        -- If we should not download it, read from file
        False -> do
          logInfo "Recent packages cache metadata found, using it.."
          fmap maybeToMonoid $ liftIO $ decodeFileStrict globalPathToMeta
        -- Otherwise download it, write it to file, and return it
        True -> do
          logInfo "Unable to find packages cache metadata, downloading from GitHub.."
          downloadMeta


-- | Directory in which spago will put its global cache
--   `getXdgDirectory XdgCache` tries to find the folder pointed by
--   `$XDG_CACHE_HOME`, otherwise it uses:
--   - (on Linux/MacOS) the folder pointed by `$HOME/.cache`, or
--   - (on Windows) the folder pointed by `LocalAppData`
getGlobalCacheDir :: (MonadIO m, HasLogFunc env, MonadReader env m) => m FilePath.FilePath
getGlobalCacheDir = do
  globalCache <- liftIO $ getXdgDirectory XdgCache "spago" <|> pure ".spago-global-cache"
  assertDirectory globalCache
  pure globalCache


-- | Fetch the tarball at `archiveUrl` and unpack it into `destination`
fetchTarball :: HasLogFunc env => FilePath.FilePath -> Text -> RIO env ()
fetchTarball destination archiveUrl = do
  logDebug $ "Fetching " <> display archiveUrl
  tarballUrl <- Http.parseRequest $ Text.unpack archiveUrl
  lbs <- fmap Http.getResponseBody
    -- We retry a couple of times here to avoid transient network errors
    $ Retry.recoverAll (Retry.fullJitterBackoff 100000 <> Retry.limitRetries 2)
    $ \_retryStatus -> Http.httpLBS tarballUrl
  liftIO $ Tar.unpack destination $ Tar.read $ GZip.decompress lbs
