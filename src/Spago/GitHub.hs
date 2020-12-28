{-# LANGUAGE ViewPatterns #-}
module Spago.GitHub
  ( getLatestPackageSetsTag
  , getLatestReleasesFile
  ) where

import           Spago.Prelude
import           Spago.Env

import qualified Control.Retry       as Retry
import qualified Data.List           as List
import qualified Data.Text           as Text
import qualified Data.Text.Encoding
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http
import qualified Data.Versions       as Version
import qualified Data.Map.Strict     as Map

import qualified Spago.Messages      as Messages


tagCacheFile :: Text -> Text -> Text
tagCacheFile org repo = org <> "-" <> repo <> "-tag.txt"

getLatestPackageSetsTag
  :: (HasLogFunc env, HasGlobalCache env)
  => Text -> Text -> RIO env (Either SomeException Text)
getLatestPackageSetsTag org repo = do
  GlobalCache globalCacheDir cacheFlag <- view (the @GlobalCache)
  assertDirectory globalCacheDir
  let globalPathToCachedTag = globalCacheDir </> (Text.unpack $ tagCacheFile org repo)
  let writeTagCache = writeTextFile (Text.pack globalPathToCachedTag)
  let readTagCache = try $ readTextFile $ pathFromText $ Text.pack globalPathToCachedTag
  let downloadTagToCache env = try
        $ Retry.recoverAll (Retry.fullJitterBackoff 50000 <> Retry.limitRetries 5)
        $ \_ -> runReaderT getLatestRelease env

  logDebug $ "Getting latest release for " <> display org <> "/" <> display repo

  shouldRefresh <- if isNothing cacheFlag
    then shouldRefreshFile globalPathToCachedTag
    else pure True
  when shouldRefresh $ do
    env <- ask
    liftIO (downloadTagToCache env) >>= \case
      Left (err :: SomeException) -> logDebug $ display $ Messages.failedToReachGitHub err
      Right releaseTagName -> writeTagCache releaseTagName

  readTagCache

  where
    -- | The idea here is that we go to the `latest` endpoint, and then get redirected
    --   to the latest release. So we search for the `Location` header which should contain
    --   the URL we get redirected to, and strip the release name from there (it's the
    --   last segment of the URL)
    getLatestRelease :: (HasLogFunc env, MonadReader env m, MonadIO m, MonadThrow m, Alternative m) => m Text
    getLatestRelease = do
      request <- Http.parseRequest $ "https://github.com/" <> (Text.unpack org) <> "/" <> (Text.unpack repo) <> "/releases/latest"
      response <- Http.httpBS
        $ Http.addRequestHeader "User-Agent" "Mozilla/5.0"
        $ request { Http.redirectCount = 0 }
      case Http.getResponseHeader "Location" response of
        [redirectUrl] -> return $ List.last $ Text.splitOn "/" $ Data.Text.Encoding.decodeUtf8 redirectUrl
        _ -> do
          logWarn "Error following GitHub redirect, response:"
          logWarn $ displayShow response
          empty

getLatestReleasesFile
  :: (HasLogFunc env, MonadReader env m, MonadThrow m, MonadIO m)
  => Text -> Text -> m (Map Version.SemVer Text)
getLatestReleasesFile org repo = do
  logDebug $ "Getting `latest-compatible-sets.json` from " <> display org <> "/" <> display repo
  request <- Http.parseRequest $ "https://raw.githubusercontent.com/" <> (Text.unpack org) <> "/" <> (Text.unpack repo) <> "/master/latest-compatible-sets.json"
  response <- Http.responseBody <$> Http.httpJSON request
  let parseVersion (k, v) = case Version.semver k of
        Left _ -> Nothing
        Right version -> Just (version, v)
  pure $ Map.fromList $ mapMaybe parseVersion $ Map.toList response