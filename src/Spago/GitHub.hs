{-# LANGUAGE ViewPatterns #-}
module Spago.GitHub where

import           Spago.Prelude

import qualified Control.Retry       as Retry
import qualified Data.List           as List
import qualified Data.Text           as Text
import qualified Data.Text.Encoding
import qualified GitHub
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Simple as Http
import qualified System.Environment

import qualified Spago.GlobalCache   as GlobalCache
import qualified Spago.Messages      as Messages


tagCacheFile, tokenCacheFile :: IsString t => t
tagCacheFile = "package-sets-tag.txt"
tokenCacheFile = "github-token.txt"


login :: HasEnv env => RIO env ()
login = do
 maybeToken <- liftIO (System.Environment.lookupEnv githubTokenEnvVar)
 globalCacheDir <- view globalCacheL

 case maybeToken of
   Nothing -> die [ display Messages.getNewGitHubToken ]
   Just (Text.pack -> token) -> do
     logInfo "Token read, authenticating with GitHub.."
     username <- getUsername token
     logInfo $ "Successfully authenticated as " <> displayShow username
     writeTextFile (Text.pack $ globalCacheDir </> tokenCacheFile) token
  where
    getUsername token = do
      result <- liftIO $ GitHub.executeRequest
        (GitHub.OAuth $ Data.Text.Encoding.encodeUtf8 token)
        GitHub.userInfoCurrentR
      case result of
        Left err              -> die [ display $ Messages.failedToReachGitHub err ]
        Right GitHub.User{..} -> pure $ GitHub.untagName userLogin


readToken :: (MonadReader env m, MonadIO m, Alternative m, HasLogFunc env, MonadThrow m) => m Text
readToken = readFromEnv <|> readFromFile
  where
    readFromEnv = liftIO (System.Environment.lookupEnv githubTokenEnvVar) >>= \case
      Nothing -> empty
      Just (Text.pack -> token) -> return token

    readFromFile = do
      globalCache <- GlobalCache.getGlobalCacheDir
      assertDirectory globalCache
      readTextFile $ pathFromText $ Text.pack $ globalCache </> tokenCacheFile


getLatestPackageSetsTag :: Spago (Either SomeException Text)
getLatestPackageSetsTag = do
  globalCacheDir <- view globalCacheL
  assertDirectory globalCacheDir
  let globalPathToCachedTag = globalCacheDir </> tagCacheFile
  let writeTagCache = writeTextFile (Text.pack globalPathToCachedTag)
  let readTagCache = try $ readTextFile $ pathFromText $ Text.pack globalPathToCachedTag
  let downloadTagToCache env = try
        $ Retry.recoverAll (Retry.fullJitterBackoff 50000 <> Retry.limitRetries 5)
        $ \_ -> runReaderT (getLatestRelease1 <|> getLatestRelease2) env

  whenM (shouldRefreshFile globalPathToCachedTag) $ do
    env <- ask
    liftIO (downloadTagToCache env) >>= \case
      Left (err :: SomeException) -> logDebug $ display $ Messages.failedToReachGitHub err
      Right releaseTagName -> writeTagCache releaseTagName

  readTagCache

  where
    getLatestRelease1 :: (HasLogFunc env, MonadReader env m, MonadIO m, Alternative m, MonadUnliftIO m, MonadThrow m) => m Text
    getLatestRelease1 = do
      maybeToken :: Either SomeException Text <- try readToken
      f <- case hush maybeToken of
        Nothing -> pure GitHub.executeRequest'
        Just token -> do
          logDebug "Using cached GitHub token for getting the latest release.."
          pure $ GitHub.executeRequest (GitHub.OAuth $ Data.Text.Encoding.encodeUtf8 token)
      result <- liftIO $ f $ GitHub.latestReleaseR "purescript" "package-sets"

      case result of
        Right GitHub.Release{..} -> return releaseTagName
        Left err -> do
          logWarn $ display $ Messages.failedToReachGitHub err
          empty

    -- | The idea here is that we go to the `latest` endpoint, and then get redirected
    --   to the latest release. So we search for the `Location` header which should contain
    --   the URL we get redirected to, and strip the release name from there (it's the
    --   last segment of the URL)
    getLatestRelease2 :: (HasLogFunc env, MonadReader env m, MonadIO m, MonadThrow m, Alternative m) => m Text
    getLatestRelease2 = do
      request <- Http.parseRequest "https://github.com/purescript/package-sets/releases/latest"
      response <- Http.httpBS
        $ Http.addRequestHeader "User-Agent" "Mozilla/5.0"
        $ request { Http.redirectCount = 0 }
      case Http.getResponseHeader "Location" response of
        [redirectUrl] -> return $ List.last $ Text.splitOn "/" $ Data.Text.Encoding.decodeUtf8 redirectUrl
        _ -> do
          logWarn "Error following GitHub redirect, response:"
          logWarn $ displayShow response
          empty
