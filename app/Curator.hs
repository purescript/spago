{-# LANGUAGE BangPatterns #-}
module Curator (main) where

import           Spago.Prelude

import qualified Control.Concurrent             as Concurrent
import qualified Control.Concurrent.Async.Pool  as Async
import qualified Control.Concurrent.STM.TBQueue as Queue
import qualified Control.Concurrent.STM.TQueue  as Queue
import qualified Control.Retry                  as Retry
import qualified Data.ByteString.Lazy           as BSL
import qualified Data.List                      as List
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Encoding
import qualified Data.Vector                    as Vector
import qualified Dhall.Map
import qualified GHC.IO
import qualified GHC.IO.Encoding
import qualified GitHub
import qualified Spago.Dhall                    as Dhall
import qualified System.Environment             as Env
import qualified System.Process                 as Process
import qualified Turtle

import           Data.Aeson.Encode.Pretty       (encodePretty)
import           Data.Vector                    (Vector)
import           Spago.GlobalCache
import           Spago.PackageSet               (Package (..), PackageName (..), PackageSet,
                                                 Repo (..))

data SpagoUpdaterMessage
  = MStart

data FetcherMessage
  = MPackageSetTag !Text

data MetadataUpdaterMessage
  = MMetadata !PackageName !RepoMetadataV1
  | MEnd

data PackageSetsUpdaterMessage
  = MTags !PackageName !(Vector Tag)


-- | Main loop. Setup folders, repos, channels and threads, and then control them
main :: IO ()
main = do
  -- We always want to run in UTF8 anyways
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
  -- Stop `git` from asking for input, not gonna happen
  -- We just fail instead. Source:
  -- https://serverfault.com/questions/544156
  Env.setEnv "GIT_TERMINAL_PROMPT" "0"

  -- Prepare data folder that will contain the repos
  mktree "data"

  -- Make sure the repos are cloned and configured
  echo "Cloning and configuring repos.."
  ensureRepo "spacchetti" "spago"
  ensureRepo "spacchetti" "package-sets-metadata"
  ensureRepo "purescript" "package-sets"

  -- Read GitHub Auth Token
  token <- fmap Text.pack $ Env.getEnv "SPACCHETTIBOTTI_TOKEN"

  -- Set up comms channels
  chanFetcher            <- Queue.newTBQueueIO 10
  chanSpagoUpdater       <- Queue.newTBQueueIO 10
  chanMetadataUpdater    <- Queue.newTQueueIO
  chanPackageSetsUpdater <- Queue.newTQueueIO

  -- Start threads
  Concurrent.forkIO $ fetcher token chanFetcher chanMetadataUpdater chanPackageSetsUpdater
  Concurrent.forkIO $ spagoUpdater token chanSpagoUpdater chanFetcher
  Concurrent.forkIO $ metadataUpdater chanMetadataUpdater
  Concurrent.forkIO $ packageSetsUpdater chanPackageSetsUpdater

  {- |

  To Kickstart the whole thing we just need to ping the SpagoUpdater every 1h.
  It will:
  - fetch the latest release of package-sets and try to commit to spago if it didn't before
  - send a message to the Fetcher

  The Fetcher upon receiving a message, will:
  - start to swoop through the repos
  - send messages to both the MetadataUpdater and PackageSetsUpdater for commits and tags
  - send an End message to MetadataUpdater once done

  -}
  forever $ do
    atomically $ Queue.writeTBQueue chanSpagoUpdater MStart
    sleep _60m

  where
    _60m = 60 * 60 * 1000000

    sleep = Concurrent.threadDelay

    ensureRepo org repo = do
      isThere <- testdir $ Turtle.decodeString $ "data" </> repo
      -- clone if needed
      when (not isThere) $ do
        (code, _out, _err) <- runWithCwd "data" $ "git clone git@github.com:" <> org <> "/" <> repo <> ".git"
        case code of
          ExitSuccess -> echoStr $ "Cloned " <> org <> "/" <> repo
          _           -> die "Error while cloning repo"
      -- set the local git identity to spacchettibotti
      runWithCwd ("data/" <> repo) "git config --local user.name 'Spacchettibotti' && git config --local user.email 'spacchettibotti@ferrai.io'"


spagoUpdater :: Text -> Queue.TBQueue SpagoUpdaterMessage -> Queue.TBQueue FetcherMessage -> IO ()
spagoUpdater token controlChan fetcherChan = go Nothing
  where
    go maybeOldTag = do
      (atomically $ Queue.readTBQueue controlChan) >>= \case
        MStart -> do
          -- Get which one is the latest release of package-sets and download it
          echo "Update has been kickstarted by main thread."
          echo "Getting latest package-sets release.."
          Right GitHub.Release{..} <- GitHub.executeRequest' $ GitHub.latestReleaseR "purescript" "package-sets"

          echo $ "Latest tag fetched: " <> releaseTagName

          -- Get spago a new package set if needed.
          -- So we skip only if the oldTag is the same as the new tag
          case (maybeOldTag, releaseTagName) of
            (Just oldTag, newTag) | oldTag == newTag -> pure ()
            (_, newTag) -> do
              echo "Found newer tag. Checking if we ever opened a PR about this.."
              let auth = GitHub.OAuth $ Encoding.encodeUtf8 token
                  owner = GitHub.mkName Proxy "spacchetti"
                  repo = GitHub.mkName Proxy "spago"

              let branchName = "spacchettibotti-" <> newTag
              oldPRs <- GitHub.executeRequest auth
                $ GitHub.pullRequestsForR owner repo
                    (GitHub.optionsHead ("spacchetti:" <> branchName) <> GitHub.stateAll)
                    GitHub.FetchAll
              case oldPRs of
                Left err -> echoStr $ "Error: " <> show err
                Right prs | not $ Vector.null prs -> echo "PR has been already opened, skipping.."
                Right _ -> do
                  echo "No previous PRs found, updating package-sets version.."

                  -- Sync the repo, commit and push
                  echo "Pushing new commit (maybe)"
                  (code, out, err) <- runWithCwd "data/spago" $ List.intercalate " && "
                    [ "git checkout master"
                    , "git pull --rebase"
                    , "git checkout -B master origin/master"
                    , "cd templates"
                    , "spago package-set-upgrade"
                    , "cd .."
                    , "git checkout -B " <> Text.unpack branchName
                    , "git add templates/packages.dhall"
                    , "git commit -am 'Update package-sets tag to " <> Text.unpack newTag <> "'"
                    , "git push --set-upstream origin " <> Text.unpack branchName
                    ]

                  case code of
                    ExitSuccess -> do
                      echo "Pushed a new commit, opening PR.."
                      response <- GitHub.executeRequest auth
                        $ GitHub.createPullRequestR owner repo
                        $ GitHub.CreatePullRequest ("Update to package-sets@" <> newTag) "" branchName "master"
                      case response of
                        Right _ -> echo "Created PR 🎉"
                        Left err' -> echoStr $ "Error while creating PR: " <> show err'
                    _ -> do
                      echo "Something's off. Either there wasn't anything to push or there are errors. Output:"
                      echo out
                      echo err

          echo "Kickstarting the Fetcher.."
          atomically $ Queue.writeTBQueue fetcherChan $ MPackageSetTag releaseTagName
          go $ Just releaseTagName



fetcher :: Text -> Queue.TBQueue FetcherMessage -> Queue.TQueue MetadataUpdaterMessage -> Queue.TQueue PackageSetsUpdaterMessage -> IO b
fetcher token controlChan metadataChan psChan = forever $ do
  (atomically $ Queue.readTBQueue controlChan) >>= \case
    MPackageSetTag tag -> do
      echo "Downloading and parsing package set.."
      packageSet <- fetchPackageSet tag
      let packages = Map.toList packageSet
      echoStr $ "Fetching metadata for " <> show (length packages) <> " packages"

      -- Call GitHub for all these packages and get metadata for them
      Async.withTaskGroup 10 $ \taskGroup -> do
        asyncs <- for packages (Async.async taskGroup . fetchRepoMetadata)
        for asyncs Async.wait

      echo "Fetched all metadata."
      atomically $ Queue.writeTQueue metadataChan MEnd

  where
    -- | Call GitHub to get metadata for a single package
    fetchRepoMetadata :: (PackageName, Package) -> IO ()
    fetchRepoMetadata (_, Package{ repo = Local _, ..}) = pure ()
    fetchRepoMetadata (packageName, Package{ repo = Remote repoUrl, .. }) =
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

        echo $ "Retry " <> tshow rsIterNumber <> ": fetching tags metadata for '" <> owner <> "/" <> repo <> "'.."
        Right tagsVec <- GitHub.executeRequest auth $ GitHub.tagsForR ownerN repoN GitHub.FetchAll
        -- Here we immediately send the tags to the PackageSets updater
        atomically $ Queue.writeTQueue psChan $ MTags packageName $ fmap (Tag . GitHub.tagName) tagsVec

        echo $ "Retry " <> tshow rsIterNumber <> ": fetching commit metadata for '" <> owner <> "/" <> repo <> "'.."
        Right commitsVec <- GitHub.executeRequest auth $ GitHub.commitsForR ownerN repoN GitHub.FetchAll

        echo $ "Retry " <> tshow rsIterNumber <> ": fetched commits and tags for '" <> owner <> "/" <> repo <> "'"
        let !commits = Vector.toList $ fmap (CommitHash . GitHub.untagName . GitHub.commitSha) commitsVec
        let !tags = Map.fromList $ Vector.toList
              $ fmap (\t ->
                        ( Tag $ GitHub.tagName t
                        , CommitHash $ GitHub.branchCommitSha $ GitHub.tagCommit t
                        )) tagsVec
        atomically $ Queue.writeTQueue metadataChan $ MMetadata packageName RepoMetadataV1{..}


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



packageSetsUpdater :: Queue.TQueue PackageSetsUpdaterMessage -> IO b
packageSetsUpdater dataChan = forever $ do
  (atomically $ Queue.readTQueue dataChan) >>= \case
    MTags packageName tags -> do
      -- TODO:
      -- - check with the list of tags we have (TODO: save it?)
      -- - then get the latest one
      -- - then if it's different we should save the new tags
      -- - commit and PR
      -- - save that the PR is up somehow
      echoStr $ "Tags for '" <> show packageName <> "': " <> show tags


metadataUpdater :: Queue.TQueue MetadataUpdaterMessage -> IO ()
metadataUpdater dataChan = go mempty
  where
    go :: ReposMetadataV1 -> IO ()
    go state = do
      (atomically $ Queue.readTQueue dataChan) >>= \case
        MMetadata packageName meta -> do
          go $ Map.insert packageName meta state
        MEnd -> do
          -- Write the metadata to file
          echo "Writing metadata to file.."
          BSL.writeFile "data/package-sets-metadata/metadataV1.json" $ encodePretty state
          echo "Done."

          -- Sync the repo, commit and push
          echo "Pushing new commit (maybe)"
          (code, out, err) <- runWithCwd "data/package-sets-metadata" $ List.intercalate " && "
            [ "git checkout master"
            , "git pull --rebase"
            , "git checkout -B master origin/master"
            , "git add metadataV1.json"
            , "git commit -m 'Update GitHub index file'"
            , "git push --set-upstream origin master"
            ]

          case code of
            ExitSuccess -> echo "Pushed a new commit!"
            _ -> do
              echo "Something's off. Either there wasn't anything to push or there are errors. Output:"
              echo out
              echo err

          go state


runWithCwd :: MonadIO io => GHC.IO.FilePath -> String -> io (ExitCode, Text, Text)
runWithCwd cwd cmd = do
  let processWithNewCwd = (Process.shell cmd)
                    { Process.cwd = Just cwd }

  systemStrictWithErr processWithNewCwd empty
