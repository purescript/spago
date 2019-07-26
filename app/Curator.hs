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
import qualified Data.Set                       as Set
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Encoding
import qualified Data.Time                      as Time
import qualified Data.Vector                    as Vector
import qualified Dhall.Core
import qualified Dhall.Map
import qualified GHC.IO
import qualified GHC.IO.Encoding
import qualified GitHub
import qualified Spago.Dhall                    as Dhall
import qualified System.Environment             as Env
import qualified System.IO.Temp                 as Temp
import qualified System.Process                 as Process
import qualified Turtle

import           Data.Aeson.Encode.Pretty       (encodePretty)
import           Spago.GlobalCache
import           Spago.PackageSet               (Package (..), PackageName (..), Repo (..))

type Expr = Dhall.DhallExpr Dhall.Import
type PackageSetMap = Map PackageName Package


data SpagoUpdaterMessage
  = MStart

data FetcherMessage
  = MPackageSetTag !Text

data MetadataUpdaterMessage
  = MMetadata !PackageName !RepoMetadataV1
  | MEnd

data PackageSetsUpdaterMessage
  = MLatestTag !PackageName !Text !Tag
  | MPackageSet !PackageSetMap


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
  spawnThread "fetcher"      $ fetcher token chanFetcher chanMetadataUpdater chanPackageSetsUpdater
  spawnThread "spagoUpdater" $ spagoUpdater token chanSpagoUpdater chanFetcher
  spawnThread "metaUpdater"  $ metadataUpdater chanMetadataUpdater
  spawnThread "setsUpdater"  $ packageSetsUpdater token chanPackageSetsUpdater

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

    spawnThread name thread = Concurrent.forkIO $ catch thread $ \(err :: SomeException) -> do
      now <- Time.getCurrentTime
      BSL.appendFile "curator-errors.log"
        $ "Current time: " <> repr now <> "\n"
        <> "Got error from thread '" <> name <> "'\n"
        <> "Exception was:\n\n"
        <> (BSL.fromStrict . Encoding.encodeUtf8 . tshow) err
        <> "\n\n\n"

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
                    , "spago upgrade-set"
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
                        Right _   -> echo "Created PR 🎉"
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
      atomically $ Queue.writeTQueue psChan $ MPackageSet packageSet
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
      Retry.recoverAll (Retry.fullJitterBackoff 50000 <> Retry.limitRetries 25) $ \Retry.RetryStatus{..} -> do
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
        -- Here we immediately send the latest tag to the PackageSets updater
        case tagsVec Vector.!? 0 of
          Nothing -> pure ()
          Just latest -> atomically $ Queue.writeTQueue psChan $ MLatestTag packageName owner $ Tag $ GitHub.tagName latest

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
    fetchPackageSet :: Text -> IO PackageSetMap
    fetchPackageSet tag = do
      let packageTyp = Dhall.genericAuto :: Dhall.Type Package
      expr <- Dhall.inputExpr ("https://raw.githubusercontent.com/purescript/package-sets/" <> tag <> "/src/packages.dhall")
      Right packageSet <- pure $ case expr of
        Dhall.RecordLit pkgs -> (Map.mapKeys PackageName . Dhall.Map.toMap)
          <$> traverse (Dhall.coerceToType packageTyp) pkgs
        something -> Left $ Dhall.PackagesIsNotRecord something
      pure packageSet


packageSetsUpdater :: Text -> Queue.TQueue PackageSetsUpdaterMessage -> IO ()
packageSetsUpdater token dataChan = go mempty mempty
  where
    updateVersion :: Monad m => PackageName -> Tag -> Expr -> m Expr
    updateVersion (PackageName packageName) (Tag tag) (Dhall.RecordLit kvs)
      | Just (Dhall.RecordLit pkgKVs) <- Dhall.Map.lookup packageName kvs
      , Just (Dhall.TextLit _) <- Dhall.Map.lookup "version" pkgKVs =
          let
            newPackageVersion = Dhall.toTextLit tag
            newPackage = Dhall.RecordLit $ Dhall.Map.insert "version" newPackageVersion pkgKVs
          in pure $ Dhall.RecordLit $ Dhall.Map.insert packageName newPackage kvs
    updateVersion _ _ other = pure other

    go packageSet banned = do
      (atomically $ Queue.readTQueue dataChan) >>= \case
        MPackageSet newSet -> do
          echo "Received new package set, updating.."
          go newSet banned
        MLatestTag packageName@(PackageName name) owner tag'@(Tag tag) -> do
          -- First we check if the latest tag is the one in the package set
          case Map.lookup packageName packageSet of
            -- We're only interested in the case in which the tag in the package set
            -- is different from the current tag.
            Just Package{ version = version, .. } | version /= tag -> do
              echo $ "Found a newer tag for '" <> name <> "': " <> tag
              let auth = GitHub.OAuth $ Encoding.encodeUtf8 token
                  owner' = GitHub.mkName Proxy "purescript"
                  repo' = GitHub.mkName Proxy "package-sets"
                  branchName = "spacchettibotti-" <> name <> "-" <> tag

              -- Check that we didn't open a PR about this before
              oldPRs <- GitHub.executeRequest auth
                $ GitHub.pullRequestsForR owner' repo'
                    (GitHub.optionsHead ("purescript:" <> branchName) <> GitHub.stateAll)
                    GitHub.FetchAll

              case (oldPRs, Set.member branchName banned) of
                (Left err, _) -> do
                  echoStr $ "Error: " <> show err
                  go packageSet banned
                (Right prs, _) | not $ Vector.null prs -> do
                  echo "PR has been already opened once, skipping.."
                  go packageSet banned
                (Right _, True) -> do
                  echo "Package has failed to verify before, skipping.."
                  go packageSet banned
                (Right _, False) -> do
                  echo "No previous PRs found, verifying the addition and eventually committing.."
                  echo $ "Branch name: " <> branchName

                  withAST ("data/package-sets/src/groups/" <> Text.toLower owner <> ".dhall")
                    $ updateVersion packageName tag'

                  newBanned <- Temp.withTempDirectory "data/package-sets" "spacchettibotti-" $ \tempDir -> do
                    echoStr $ "Tempdir: " <> tempDir

                    (code, out, err) <- runWithCwd "data/package-sets" $ List.intercalate " && "
                      [ "git checkout master"
                      , "git pull"
                      , "git checkout -B master origin/master"
                      , "cd ../../" <> tempDir
                      , "spago init"
                      , "echo '../src/packages.dhall' > packages.dhall"
                      , "spago verify-set"
                      , "cd .."
                      , "git checkout -B " <> Text.unpack branchName
                      , "make"
                      , "git add packages.json"
                      , "git add src/groups"
                      , "git commit -am 'Update " <> Text.unpack name <> " to " <> Text.unpack tag <> "'"
                      , "git push --set-upstream origin " <> Text.unpack branchName
                      , "git checkout master"
                      ]

                    case code of
                      ExitSuccess -> do
                        echo "Pushed a new commit, opening PR.."
                        let releaseLink = "https://github.com/" <> owner <> "/purescript-" <> name <> "/releases/tag/" <> tag
                            body = "The addition has been verified by running `spago verify-set` in a clean project, so this is safe to merge.\n\nLink to release: " <> releaseLink
                        response <- GitHub.executeRequest auth
                          $ GitHub.createPullRequestR owner' repo'
                          $ GitHub.CreatePullRequest ("Update " <> name <> " to " <> tag) body branchName "master"
                        case response of
                          Right _   -> echo "Created PR 🎉"
                          Left err' -> echoStr $ "Error while creating PR: " <> show err'
                        pure banned
                      _ -> do
                        echo "Something's off. Either there wasn't anything to push or there are errors. Output:"
                        echo out
                        echo err
                        echo "Reverting changes.."
                        runWithCwd "data/package-sets" "git checkout -- src/groups && git checkout master"
                        -- IMPORTANT: add the package to the banned ones so we don't reverify every time
                        pure $ Set.insert branchName banned
                  go packageSet newBanned
            _ -> go packageSet banned


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
          BSL.writeFile "data/package-sets-metadata/metadataV1new.json" $ encodePretty state
          echo "Done."

          -- Sync the repo, commit and push
          echo "Pushing new commit (maybe)"
          (code, out, err) <- runWithCwd "data/package-sets-metadata" $ List.intercalate " && "
            [ "git checkout master"
            , "git pull --rebase"
            , "git checkout -B master origin/master"
            , "mv -u metadataV1new.json metadataV1.json"
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
  let processWithNewCwd = (Process.shell cmd) { Process.cwd = Just cwd }
  systemStrictWithErr processWithNewCwd empty


withAST :: MonadIO m => Text -> (Expr -> m Expr) -> m ()
withAST path transform = do
  rawConfig <- liftIO $ Dhall.readRawExpr path
  case rawConfig of
    Nothing -> echo $ "Could not find file " <> path
    Just (header, expr) -> do
      newExpr <- transformMExpr transform expr
      echo $ "Done. Updating the \"" <> path <> "\" file.."
      writeTextFile (pathFromText path) $ Dhall.prettyWithHeader header newExpr <> "\n"
      liftIO $ Dhall.format DoFormat path
  where
    transformMExpr
      :: Monad m
      => (Dhall.Expr s Dhall.Import -> m (Dhall.Expr s Dhall.Import))
      -> Dhall.Expr s Dhall.Import
      -> m (Dhall.Expr s Dhall.Import)
    transformMExpr rules =
      transformMOf
        Dhall.subExpressions
        rules
        . Dhall.Core.denote
