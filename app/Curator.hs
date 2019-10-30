{-# LANGUAGE BangPatterns #-}

{-

# The `spago-curator` tool

The purpose of this executable is to assist in the automation of certain infrastructure
tasks that make life easier in Spago-land (both for maintainers and users).
You can think of it as a glorified Perl script.

It requires a GitHub token in the `SPACCHETTIBOTTI_TOKEN` and a configured ssh key,
authenticated to all the repos it pushes to.

All its operations are run as the `spacchettibotti` user.

Once started, every 1h will do the following things:
- check if there's a new tag out for package-sets. If yes, it opens a PR to `spago` to update it
- crawl GitHub downloading the list of all tags and commits for every repo in the set.
  These will be put in a `metadataV1.json` file, in [the `package-sets-metadata` repo][package-sets-metadata].
  This file is used so that `spago` can rely on it for information about "is this ref immutable",
  effectively enabling the possibility of a global cache.
- check if the latest tag for every package is the latest tag in the set.
  If not, it updates it locally, tries to verify the new set, and if everything is fine it
  opens a PR to the [`package-sets` repo](https://github.com/purescript/package-sets)

-}


module Curator (main) where

import           Spago.Prelude

import qualified Control.Concurrent            as Concurrent
import qualified Control.Concurrent.Async.Pool as Async
import qualified Control.Concurrent.STM.TChan  as Chan
import qualified Control.Retry                 as Retry
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Map.Merge.Strict         as Map
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Encoding
import qualified Data.Time                     as Time
import qualified Data.Vector                   as Vector
import qualified Dhall.Core
import qualified Dhall.Map
import qualified GHC.IO
import qualified GHC.IO.Encoding
import qualified GitHub
import qualified Spago.Config
import qualified Spago.Dhall                   as Dhall
import qualified System.Environment            as Env
import qualified System.IO.Temp                as Temp
import qualified System.Process                as Process

import           Data.Aeson.Encode.Pretty      (encodePretty)
import           Spago.GlobalCache             (CommitHash (..), RepoMetadataV1 (..),
                                                ReposMetadataV1, Tag (..))
import           Spago.Types


type Expr = Dhall.DhallExpr Dhall.Import
type PackageSetMap = Map PackageName Package
type GitHubAddress = (GitHub.Name GitHub.Owner, GitHub.Name GitHub.Repo)


data Message
  = RefreshState
  | NewRepoRelease !GitHubAddress !Text
  | NewPackageSet !PackageSetMap
  | NewMetadata !ReposMetadataV1


data State = State
  { latestReleases :: Map GitHubAddress Text
  , packageSet     :: PackageSetMap
  , metadata       :: ReposMetadataV1
  , banned         :: Set Text
  } deriving (Show)

emptyState :: State
emptyState = State{..}
  where
    latestReleases = mempty
    packageSet = mempty
    metadata = mempty
    banned = mempty


state :: Concurrent.MVar State
state = GHC.IO.unsafePerformIO $ Concurrent.newMVar emptyState


bus :: Chan.TChan Message
bus = GHC.IO.unsafePerformIO Chan.newBroadcastTChanIO


packageSetsRepo, purescriptRepo, docsSearchRepo, spagoRepo, metadataRepo :: GitHubAddress
packageSetsRepo = ("purescript", "package-sets")
purescriptRepo  = ("purescript", "purescript")
docsSearchRepo  = ("spacchetti", "purescript-docs-search")
metadataRepo    = ("spacchetti", "package-sets-metadata")
spagoRepo       = ("spacchetti", "spago")

main :: IO ()
main = do
  -- We always want to run in UTF8 anyways
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
  -- Stop `git` from asking for input, not gonna happen
  -- We just fail instead. Source:
  -- https://serverfault.com/questions/544156
  Env.setEnv "GIT_TERMINAL_PROMPT" "0"

  -- Read GitHub Auth Token
  token <- (GitHub.OAuth . Encoding.encodeUtf8 . Text.pack) <$> Env.getEnv "SPACCHETTIBOTTI_TOKEN"

  -- Prepare data folder that will contain the temp copies of the repos
  mktree "data"

  spawnThread "writer"                  $ persistState
  spawnThread "releaseCheckPackageSets" $ checkLatestRelease token packageSetsRepo
  spawnThread "releaseCheckPureScript"  $ checkLatestRelease token purescriptRepo
  spawnThread "releaseCheckDocsSearch"  $ checkLatestRelease token docsSearchRepo
  spawnThread "spagoUpdatePackageSets"  $ spagoUpdatePackageSets token
  spawnThread "metadataFetcher"         $ metadataFetcher token
  spawnThread "metadataUpdater"         $ metadataUpdater token
  spawnThread "packageSetsUpdater"      $ packageSetsUpdater token
  -- TODO: update purescript-metadata repo on purs release
  -- TODO: update CI version for purescript on purs release
  -- TODO: have the bot monitor comments for banned packages

  -- To kickstart the whole thing we just need to send a "heartbeat" on the bus once an hour
  -- Threads will be listening to this and act accordingly
  forever $ do
    atomically $ Chan.writeTChan bus RefreshState
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



----------------------------------
--
--  GitHub
--
----------------------------------


getLatestRelease :: GitHub.AuthMethod am => am -> GitHubAddress -> IO (Either GitHub.Error GitHub.Release)
getLatestRelease token address@(owner, repo) = do
  echo $ "Getting latest release for " <> tshow address
  GitHub.executeRequest token $ GitHub.latestReleaseR owner repo


getTags :: GitHub.AuthMethod am => am -> GitHubAddress -> IO (Either GitHub.Error (Maybe Tag, (Map Tag CommitHash)))
getTags token address@(owner, repo) = do
  echo $ "Getting tags for " <> tshow address
  res <- GitHub.executeRequest token $ GitHub.tagsForR owner repo GitHub.FetchAll
  let f vec =
        ( (Tag . GitHub.tagName) <$> vec Vector.!? 0
        , Map.fromList
          $ Vector.toList
          $ fmap (\t ->
                    ( Tag $ GitHub.tagName t
                    , CommitHash $ GitHub.branchCommitSha $ GitHub.tagCommit t
                    ))
          vec
        )
  pure (fmap f res)


getCommits :: GitHub.AuthMethod am => am -> GitHubAddress -> IO (Either GitHub.Error [CommitHash])
getCommits token address@(owner, repo) = do
  echo $ "Getting commits for " <> tshow address
  res <- GitHub.executeRequest token $ GitHub.commitsForR owner repo GitHub.FetchAll
  pure $ fmap (Vector.toList . fmap (CommitHash . GitHub.untagName . GitHub.commitSha)) res



----------------------------------
--
--  Threads
--
----------------------------------


-- | Everything that goes on the bus is persisted in the State,
--   so threads can access some decently-up-to-date info
--   (with no guarantee of atomicity though, this is only for convenience)
persistState :: IO a
persistState = liftIO $ do
  pullChan <- atomically $ Chan.dupTChan bus
  forever $ atomically (Chan.readTChan pullChan) >>= \case
    RefreshState -> pure ()
    NewRepoRelease address release -> do
      Concurrent.modifyMVar_ state
        $ \State{..} -> let newReleases = Map.insert address release latestReleases
                        in pure State{ latestReleases = newReleases , ..}
    NewMetadata newMetadata -> Concurrent.modifyMVar_ state
      $ \State{..} -> pure State{ metadata = newMetadata, ..}
    NewPackageSet newPackageSet -> Concurrent.modifyMVar_ state
      $ \State{..} -> pure State{ packageSet = newPackageSet, ..}


-- | Calls GitHub to check for new releases of a repository
--   When there's a new one and we don't have it in our state we send a message on the bus
checkLatestRelease :: GitHub.Auth -> GitHubAddress -> IO ()
checkLatestRelease token address = do
  pullChan <- atomically $ Chan.dupTChan bus
  forever $ atomically (Chan.readTChan pullChan) >>= \case
    RefreshState -> getLatestRelease token address >>= \case
      Left _ -> pure () -- TODO: error out here?
      Right GitHub.Release {..} -> do
        State{..} <- Concurrent.readMVar state
        case Map.lookup address latestReleases of
          -- We don't do anything if we have a release saved and it's the current one
          Just currentRelease | currentRelease == releaseTagName -> pure ()
          _ -> do
            echo $ "Found a new release for " <> tshow address <> ": " <> releaseTagName
            atomically $ Chan.writeTChan bus $ NewRepoRelease address releaseTagName
    _ -> pure ()


-- | Whenever there's a new release of package-sets, update it in Spago's template
spagoUpdatePackageSets :: GitHub.AuthMethod am => am -> IO b
spagoUpdatePackageSets token = do
  pullChan <- atomically $ Chan.dupTChan bus
  forever $ atomically (Chan.readTChan pullChan) >>= \case
    NewRepoRelease address newTag | address == packageSetsRepo -> do
      let prTitle = "Update to package-sets@" <> newTag
      let prBranchName = "spacchettibotti-" <> newTag
      runAndOpenPR token PullRequest{ prBody = "", prAddress = spagoRepo, ..}
        [ "cd templates"
        , "spago upgrade-set"
        , "git add packages.dhall"
        ]
    _ -> pure ()


metadataFetcher :: GitHub.AuthMethod am => am -> IO b
metadataFetcher token = do
  pullChan <- atomically $ Chan.dupTChan bus
  forever $ atomically (Chan.readTChan pullChan) >>= \case
    RefreshState -> do
      echo "Downloading and parsing package set.."
      packageSet <- fetchPackageSet
      atomically $ Chan.writeTChan bus $ NewPackageSet packageSet
      let packages = Map.toList packageSet
      echoStr $ "Fetching metadata for " <> show (length packages) <> " packages"

      -- Call GitHub for all these packages and get metadata for them
      metadata <- Async.withTaskGroup 10 $ \taskGroup -> do
        asyncs <- for packages (Async.async taskGroup . fetchRepoMetadata)
        for asyncs Async.wait

      echo "Fetched all metadata."
      atomically $ Chan.writeTChan bus $ NewMetadata $ foldMap (uncurry Map.singleton) metadata
    _ -> pure ()

  where
    fetchRepoMetadata :: MonadIO m => MonadThrow m => (PackageName, Package) -> m (PackageName, RepoMetadataV1)
    fetchRepoMetadata (_, pkg@Package{ location = Local{..}, ..}) = die $ "Tried to fetch a local package: " <> tshow pkg
    fetchRepoMetadata (packageName, Package{ location = Remote{ repo = Repo repoUrl, ..}, ..}) =
      liftIO $ Retry.recoverAll (Retry.fullJitterBackoff 50000 <> Retry.limitRetries 25) $ \Retry.RetryStatus{..} -> do
        let !(owner:repo:_rest)
              = Text.split (=='/')
              $ Text.replace "https://github.com/" ""
              $ case Text.isSuffixOf ".git" repoUrl of
                  True  -> Text.dropEnd 4 repoUrl
                  False -> repoUrl
            address = (GitHub.mkName Proxy owner, GitHub.mkName Proxy repo)

        echo $ "Retry " <> tshow rsIterNumber <> ": fetching tags and commits for " <> tshow address

        !eitherTags <- getTags token address
        !eitherCommits <- getCommits token address

        case (eitherTags, eitherCommits) of
          (Left _, _) -> die $ "Retry " <> tshow rsIterNumber <> ": failed to fetch tags"
          (_, Left _) -> die $ "Retry " <> tshow rsIterNumber <> ": failed to fetch commits"
          (Right (latest, tags), Right commits) -> do
            pure (packageName, RepoMetadataV1{..})


    -- | Tries to read in a PackageSet from GitHub, master branch
    --   (so we always get the most up to date and we don't have to wait for a release)
    fetchPackageSet :: MonadIO m => MonadThrow m => m PackageSetMap
    fetchPackageSet = do
      expr <- liftIO $ Dhall.inputExpr "https://raw.githubusercontent.com/purescript/package-sets/master/src/packages.dhall"
      case expr of
        Dhall.RecordLit pkgs -> fmap (Map.mapKeys PackageName . Dhall.Map.toMap)
          $ traverse Spago.Config.parsePackage pkgs
        something -> throwM $ Dhall.PackagesIsNotRecord something



-- | Whenever there's a new metadata set, push it to the repo
metadataUpdater :: GitHub.AuthMethod am => am -> IO b
metadataUpdater token = do
  pullChan <- atomically $ Chan.dupTChan bus
  forever $ atomically (Chan.readTChan pullChan) >>= \case
    NewMetadata metadata -> do
      -- Write the metadata to file
      echo "Writing metadata to file.."
      path <- makeAbsolute "metadataV1new.json"
      BSL.writeFile path $ encodePretty metadata
      echo "Done."

      let commitMessage = "Update GitHub index file"
      runAndPushMaster token metadataRepo commitMessage
        [ "mv -u " <> Text.pack path <> " metadataV1.json"
        , "git add metadataV1.json"
        ]
    _ -> pure ()


packageSetsUpdater :: am -> IO ()
packageSetsUpdater _token = do
  pullChan <- atomically $ Chan.dupTChan bus
  forever $ atomically (Chan.readTChan pullChan) >>= \case
    NewMetadata newMetadata -> do
      -- once we get a new set of metadata, we get all the latest releases from there,
      -- and diff this list with the current package set from the state.
      -- This gives us a list of packages that need to be updated
      let latestTags :: Map PackageName Tag = Map.mapMaybe id $ latest <$> newMetadata
      State{..} <- Concurrent.readMVar state
      let packageSetTags :: Map PackageName Tag  = Map.mapMaybe id $ filterLocalPackages <$> packageSet
      let intersectionMaybe f = Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMaybeMatched f)
      let pickLatestIfDifferent _k a b = if a == b then Nothing else Just a
      let newTags = intersectionMaybe pickLatestIfDifferent latestTags packageSetTags
      echo $ "Found " <> tshow (length newTags) <> " packages to update"
      echo $ tshow newTags
      -- let branchName = "spacchettibotti-" <> name <> "-" <> tag
      -- echo $ "Branch name: " <> branchName
{-
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
-}
    _ -> pure ()
  where
    filterLocalPackages Package{..} = case location of
      Local _    -> Nothing
      Remote{..} -> Just $ Tag version

    updateVersion :: Monad m => PackageName -> Tag -> Expr -> m Expr
    updateVersion (PackageName packageName) (Tag tag) (Dhall.RecordLit kvs)
      | Just (Dhall.RecordLit pkgKVs) <- Dhall.Map.lookup packageName kvs
      , Just (Dhall.TextLit _) <- Dhall.Map.lookup "version" pkgKVs =
          let
            newPackageVersion = Dhall.toTextLit tag
            newPackage = Dhall.RecordLit $ Dhall.Map.insert "version" newPackageVersion pkgKVs
          in pure $ Dhall.RecordLit $ Dhall.Map.insert packageName newPackage kvs
    updateVersion _ _ other = pure other



----------------------------------
--
--  Machinery
--
----------------------------------


data OpenPR = OpenPR | PushMaster
  deriving Eq

data PullRequest = PullRequest
  { prBranchName :: Text
  , prAddress    :: GitHubAddress
  , prTitle      :: Text
  , prBody       :: Text
  }

runAndOpenPR :: GitHub.AuthMethod am => am -> PullRequest -> [Text] -> IO ()
runAndOpenPR = runAndPush OpenPR

runAndPushMaster :: GitHub.AuthMethod am => am -> GitHubAddress -> Text -> [Text] -> IO ()
runAndPushMaster token prAddress prTitle = runAndPush PushMaster token PullRequest{ prBranchName = "master", prBody = "", ..}

runAndPush :: GitHub.AuthMethod am => OpenPR -> am -> PullRequest -> [Text] -> IO ()
runAndPush shouldOpenPR token PullRequest{..} commands = unlessM pullRequestExists runCommandsAndPR
  where
    (owner, repo) = prAddress

    runCommandsAndPR = do
      -- Clone the repo in a temp folder
      Temp.withTempDirectory "data" "__temp-repo" $ \path -> do
        let runInRepo = runWithCwd (path </> (Text.unpack . GitHub.untagName) repo) . (Text.intercalate " && ")
        (code, _out, _err) <- runWithCwd path $ "git clone git@github.com:" <> GitHub.untagName owner <> "/" <> GitHub.untagName repo <> ".git"
        if code /= ExitSuccess
          then echo "Error while cloning repo"
          else do
            echo $ "Cloned " <> tshow prAddress
            -- Configure the repo: set the git identity to spacchettibotti and switch to the branch
            runInRepo
              [ "git config --local user.name 'Spacchettibotti'"
              , "git config --local user.email 'spacchettibotti@ferrai.io'"
              , "git checkout -B " <> prBranchName
              ]
            -- Run the commands we wanted to run, check if everything is fine
            (code', out, err) <- runInRepo commands
            if code' /= ExitSuccess
              then do
                echo "Something's off. Output:"
                echo out
                echo err
              else do
                -- If it is fine, then check if anything actually changed/got staged
                (code'', _out, _err) <- runInRepo [ "git diff --staged --exit-code" ]
                if code'' == ExitSuccess
                  then echo "Nothing to commit, skipping.."
                  else do
                    -- If yes, then push and open PR
                    _ <- runInRepo
                         [ "git commit -am '" <> prTitle <> "'"
                         , "git push --set-upstream origin " <> prBranchName
                         ]
                    when (shouldOpenPR == OpenPR) $ do
                      echo "Pushed a new commit, opening PR.."
                      response <- GitHub.executeRequest token
                        $ GitHub.createPullRequestR owner repo
                        $ GitHub.CreatePullRequest prTitle prBody prBranchName "master"
                      case response of
                        Right _   -> echo "Created PR 🎉"
                        Left err' -> echoStr $ "Error while creating PR: " <> show err'


    pullRequestExists = do
      echo $ "Checking if we ever opened a PR " <> surroundQuote prTitle

      oldPRs <- GitHub.executeRequest token
        $ GitHub.pullRequestsForR owner repo
        (GitHub.optionsHead (GitHub.untagName owner <> ":" <> prBranchName) <> GitHub.stateAll)
        GitHub.FetchAll
      case oldPRs of
        Left err -> do
          echoStr $ "Error: " <> show err
          pure True
        Right prs | not $ Vector.null prs -> do
          echo "PR was opened, skipping.."
          pure True
        Right _ -> do
          echo "No previous PRs found, opening one.."
          pure False



runWithCwd :: MonadIO io => GHC.IO.FilePath -> Text -> io (ExitCode, Text, Text)
runWithCwd cwd cmd = do
  let processWithNewCwd = (Process.shell (Text.unpack cmd)) { Process.cwd = Just cwd }
  systemStrictWithErr processWithNewCwd empty


withAST :: MonadIO m => Text -> (Expr -> m Expr) -> m ()
withAST path transform = do
  rawConfig <- liftIO $ Dhall.readRawExpr path
  case rawConfig of
    Nothing -> echo $ "Could not find file " <> path
    Just (header, expr) -> do
      newExpr <- transformMExpr transform expr
      echo $ "Done. Updating the \"" <> path <> "\" file.."
      writeTextFile path $ Dhall.prettyWithHeader header newExpr <> "\n"
      liftIO $ Dhall.format path
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
