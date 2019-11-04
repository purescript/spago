{-# LANGUAGE BangPatterns #-}

{-|

# The `spago-curator` tool

The purpose of this executable is to assist in the automation of certain infrastructure
tasks that make life easier in Spago-land (both for maintainers and users).
You can think of it as a glorified Perl script.

It requires a GitHub token in the `SPACCHETTIBOTTI_TOKEN` and a configured ssh key,
authenticated to all the repos it pushes to.

All its operations are run as the `spacchettibotti` user.

-}


module Curator where

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
import qualified Data.Set as Set
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
import qualified Text.Megaparsec as Parse

import           Data.Aeson.Encode.Pretty      (encodePretty)
import           Spago.GlobalCache             (CommitHash (..), RepoMetadataV1 (..),
                                                ReposMetadataV1, Tag (..))
import           Spago.Types


type Expr = Dhall.DhallExpr Dhall.Import
type PackageSetMap = Map PackageName Package

data GitHubAddress = Address
  { owner :: GitHub.Name GitHub.Owner
  , repo  :: GitHub.Name GitHub.Repo
  } deriving (Eq, Ord)

instance Show GitHubAddress where
  show (Address owner repo) = Text.unpack
    $ "\"" <> GitHub.untagName owner <> "/" <> GitHub.untagName repo <> "\""

data Message
  = RefreshState
  | NewRepoRelease !GitHubAddress !Text
  | NewPackageSet !PackageSetMap
  | NewMetadata !ReposMetadataV1
  | NewVerification !(ExitCode, Text, Text)


data State = State
  { latestReleases :: Map GitHubAddress Text
  , packageSet     :: PackageSetMap
  , metadata       :: ReposMetadataV1
  , banned         :: Set (PackageName, Tag)
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
packageSetsRepo = Address "purescript" "package-sets"
purescriptRepo  = Address "purescript" "purescript"
docsSearchRepo  = Address "spacchetti" "purescript-docs-search"
metadataRepo    = Address "spacchetti" "package-sets-metadata"
spagoRepo       = Address "spacchetti" "spago"

main :: IO ()
main = do
  -- We always want to run in UTF8 anyways
  GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
  -- Stop `git` from asking for input, not gonna happen
  -- We just fail instead. Source:
  -- https://serverfault.com/questions/544156
  Env.setEnv "GIT_TERMINAL_PROMPT" "0"

  -- Read GitHub Auth Token
  echo "Reading GitHub token.."
  token <- (GitHub.OAuth . Encoding.encodeUtf8 . Text.pack) <$> Env.getEnv "SPACCHETTIBOTTI_TOKEN"

  -- Prepare data folder that will contain the temp copies of the repos
  echo "Creating 'data' folder"
  mktree "data"

  -- Start spawning threads
  --   General utility
  spawnThread "writer"                  $ persistState
  --   purescript repo
  spawnThread "releaseCheckPureScript"  $ checkLatestRelease token purescriptRepo
  --   purescript-docs-search repo
  spawnThread "releaseCheckDocsSearch"  $ checkLatestRelease token docsSearchRepo
  --   spago repo
  spawnThread "spagoUpdatePackageSets"  $ spagoUpdatePackageSets token
  --     TODO: update purescript-metadata repo on purs release
  --     TODO: update CI version for purescript on purs release
  --   package-sets-metadata repo
  spawnThread "metadataFetcher"         $ metadataFetcher token
  spawnThread "metadataUpdater"         $ metadataUpdater
  -- package-sets repo
  spawnThread "releaseCheckPackageSets" $ checkLatestRelease token packageSetsRepo
  spawnThread "packageSetsUpdater"      $ packageSetsUpdater token
  spawnThread "packageSetsCommenter"    $ packageSetCommenter token

  -- To kickstart the whole thing we just need to send a "heartbeat" on the bus once an hour
  -- Threads will be listening to this and act accordingly
  forever $ do
    atomically $ Chan.writeTChan bus RefreshState
    sleep _60m

  where
    _60m = 60 * 60 * 1000000

    sleep = Concurrent.threadDelay

    spawnThread name thread = do
      echo $ "Spawning thread " <> tshow name
      Concurrent.forkIO $ catch thread $ \(err :: SomeException) -> do
        now <- Time.getCurrentTime
        BSL.appendFile "curator-errors.log"
          $ "Current time: " <> repr now <> "\n"
          <> "Got error from thread '" <> name <> "'\n"
          <> "Exception was:\n\n"
          <> (BSL.fromStrict . Encoding.encodeUtf8 . tshow) err
          <> "\n\n\n"



-- * GitHub operations
--
--


getLatestRelease :: GitHub.AuthMethod am => am -> GitHubAddress -> IO (Either GitHub.Error GitHub.Release)
getLatestRelease token address@(Address owner repo) = do
  echo $ "Getting latest release for " <> tshow address
  GitHub.executeRequest token $ GitHub.latestReleaseR owner repo


getTags :: GitHub.AuthMethod am => am -> GitHubAddress -> IO (Either GitHub.Error (Maybe Tag, (Map Tag CommitHash)))
getTags token address@(Address owner repo) = do
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
getCommits token address@(Address owner repo) = do
  echo $ "Getting commits for " <> tshow address
  res <- GitHub.executeRequest token $ GitHub.commitsForR owner repo GitHub.FetchAll
  pure $ fmap (Vector.toList . fmap (CommitHash . GitHub.untagName . GitHub.commitSha)) res


getPullRequestForUser :: GitHub.AuthMethod am => am -> GitHub.Name GitHub.User -> GitHubAddress -> IO (Maybe GitHub.PullRequest)
getPullRequestForUser token user Address{..} = do
  maybePRs <- fmap hush $ GitHub.executeRequest token
    $ GitHub.pullRequestsForR owner repo GitHub.stateOpen GitHub.FetchAll
  let findPRbyUser = Vector.find
        (\GitHub.SimplePullRequest{ simplePullRequestUser = GitHub.SimpleUser{..}}
          -> simpleUserLogin == user)
  let fetchFullPR GitHub.SimplePullRequest{..} = fmap hush $ GitHub.executeRequest token $ GitHub.pullRequestR owner repo simplePullRequestNumber
  -- TODO: there must be a nice way to lift this instead of casing
  case (findPRbyUser =<< maybePRs :: Maybe GitHub.SimplePullRequest) of
    Nothing -> pure Nothing
    Just pr -> fetchFullPR pr


getCommentsOnPR :: GitHub.AuthMethod am => am -> GitHubAddress -> GitHub.IssueNumber -> IO [GitHub.IssueComment]
getCommentsOnPR token Address{..} issueNumber = do
  eitherComments <- GitHub.executeRequest token
    $ GitHub.commentsR owner repo issueNumber GitHub.FetchAll
  pure $ case eitherComments of
    Left _ -> []
    Right comments -> Vector.toList comments


updatePullRequestBody :: GitHub.AuthMethod am => am -> GitHubAddress -> GitHub.IssueNumber -> Text -> IO ()
updatePullRequestBody token Address{..} pullRequestNumber newBody = do
  void
    $ GitHub.executeRequest token
    $ GitHub.updatePullRequestR owner repo pullRequestNumber
    $ GitHub.EditPullRequest Nothing (Just newBody) Nothing Nothing Nothing



-- * Threads
--
--


-- | Everything that goes on the bus is persisted in the State,
--   so threads can access some decently-up-to-date info
--   (with no guarantee of atomicity though, this is only for convenience)
persistState :: IO a
persistState = liftIO $ do
  pullChan <- atomically $ Chan.dupTChan bus
  forever $ atomically (Chan.readTChan pullChan) >>= \case
    RefreshState -> pure ()
    NewVerification _ -> pure () -- TODO: maybe save this?
    NewRepoRelease address release -> Concurrent.modifyMVar_ state
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
      runAndOpenPR token PullRequest{ prBody = "", prAddress = spagoRepo, ..} (const $ pure ())
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
            address = Address (GitHub.mkName Proxy owner) (GitHub.mkName Proxy repo)

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
metadataUpdater :: IO b
metadataUpdater = do
  pullChan <- atomically $ Chan.dupTChan bus
  forever $ atomically (Chan.readTChan pullChan) >>= \case
    NewMetadata metadata -> do
      -- Write the metadata to file
      let writeMetadata :: GHC.IO.FilePath -> IO ()
          writeMetadata tempfolder = do
            path <- makeAbsolute (tempfolder </> "metadataV1new.json")
            echo $ "Writing metadata to file: " <> tshow path
            BSL.writeFile path $ encodePretty metadata
            echo "Done."

      let commitMessage = "Update GitHub index file"
      runAndPushMaster metadataRepo commitMessage
        writeMetadata
        [ "mv -f metadataV1new.json metadataV1.json"
        , "git add metadataV1.json"
        ]
    _ -> pure ()


packageSetCommenter :: GitHub.AuthMethod am => am -> IO b
packageSetCommenter token = do
  pullChan <- atomically $ Chan.dupTChan bus
  forever $ atomically (Chan.readTChan pullChan) >>= \case
    NewVerification result -> do
      maybePR <- getPullRequestForUser token "spacchettibotti" packageSetsRepo

      case maybePR of
        Nothing -> do
          echo "Could not find an open PR, waiting 5 mins.."
          Concurrent.threadDelay (5 * 60 * 1000000)
          atomically $ Chan.writeTChan bus $ NewVerification result
        Just GitHub.PullRequest{..} -> do
          let commentBody = case result of
                (ExitSuccess, _, _) -> "Result of `spago verify-set` in a clean project: **success** 🎉"
                (_, out, err) -> Text.unlines
                  [ "Result of `spago verify-set` in a clean project: **failure** 😱"
                  , ""
                  , "<details><summary>Output of `spago verify-set`</summary><p>"
                  , ""
                  , "```"
                  , out
                  , "```"
                  , ""
                  , "</p></details>"
                  , ""
                  , "<details><summary>Error output</summary><p>"
                  , ""
                  , "```"
                  , err
                  , "```"
                  , ""
                  , "</p></details>"
                  ]
          let (Address owner repo) = packageSetsRepo
          (GitHub.executeRequest token $ GitHub.createCommentR owner repo pullRequestNumber commentBody) >>= \case
            Left err -> echo $ "Something went wrong while commenting. Error: " <> tshow err
            Right _ -> echo "Commented on the open PR"
    _ -> pure ()


data BotCommand
  = Ban PackageName
  | Unban PackageName
  deriving (Eq, Ord)

packageSetsUpdater :: GitHub.AuthMethod am => am -> IO ()
packageSetsUpdater token = do
  pullChan <- atomically $ Chan.dupTChan bus
  forever $ atomically (Chan.readTChan pullChan) >>= \case
    NewMetadata newMetadata -> do
      -- This metadata is the most up-to-date snapshot of all the repos in package-sets
      -- master branch. Among other things it contains the latest releases of all the
      -- packages in there.
      -- So here we take all these releases, and diff this list with the current
      -- package set from the state - i.e. the one versioned in package-sets master.
      -- This gives us a list of packages that need to be updated.
      -- In doing this we consider packages that might have been "banned"
      -- (i.e. that we don't want to update)
      let intersectionMaybe f = Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMaybeMatched f)
      let computePackagesToUpdate metadata packageSet banned
            = intersectionMaybe pickPackage metadata packageSet
            where
              pickPackage :: PackageName -> RepoMetadataV1 -> Package -> Maybe (Tag, Text)
              -- | We throw away packages without a release
              pickPackage _ RepoMetadataV1{ latest = Nothing, ..} _ = Nothing
              -- | And the local ones
              pickPackage _ _ Package{ location = Local _, ..} = Nothing
              -- | For the remaining ones: we pick the latest from metadata if it's
              --   different from the one we have in the set. Except if that version
              --   is banned, in that case we pick it from the package set
              pickPackage packageName RepoMetadataV1{ latest = Just latest, owner } Package{ location = Remote{ version } }
                = case (latest == Tag version, Set.member (packageName, latest) banned) of
                    (True, _) -> Nothing
                    (_, True) -> Just (Tag version, owner)
                    (_, _)    -> Just (latest, owner)

      State{..} <- Concurrent.readMVar state
      let removeBannedOverrides packageName (Tag tag, _) = case Map.lookup packageName packageSet of
            Just Package{ location = Remote {version}} | tag == version -> False
            _ -> True
      let newVersionsWithBanned = computePackagesToUpdate newMetadata packageSet banned
      let newVersions = Map.filterWithKey removeBannedOverrides newVersionsWithBanned

      let patchVersions path = do
            for_ (Map.toList newVersionsWithBanned) $ \(packageName, (tag, owner)) -> do
              echo $ "Patching version for " <> tshow packageName
              withAST (Text.pack $ path </> "src" </> "groups" </> Text.unpack (Text.toLower owner) <> ".dhall")
                $ updateVersion packageName tag

            echo "Verifying new set. This might take a LONG while.."
            result <- runWithCwd path "cd src; spago init; spago verify-set"
            echo "Verified packages, spamming the channel with the result.."
            atomically $ Chan.writeTChan bus $ NewVerification result

      let commands =
            [ "make"
            , "git add packages.json"
            , "git add src/groups"
            ]

      echo $ "Found " <> tshow (length newVersions) <> " packages to update"

      when (length newVersions > 0) $ do
        echo $ tshow newVersions
        -- If we have more than one package to update, let's see if we already have an
        -- open PR to package-sets. If we do we can just commit there
        maybePR <- getPullRequestForUser token "spacchettibotti" packageSetsRepo

        case maybePR of
          Nothing -> do
            today <- (Text.pack . Time.showGregorian . Time.utctDay) <$> Time.getCurrentTime
            let prBranchName = "spacchettibotti-updates-" <> today
                prTitle = "Updates " <> today
                prAddress = packageSetsRepo
                prBody = mkBody newVersions banned
            runAndOpenPR token PullRequest{..} patchVersions commands
          Just GitHub.PullRequest{ pullRequestHead = GitHub.PullRequestCommit{..}, ..} -> do
            -- A PR is there and there might be updates to the banned packages, so we
            -- try to update the banlist
            commentsForPR <- getCommentsOnPR token packageSetsRepo pullRequestNumber
            let newBanned = computeNewBanned commentsForPR banned packageSet
            let newVersionsWithBanned' = computePackagesToUpdate newMetadata packageSet newBanned
            let newVersions' = Map.filterWithKey removeBannedOverrides newVersionsWithBanned'

            -- Since a PR is already there we might have to skip verification,
            -- because we might have verified that commit already
            -- Since we leave a comment every time we verify, we can check if there
            -- are any new commits since the last comment
            -- (this means that any comment will retrigger a verification)
            let shouldVerifyAgain path = do
                  lastCommitTime <- getLatestCommitTime path
                  let lastCommentTime = case lastMay commentsForPR of
                        Nothing -> pullRequestCreatedAt
                        Just GitHub.IssueComment{..} -> issueCommentCreatedAt
                  pure $ or
                    -- If the banned packages changed
                    [ newVersions /= newVersions'
                    -- Or the latest commit to our branch is newer than our latest comment on the PR
                    , Time.diffUTCTime lastCommitTime lastCommentTime > 0
                    ]
            let patchVersions' path = shouldVerifyAgain path >>= \case
                  False -> echo "Skipping verification as there's nothing new under the sun.."
                  True -> do
                    patchVersions path
                    updatePullRequestBody token packageSetsRepo pullRequestNumber $ mkBody newVersions' newBanned

            runAndPushBranch pullRequestCommitRef packageSetsRepo pullRequestTitle patchVersions' commands
    _ -> pure ()
  where
    computeNewBanned comments banned packageSet
      = Set.fromList $ Map.toList
      $ foldl applyCommand (Map.fromList $ Set.toList banned)
      $ mapMaybe parseComment comments
      where
        applyCommand :: Map PackageName Tag -> BotCommand -> Map PackageName Tag
        applyCommand bannedMap (Ban package) | Just Package{ location = Remote{ version }} <- Map.lookup package packageSet
          = Map.insert package (Tag version) bannedMap
        applyCommand bannedMap (Unban package) = Map.delete package bannedMap
        applyCommand bannedMap _ = bannedMap

        parseComment GitHub.IssueComment{..} = Parse.parseMaybe parseCommand issueCommentBody

        parseCommand :: Parse.Parsec Void Text BotCommand
        parseCommand = do
          void $ Parse.chunk "@spacchettibotti "
          command <- (Parse.chunk "ban " >> pure Ban) <|> (Parse.chunk "unban " >> pure Unban)
          package <- PackageName <$> Parse.takeRest
          pure $ command package

    mkBody packages banned =
      let renderUpdate (PackageName packageName, (Tag tag, owner))
            = "- [`" <> packageName <> "` upgraded to `" <> tag <> "`](https://github.com/"
              <> owner <> "/purescript-" <> packageName <> "/releases/tag/" <> tag <> ")"
          renderBanned (PackageName packageName, Tag tag)
            = "- `" <> packageName <> "`@`" <> tag <> "`"
      in Text.unlines
         $ [ "Updated packages:" ]
         <> fmap renderUpdate (Map.toList packages)
         <> (if Set.null banned
             then []
             else [ "", "Banned packages:" ] <> fmap renderBanned (Set.toList banned))
         <> [ ""
            , "You can give commands to the bot by adding a comment where you tag it, e.g.:"
            , "- `@spacchettibotti ban react-basic`"
            , "- `@spacchettibotti unban simple-json`"
            ]

    updateVersion :: Monad m => PackageName -> Tag -> Expr -> m Expr
    updateVersion (PackageName packageName) (Tag tag) (Dhall.RecordLit kvs)
      | Just (Dhall.RecordLit pkgKVs) <- Dhall.Map.lookup packageName kvs
      , Just (Dhall.TextLit _) <- Dhall.Map.lookup "version" pkgKVs =
          let
            newPackageVersion = Dhall.toTextLit tag
            newPackage = Dhall.RecordLit $ Dhall.Map.insert "version" newPackageVersion pkgKVs
          in pure $ Dhall.RecordLit $ Dhall.Map.insert packageName newPackage kvs
    updateVersion _ _ other = pure other



-- * Machinery
--
--


data PullRequest = PullRequest
  { prBranchName :: Text
  , prAddress    :: GitHubAddress
  , prTitle      :: Text
  , prBody       :: Text
  }


runAndPushBranch :: Text -> GitHubAddress -> Text -> (GHC.IO.FilePath -> IO ()) -> [Text] -> IO ()
runAndPushBranch branchName address commit preAction commands
  = runInClonedRepo address branchName commit preAction commands (pure ())


runAndPushMaster :: GitHubAddress -> Text -> (GHC.IO.FilePath -> IO ()) -> [Text] -> IO ()
runAndPushMaster = runAndPushBranch "master"


runAndOpenPR :: GitHub.AuthMethod am => am -> PullRequest -> (GHC.IO.FilePath -> IO ()) -> [Text] -> IO ()
runAndOpenPR token PullRequest{ prAddress = address@Address{..}, ..} preAction commands
  = unlessM pullRequestExists (runInClonedRepo address prBranchName prTitle preAction commands openPR)
  where
    openPR = do
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


runInClonedRepo :: GitHubAddress -> Text -> Text -> (GHC.IO.FilePath -> IO ()) -> [Text] -> IO () -> IO ()
runInClonedRepo address@Address{..} branchName commit preAction commands postAction =
  -- Clone the repo in a temp folder
  Temp.withTempDirectory "data" "__temp-repo" $ \path -> do
    let repoPath = Text.unpack $ GitHub.untagName repo
    let runInRepo cmds failure success = do
          (code, out, err) <- runWithCwd (path </> repoPath) $ Text.intercalate " && " cmds
          if code /= ExitSuccess
            then do
              failure
              echo out
              echo err
            else success

    (code, _out, _err) <- runWithCwd path $ "git clone git@github.com:" <> GitHub.untagName owner <> "/" <> GitHub.untagName repo <> ".git"
    if code /= ExitSuccess
      then echo "Error while cloning repo"
      else do
        echo $ "Cloned " <> tshow address
        -- Configure the repo: set the git identity to spacchettibotti and switch to the branch
        runInRepo
          [ "git config --local user.name 'Spacchettibotti'"
          , "git config --local user.email 'spacchettibotti@ferrai.io'"
          , "git checkout " <> branchName <> " || git checkout -b " <> branchName
          ]
          (echo "Failed to configure the repo")
          -- If the setup was fine, run the setup code before running the commands
          (preAction =<< makeAbsolute (path </> repoPath))
        -- Run the commands we wanted to run
        runInRepo
          commands
          (echo "Something was off while running commands..")
          -- Check if anything actually changed or got staged
          (runInRepo
            [ "git diff --staged --exit-code" ]
            (runInRepo
              [ "git commit -m '" <> commit <> "'"
              , "git push --set-upstream origin " <> branchName
              ]
              (echo "Failed to commit!")
              postAction)
            (echo "Nothing to commit, skipping.."))


runWithCwd :: MonadIO io => GHC.IO.FilePath -> Text -> io (ExitCode, Text, Text)
runWithCwd cwd cmd = do
  echo $ "Running in path " <> Text.pack cwd <> ": `" <> cmd <> "`"
  let processWithNewCwd = (Process.shell (Text.unpack cmd)) { Process.cwd = Just cwd }
  systemStrictWithErr processWithNewCwd empty


getLatestCommitTime :: GHC.IO.FilePath -> IO Time.UTCTime
getLatestCommitTime path = do
  (_code, out, _err) <- runWithCwd path "git show -s --format=%ci"
  Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" $ Text.unpack out


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
