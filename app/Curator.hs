{-# LANGUAGE BangPatterns #-}

{-|

# The `spago-curator` tool

The purpose of this executable is to assist in the automation of certain infrastructure
tasks that make life easier in Spago-land (both for maintainers and users).
Basically pulling data from places and pushing it somewhere else, and kind of keeping
things in sync so that humans don't have to do it.

It requires a GitHub token in the `SPACCHETTIBOTTI_TOKEN` and a configured ssh key,
authenticated to all the repos it pushes to.
All its operations are run as the `spacchettibotti` user.

To see all the things that this program does, check out the "Threads" section

-}


module Curator where

import           Spago.Prelude

import qualified Control.Concurrent            as Concurrent
import qualified Control.Concurrent.STM.TChan  as Chan
import qualified Control.Retry                 as Retry
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Map.Merge.Strict         as Map
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Encoding
import qualified Data.Time                     as Time
import qualified Data.Vector                   as Vector
import qualified Data.Set                      as Set
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
import qualified Text.Megaparsec               as Parse

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


-- | Concurrent-safe global state, so we can read data in here instead of
--   having to pass it around.
state :: Concurrent.MVar State
state = GHC.IO.unsafePerformIO $ Concurrent.newMVar emptyState


-- | Main message bus. It is write-only so you should use `spawnThread` to read from it
bus :: Chan.TChan Message
bus = GHC.IO.unsafePerformIO Chan.newBroadcastTChanIO


packageSetsRepo, purescriptRepo, docsSearchRepo, spagoRepo, metadataRepo :: GitHubAddress
packageSetsRepo = Address "purescript" "package-sets"
purescriptRepo  = Address "purescript" "purescript"
docsSearchRepo  = Address "spacchetti" "purescript-docs-search"
metadataRepo    = Address "spacchetti" "package-sets-metadata"
spagoRepo       = Address "spacchetti" "spago"


type Curator = RIO LogFunc

main :: IO ()
main = withBinaryFile "curator.log" AppendMode $ \configHandle -> do
  logStderr <- setLogUseLoc False <$> logOptionsHandle stderr True
  logFile <- setLogUseLoc False <$> logOptionsHandle configHandle True

  withLogFunc logStderr $ \logFuncConsole -> withLogFunc logFile $ \logFuncFile ->
    runRIO (logFuncConsole <> logFuncFile) $ do
    -- We always want to run in UTF8 anyways
    liftIO $ GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8
    -- Stop `git` from asking for input, not gonna happen
    -- We just fail instead. Source:
    -- https://serverfault.com/questions/544156
    liftIO $ Env.setEnv "GIT_TERMINAL_PROMPT" "0"

    -- Read GitHub Auth Token
    logInfo "Reading GitHub token.."
    token <- liftIO $ (GitHub.OAuth . Encoding.encodeUtf8 . Text.pack) <$> Env.getEnv "SPACCHETTIBOTTI_TOKEN"

    -- Prepare data folder that will contain the temp copies of the repos
    logInfo "Creating 'data' folder"
    mktree "data"

    env <- ask
    let spawnThread :: Text -> (Message -> Curator ()) -> Curator ()
        spawnThread name thread = do
          let threadLoop :: IO ()
              threadLoop = do
                pullChan <- atomically $ Chan.dupTChan bus
                forever $ atomically (Chan.readTChan pullChan) >>= (runRIO env . thread)
          logInfo $ "Spawning thread " <> displayShow name
          void $ liftIO $ Concurrent.forkIO $ catch threadLoop $ \(err :: SomeException) -> runRIO env $ do
            logError $ "Thread " <> displayShow name <> " broke, restarting.."
            logError $ "Error was: " <> display err
            spawnThread name thread

    -- Start spawning threads
    --   General utility
    spawnThread "writer"                  $ persistState
    --   purescript repo
    spawnThread "releaseCheckPureScript"  $ checkLatestRelease token purescriptRepo
    --   purescript-docs-search repo
    spawnThread "releaseCheckDocsSearch"  $ checkLatestRelease token docsSearchRepo
    --   spago repo
    spawnThread "spagoUpdatePackageSets"  $ spagoUpdatePackageSets token
    spawnThread "spagoUpdateDocsSearch"   $ spagoUpdateDocsSearch token
    --     TODO: update purescript-metadata repo on purs release
    spawnThread "spagoUpdatePurescript"   $ spagoUpdatePurescriptVersion token
    --   package-sets-metadata repo
    spawnThread "metadataFetcher"         $ metadataFetcher token
    spawnThread "metadataUpdater"         $ metadataUpdater
    -- package-sets repo
    spawnThread "releaseCheckPackageSets" $ checkLatestRelease token packageSetsRepo
    spawnThread "packageSetsUpdater"      $ packageSetsUpdater token
    spawnThread "packageSetsCommenter"    $ packageSetCommenter token

    -- To kickstart the whole thing we just need to send a "heartbeat" on the bus once an hour
    -- Threads will be listening to this and act accordingly
    liftIO $ forever $ do
      atomically $ Chan.writeTChan bus RefreshState
      sleep _60m

  where
    _60m = 60 * 60 * 1000000

    sleep = Concurrent.threadDelay




-- * GitHub operations
--
--


getLatestRelease :: GitHub.AuthMethod am => am -> GitHubAddress -> Curator (Either GitHub.Error GitHub.Release)
getLatestRelease token address@(Address owner repo) = do
  logInfo $ "Getting latest release for " <> displayShow address
  liftIO $ GitHub.executeRequest token $ GitHub.latestReleaseR owner repo


getTags :: GitHub.AuthMethod am => am -> GitHubAddress -> Curator (Either GitHub.Error (Maybe Tag, (Map Tag CommitHash)))
getTags token address@(Address owner repo) = do
  logInfo $ "Getting tags for " <> displayShow address
  res <- liftIO $ GitHub.executeRequest token $ GitHub.tagsForR owner repo GitHub.FetchAll
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


getCommits :: GitHub.AuthMethod am => am -> GitHubAddress -> Curator (Either GitHub.Error [CommitHash])
getCommits token address@(Address owner repo) = do
  logInfo $ "Getting commits for " <> displayShow address
  res <- liftIO $ GitHub.executeRequest token $ GitHub.commitsForR owner repo GitHub.FetchAll
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
persistState :: Message -> Curator ()
persistState = \case
  RefreshState -> pure ()
  NewVerification _ -> pure () -- TODO: maybe save this?
  NewRepoRelease address release -> liftIO $ Concurrent.modifyMVar_ state
    $ \State{..} -> let newReleases = Map.insert address release latestReleases
                    in pure State{ latestReleases = newReleases , ..}
  NewMetadata newMetadata -> liftIO $ Concurrent.modifyMVar_ state
    $ \State{..} -> pure State{ metadata = newMetadata, ..}
  NewPackageSet newPackageSet -> liftIO $ Concurrent.modifyMVar_ state
    $ \State{..} -> pure State{ packageSet = newPackageSet, ..}


-- | Call GitHub to check for new releases of a repository
--   When there's a new one and we don't have it in our state we send a message on the bus
checkLatestRelease :: GitHub.Auth -> GitHubAddress -> Message -> Curator ()
checkLatestRelease token address RefreshState = getLatestRelease token address >>= \case
  Left _ -> pure () -- TODO: error out here?
  Right GitHub.Release {..} -> do
    State{..} <- liftIO $ Concurrent.readMVar state
    case Map.lookup address latestReleases of
      -- We don't do anything if we have a release saved and it's the current one
      Just currentRelease | currentRelease == releaseTagName -> pure ()
      _ -> do
        logInfo $ "Found a new release for " <> displayShow address <> ": " <> display releaseTagName
        atomically $ Chan.writeTChan bus $ NewRepoRelease address releaseTagName
checkLatestRelease _ _ _ = pure ()


-- | Whenever there's a new release of package-sets, update it in Spago's template
spagoUpdatePackageSets :: GitHub.AuthMethod am => am -> Message -> Curator ()
spagoUpdatePackageSets token (NewRepoRelease address newTag) | address == packageSetsRepo = do
  let prTitle = "Update to package-sets@" <> newTag
  let prBranchName = "spacchettibotti-" <> newTag
  runAndOpenPR token PullRequest{ prBody = "", prAddress = spagoRepo, ..} (const $ pure ())
    [ "cd templates"
    , "spago upgrade-set"
    , "git add packages.dhall"
    ]
spagoUpdatePackageSets _ _ = pure ()


-- | Whenever there's a new release of purescript, update the release on our various CI files
spagoUpdatePurescriptVersion :: GitHub.AuthMethod am => am -> Message -> Curator ()
spagoUpdatePurescriptVersion token (NewRepoRelease address newTag) | address == purescriptRepo = do
  let prTitle = "Update to purescript@" <> newTag
  let prBranchName = "spacchettibotti-purs-" <> newTag
  let bundleTag = Text.drop 1 newTag
  runAndOpenPR token PullRequest{ prBody = "", prAddress = spagoRepo, ..} (const $ pure ())
    [ "sed -e 's/$tag =.*$/$tag = " <> surroundQuote newTag <> "/g' -i appveyor.yml"
    , "sed -e 's/    TAG=.*$/    TAG=" <> surroundQuote newTag <> "/g' -i .travis.yml"
    , "sed -e 's/Generated by purs bundle.*/Generated by purs bundle " <> bundleTag <> "/g' -i test/fixtures/bundle-app.js"
    , "sed -e 's/Generated by purs bundle.*/Generated by purs bundle " <> bundleTag <> "/g' -i test/fixtures/bundle-module.js"
    , "sed -e 's/Generated by purs version.*/Generated by purs version " <> bundleTag <> "/g' -i test/fixtures/bundle-app.js"
    , "sed -e 's/Generated by purs version.*/Generated by purs version " <> bundleTag <> "/g' -i test/fixtures/bundle-module.js"
    , "git add .travis.yml appveyor.yml test"
    ]
spagoUpdatePurescriptVersion _ _ = pure ()


-- | Whenever there's a new release of the purescript-docs-search, update our version of it
spagoUpdateDocsSearch :: GitHub.AuthMethod am => am -> Message -> Curator ()
spagoUpdateDocsSearch token (NewRepoRelease address newTag) | address == docsSearchRepo = do
  let prTitle = "Update to purescript-docs-search@" <> newTag
  let prBranchName = "spacchettibotti-docs-search-" <> newTag
  runAndOpenPR token PullRequest{ prBody = "", prAddress = spagoRepo, ..} (const $ pure ())
    [ "sed -e 's/docsSearchVersion = .*/docsSearchVersion = " <> surroundQuote newTag <> "/g' -i src/Spago/Prelude.hs"
    , "git add src"
    ]
spagoUpdateDocsSearch _ _ = pure ()


-- | Take the latest package set from package-sets master, get a list of all the
--   packages in there, and thenn query their commits and tags. Once done, send
--   the package on the bus.
metadataFetcher :: GitHub.AuthMethod am => am -> Message -> Curator ()
metadataFetcher token RefreshState = do
  logInfo "Downloading and parsing package set.."
  packageSet <- fetchPackageSet
  atomically $ Chan.writeTChan bus $ NewPackageSet packageSet
  let packages = Map.toList packageSet
  logInfo $ "Fetching metadata for " <> display (length packages) <> " packages"

  -- Call GitHub for all these packages and get metadata for them
  metadata <- withTaskGroup' 10 $ \taskGroup -> do
    asyncs <- for packages (async' taskGroup . fetchRepoMetadata)
    for asyncs wait'

  logInfo "Fetched all metadata."
  atomically $ Chan.writeTChan bus $ NewMetadata $ foldMap (uncurry Map.singleton) metadata

  where
    fetchRepoMetadata :: (PackageName, Package) -> Curator (PackageName, RepoMetadataV1)
    fetchRepoMetadata (_, pkg@Package{ location = Local{..}, ..}) = die [ "Tried to fetch a local package: " <> displayShow pkg ]
    fetchRepoMetadata (packageName, Package{ location = Remote{ repo = Repo repoUrl, ..}, ..}) =
      Retry.recoverAll (Retry.fullJitterBackoff 50000 <> Retry.limitRetries 25) $ \Retry.RetryStatus{..} -> do
        let !(owner:repo:_rest)
              = Text.split (=='/')
              $ Text.replace "https://github.com/" ""
              $ case Text.isSuffixOf ".git" repoUrl of
                  True  -> Text.dropEnd 4 repoUrl
                  False -> repoUrl
            address = Address (GitHub.mkName Proxy owner) (GitHub.mkName Proxy repo)

        logDebug $ "Retry " <> display rsIterNumber <> ": fetching tags and commits for " <> displayShow address

        !eitherTags <- getTags token address
        !eitherCommits <- getCommits token address

        case (eitherTags, eitherCommits) of
          (Left _, _) -> die [ "Retry " <> display rsIterNumber <> ": failed to fetch tags" ]
          (_, Left _) -> die [ "Retry " <> display rsIterNumber <> ": failed to fetch commits" ]
          (Right (latest, tags), Right commits) -> do
            pure (packageName, RepoMetadataV1{..})


    -- | Tries to read in a PackageSet from GitHub, master branch
    --   (so we always get the most up to date and we don't have to wait for a release)
    fetchPackageSet :: (MonadIO m, MonadThrow m, MonadReader env m, HasLogFunc env) => m PackageSetMap
    fetchPackageSet = do
      expr <- liftIO $ Dhall.inputExpr "https://raw.githubusercontent.com/purescript/package-sets/master/src/packages.dhall"
      case expr of
        Dhall.RecordLit pkgs -> fmap (Map.mapKeys PackageName . Dhall.Map.toMap)
          $ traverse Spago.Config.parsePackage pkgs
        something -> throwM $ Dhall.PackagesIsNotRecord something
metadataFetcher _ _ = pure ()


-- | Whenever there's a new metadata set, push it to the repo
metadataUpdater :: Message -> Curator ()
metadataUpdater (NewMetadata metadata) = do
  -- Write the metadata to file
  let writeMetadata :: GHC.IO.FilePath -> Curator ()
      writeMetadata tempfolder = do
        path <- makeAbsolute (tempfolder </> "metadataV1new.json")
        logInfo $ "Writing metadata to file: " <> displayShow path
        liftIO $ BSL.writeFile path $ encodePretty metadata
        logInfo "Done."

  let commitMessage = "Update GitHub index file"
  runAndPushMaster metadataRepo commitMessage
    writeMetadata
    [ "mv -f metadataV1new.json metadataV1.json"
    , "git add metadataV1.json"
    ]
metadataUpdater _ = pure ()


-- | Watch out for the result of a `spago verify-set` command, and comment appropriately
--   on the PR thread (if any)
packageSetCommenter :: GitHub.AuthMethod am => am -> Message -> Curator ()
packageSetCommenter token (NewVerification result) = do
  maybePR <- liftIO $ getPullRequestForUser token "spacchettibotti" packageSetsRepo

  case maybePR of
    Nothing -> do
      logWarn "Could not find an open PR, waiting 5 mins.."
      liftIO $ Concurrent.threadDelay (5 * 60 * 1000000)
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
      (liftIO $ GitHub.executeRequest token $ GitHub.createCommentR owner repo pullRequestNumber commentBody) >>= \case
        Left err -> logError $ "Something went wrong while commenting. Error: " <> displayShow err
        Right _ -> logInfo "Commented on the open PR"
packageSetCommenter _ _ = pure ()


data BotCommand
  = Ban PackageName
  | Unban PackageName
  deriving (Eq, Ord)


-- | This fairly sophisticated thing will try to upgrade packages in the set
--   as soon as there's a new release available for them.
--   It will do so by getting the releases, patching the Dhall files in there,
--   then opening a PR with the result of `spago verify-set`.
--   Once in there, it will parse the comments on the PR to find out which
--   packages we want to temporarily ban from the verification/upgrade process
packageSetsUpdater :: GitHub.AuthMethod am => am -> Message -> Curator ()
packageSetsUpdater token (NewMetadata newMetadata) = do
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

  State{..} <- liftIO $ Concurrent.readMVar state
  let removeBannedOverrides packageName (Tag tag, _) = case Map.lookup packageName packageSet of
        Just Package{ location = Remote {version}} | tag == version -> False
        _ -> True
  let newVersionsWithBanned = computePackagesToUpdate newMetadata packageSet banned
  let newVersions = Map.filterWithKey removeBannedOverrides newVersionsWithBanned

  let patchVersions :: GHC.IO.FilePath -> Curator ()
      patchVersions path = do
        for_ (Map.toList newVersionsWithBanned) $ \(packageName, (tag, owner)) -> do
          logInfo $ "Patching version for " <> displayShow packageName
          withAST (Text.pack $ path </> "src" </> "groups" </> Text.unpack (Text.toLower owner) <> ".dhall")
            $ updateVersion packageName tag

        logInfo "Verifying new set. This might take a LONG while.."
        result <- runWithCwd path "cd src; spago init; spago verify-set"
        logInfo "Verified packages, spamming the channel with the result.."
        atomically $ Chan.writeTChan bus $ NewVerification result

  let commands =
        [ "make"
        , "git add packages.json"
        , "git add src/groups"
        ]

  logInfo $ "Found " <> display (length newVersions) <> " packages to update"

  when (length newVersions > 0) $ do
    logInfo $ displayShow newVersions
    -- If we have more than one package to update, let's see if we already have an
    -- open PR to package-sets. If we do we can just commit there
    maybePR <- liftIO $ getPullRequestForUser token "spacchettibotti" packageSetsRepo

    case maybePR of
      Nothing -> do
        today <- liftIO $ (Text.pack . Time.showGregorian . Time.utctDay) <$> Time.getCurrentTime
        let prBranchName = "spacchettibotti-updates-" <> today
            prTitle = "Updates " <> today
            prAddress = packageSetsRepo
            prBody = mkBody newVersions banned
        runAndOpenPR token PullRequest{..} patchVersions commands
      Just GitHub.PullRequest{ pullRequestHead = GitHub.PullRequestCommit{..}, ..} -> do
        -- A PR is there and there might be updates to the banned packages, so we
        -- try to update the banlist
        commentsForPR <- liftIO $ getCommentsOnPR token packageSetsRepo pullRequestNumber
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
              False -> logInfo "Skipping verification as there's nothing new under the sun.."
              True -> do
                patchVersions path
                liftIO $ updatePullRequestBody token packageSetsRepo pullRequestNumber $ mkBody newVersions' newBanned

        runAndPushBranch pullRequestCommitRef packageSetsRepo pullRequestTitle patchVersions' commands
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
packageSetsUpdater _ _ = pure ()


-- * Machinery
--
--


data PullRequest = PullRequest
  { prBranchName :: Text
  , prAddress    :: GitHubAddress
  , prTitle      :: Text
  , prBody       :: Text
  }


runAndPushBranch :: Text -> GitHubAddress -> Text -> (GHC.IO.FilePath -> Curator ()) -> [Text] -> Curator ()
runAndPushBranch branchName address commit preAction commands
  = runInClonedRepo address branchName commit preAction commands (pure ())


runAndPushMaster :: GitHubAddress -> Text -> (GHC.IO.FilePath -> Curator ()) -> [Text] -> Curator ()
runAndPushMaster = runAndPushBranch "master"


runAndOpenPR :: GitHub.AuthMethod am => am -> PullRequest -> (GHC.IO.FilePath -> Curator ()) -> [Text] -> Curator ()
runAndOpenPR token PullRequest{ prAddress = address@Address{..}, ..} preAction commands
  = unlessM pullRequestExists (runInClonedRepo address prBranchName prTitle preAction commands openPR)
  where
    openPR :: Curator ()
    openPR = do
      logInfo "Pushed a new commit, opening PR.."
      response <- liftIO $ GitHub.executeRequest token
        $ GitHub.createPullRequestR owner repo
        $ GitHub.CreatePullRequest prTitle prBody prBranchName "master"
      case response of
        Right _   -> logInfo "Created PR 🎉"
        Left err' -> logError $ "Error while creating PR: " <> displayShow err'

    pullRequestExists :: Curator Bool
    pullRequestExists = do
      logInfo $ "Checking if we ever opened a PR " <> displayShow prTitle

      oldPRs <- liftIO
        $ GitHub.executeRequest token
        $ GitHub.pullRequestsForR owner repo
        (GitHub.optionsHead (GitHub.untagName owner <> ":" <> prBranchName) <> GitHub.stateAll)
        GitHub.FetchAll
      case oldPRs of
        Left err -> do
          logError $ "Error: " <> displayShow err
          pure True
        Right prs | not $ Vector.null prs -> do
          logWarn "PR was opened, skipping.."
          pure True
        Right _ -> do
          logInfo "No previous PRs found, opening one.."
          pure False


runInClonedRepo :: GitHubAddress -> Text -> Text -> (GHC.IO.FilePath -> Curator ()) -> [Text] -> Curator () -> Curator ()
runInClonedRepo address@Address{..} branchName commit preAction commands postAction =
  -- Clone the repo in a temp folder
  Temp.withTempDirectory "data" "__temp-repo" $ \path -> do
    let repoPath = Text.unpack $ GitHub.untagName repo
    let runInRepo cmds failure success = do
          (code, out, err) <- runWithCwd (path </> repoPath) $ Text.intercalate " && " cmds
          if code /= ExitSuccess
            then do
              failure
              logInfo $ display out
              logError $ display err
            else success

    (code, _out, _err) <- runWithCwd path $ "git clone git@github.com:" <> GitHub.untagName owner <> "/" <> GitHub.untagName repo <> ".git"
    if code /= ExitSuccess
      then logError "Error while cloning repo"
      else do
        logInfo $ "Cloned " <> displayShow address
        -- Configure the repo: set the git identity to spacchettibotti and switch to the branch
        runInRepo
          [ "git config --local user.name 'Spacchettibotti'"
          , "git config --local user.email 'spacchettibotti@ferrai.io'"
          , "git checkout " <> branchName <> " || git checkout -b " <> branchName
          ]
          (logError "Failed to configure the repo")
          -- If the setup was fine, run the setup code before running the commands
          (preAction =<< makeAbsolute (path </> repoPath))
        -- Run the commands we wanted to run
        runInRepo
          commands
          (logError "Something was off while running commands..")
          -- Check if anything actually changed or got staged
          (runInRepo
            [ "git diff --staged --exit-code" ]
            (runInRepo
              [ "git commit -m '" <> commit <> "'"
              , "git push --set-upstream origin " <> branchName
              ]
              (logError "Failed to commit!")
              postAction)
            (logInfo "Nothing to commit, skipping.."))


runWithCwd :: GHC.IO.FilePath -> Text -> Curator (ExitCode, Text, Text)
runWithCwd cwd cmd = do
  logDebug $ "Running in path " <> displayShow cwd <> ": `" <> display cmd <> "`"
  let processWithNewCwd = (Process.shell (Text.unpack cmd)) { Process.cwd = Just cwd }
  systemStrictWithErr processWithNewCwd empty


getLatestCommitTime :: GHC.IO.FilePath -> Curator Time.UTCTime
getLatestCommitTime path = do
  (_code, out, _err) <- runWithCwd path "git show -s --format=%ci"
  Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" $ Text.unpack out


withAST :: (MonadIO m, MonadReader env m, HasLogFunc env) => Text -> (Expr -> m Expr) -> m ()
withAST path transform = do
  rawConfig <- liftIO $ Dhall.readRawExpr path
  case rawConfig of
    Nothing -> logWarn $ "Could not find file " <> display path
    Just (header, expr) -> do
      newExpr <- transformMExpr transform expr
      logInfo $ "Done. Updating the \"" <> display path <> "\" file.."
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
