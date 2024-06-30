module Spago.Git
  ( Git
  , GitEnv
  , fetchRepo
  , getGit
  , getRef
  , listTags
  , getStatus
  , pushTag
  , isIgnored
  , tagCheckedOut
  , determineShouldCache
  ) where

import Spago.Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String
import Node.ChildProcess.Types (Exit(..))
import Node.Path as Path
import Node.Process as Process
import Registry.Version as Version
import Spago.Cmd as Cmd
import Spago.FS as FS

type Git = { cmd :: String, version :: String }

type GitEnv a = { git :: Git, logOptions :: LogOptions, offline :: OnlineStatus | a }

runGit_ :: forall a. Array String -> Maybe FilePath -> ExceptT String (Spago (GitEnv a)) Unit
runGit_ args cwd = void $ runGit args cwd

runGit :: forall a. Array String -> Maybe FilePath -> ExceptT String (Spago (GitEnv a)) String
runGit args cwd = ExceptT do
  { git } <- ask
  result <- Cmd.exec git.cmd args (Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, cwd = cwd })
  pure case result of
    Right r -> Right r.stdout
    Left r -> Left r.stderr

determineShouldCache :: forall b. String -> FilePath -> Spago (GitEnv b) Boolean
determineShouldCache ref path = do
  showRef <- Except.runExceptT $ runGit_ [ "show-ref", "--verify", "refs/heads/" <> ref ] (Just path)
  pure $ isLeft showRef

fetchRepo :: forall a b. { git :: String, ref :: String | a } -> FilePath -> Spago (GitEnv b) (Either (Array String) Boolean)
fetchRepo { git, ref } path = do
  repoExists <- FS.exists path
  { offline } <- ask
  case offline, repoExists of
    Offline, true -> do
      logDebug $ "Found " <> git <> " locally, skipping fetch because we are offline"
      Right <$> determineShouldCache ref path
    Offline, false -> die [ "You are offline and the repo '" <> git <> "' is not available locally, can't make progress." ]
    Online, _ -> do
      cloneOrFetchResult <- case repoExists of
        true -> do
          logDebug $ "Found " <> git <> " locally, pulling..."
          Except.runExceptT $ runGit_ [ "fetch", "origin" ] (Just path)
        false -> do
          logInfo $ "Cloning " <> git
          -- For the reasoning on the filter options, see:
          -- https://github.com/purescript/spago/issues/701#issuecomment-1317192919
          Except.runExceptT $ runGit_ [ "clone", "--filter=tree:0", git, path ] Nothing
      shouldCache <- determineShouldCache ref path
      result <- Except.runExceptT do
        Except.ExceptT $ pure cloneOrFetchResult
        logDebug $ "Checking out the requested ref for " <> git <> " : " <> ref
        _ <- runGit [ "checkout", ref ] (Just path)
        -- if we are on a branch and not on a detached head, then we need to pull
        -- the following command will fail if on a detached head, and succeed if on a branch
        Except.mapExceptT
          ( \a -> a >>= case _ of
              Left _err ->
                pure (Right shouldCache)
              Right _ ->
                Except.runExceptT do
                  logDebug "Pulling the latest changes"
                  runGit_ [ "pull", "--rebase", "--autostash" ] (Just path)
                  pure shouldCache
          )
          (runGit_ [ "symbolic-ref", "-q", "HEAD" ] (Just path))

      case result of
        Left err -> pure $ Left
          [ "Error while fetching the repo '" <> git <> "' at ref '" <> ref <> "':"
          , "  " <> err
          ]
        Right isBranch -> do
          logDebug $ "Successfully fetched the repo '" <> git <> "' at ref '" <> ref <> "'"
          pure $ Right isBranch

listTags :: forall a. Maybe FilePath -> Spago (GitEnv a) (Either Docc (Array String))
listTags cwd = do
  let opts = Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, cwd = cwd }
  { git } <- ask
  Cmd.exec git.cmd [ "tag" ] opts >>= case _ of
    Left r -> do
      pure $ Left $ toDoc [ "Could not run `git tag`. Error:", r.message ]
    Right r -> pure $ Right $ String.split (Pattern "\n") r.stdout

getStatus :: forall a. Maybe FilePath -> Spago (GitEnv a) (Either Docc String)
getStatus cwd = do
  let opts = Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, cwd = cwd }
  { git } <- ask
  Cmd.exec git.cmd [ "status", "--porcelain" ] opts >>= case _ of
    Left r -> do
      pure $ Left $ toDoc [ "Could not run `git status`. Error:", r.message ]
    Right r -> pure $ Right r.stdout

getRef :: forall a. Maybe FilePath -> Spago (GitEnv a) (Either Docc String)
getRef cwd = do
  let opts = Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, cwd = cwd }
  { git } <- ask
  Cmd.exec git.cmd [ "rev-parse", "HEAD" ] opts >>= case _ of
    Left r -> pure $ Left $ toDoc
      [ "Could not run `git rev-parse HEAD` to determine the current ref. Error:"
      , r.shortMessage
      ]
    Right r -> pure $ Right r.stdout

tagCheckedOut :: forall a. Maybe FilePath -> Spago (GitEnv a) (Either Docc String)
tagCheckedOut cwd = do
  let opts = Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, cwd = cwd }
  { git } <- ask
  Cmd.exec git.cmd [ "describe", "--tags", "--exact-match" ] opts >>= case _ of
    Left _ -> pure $ Left $ toDoc "The git ref currently checked out is not a tag."
    Right r -> pure $ Right r.stdout

pushTag :: forall a. Maybe FilePath -> Version -> Spago (GitEnv a) (Either Docc Unit)
pushTag cwd version = do
  let opts = Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, cwd = cwd }
  { git, offline } <- ask
  case offline of
    Offline -> do
      logWarn $ "Spago is in offline mode - not pushing the git tag v" <> Version.print version
      pure $ Right unit
    Online -> do
      logInfo $ "Pushing tag 'v" <> Version.print version <> "' to the remote"
      Cmd.exec git.cmd [ "push", "origin", "v" <> Version.print version ] opts >>= case _ of
        Left r -> pure $ Left $ toDoc
          [ "Could not push the tag 'v" <> Version.print version <> "' to the remote."
          , "Error:"
          , r.shortMessage
          ]
        Right _ -> pure $ Right unit

-- | Check if the path is ignored by git
--
-- `git check-ignore` exits with 1 when path is not ignored, and 128 when
-- a fatal error occurs (i.e. when not in a git repository).
isIgnored :: forall a. FilePath -> Spago (GitEnv a) Boolean
isIgnored path = do
  { git } <- ask
  result <- Cmd.exec git.cmd [ "check-ignore", "--quiet", path ] (Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false })
  case result of
    -- Git is successful if it's an ignored file
    Right _ -> pure true
    -- Git will fail with exitCode 128 if this is not a git repo or if it's dealing with a link.
    -- We ignore links - I mean, do we really want to deal with recursive links?!?
    Left r
      | Normally 128 <- r.exit -> do
          -- Sigh. Even if something is behind a link Node will not tell us that,
          -- so we need to check all the paths between the cwd and the provided path
          -- Just beautiful
          paths <- liftEffect do
            cwd <- Process.cwd
            absolutePath <- Path.resolve [] path
            FS.getInBetweenPaths cwd absolutePath
          Array.any identity <$> traverse FS.isLink paths
      -- Git will fail with 1 when a file is just, like, normally ignored
      | Normally 1 <- r.exit ->
          pure false
      | otherwise -> do
          logDebug "IsIgnored encountered an interesting exitCode"
          logDebug $ Cmd.printExecResult r
          -- We still do not ignore it, just in case
          pure false

getGit :: forall a. Spago (LogEnv a) Git
getGit = do
  Cmd.exec "git" [ "--version" ] Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false } >>= case _ of
    Right r -> pure { cmd: "git", version: r.stdout }
    Left r -> do
      logDebug $ Cmd.printExecResult r
      die [ "Failed to find git. Have you installed it, and is it in your PATH?" ]
