module Spago.Git
  ( Git
  , GitEnv
  , fetchRepo
  , getGit
  , getRef
  , getCleanTag
  , pushTag
  , isIgnored
  , tagCheckedOut
  ) where

import Spago.Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String
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
  pure $ bimap _.stderr _.stdout result

fetchRepo :: forall a b. { git :: String, ref :: String | a } -> FilePath -> Spago (GitEnv b) (Either (Array String) Unit)
fetchRepo { git, ref } path = do
  repoExists <- FS.exists path
  { offline } <- ask
  case offline, repoExists of
    Offline, true -> do
      logDebug $ "Found " <> git <> " locally, skipping fetch because we are offline"
      pure $ Right unit
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
      result <- Except.runExceptT do
        Except.ExceptT $ pure cloneOrFetchResult
        _ <- runGit [ "checkout", ref ] (Just path)
        -- if we are on a branch and not on a detached head, then we need to pull
        -- the following command will fail if on a detached head, and succeed if on a branch
        Except.mapExceptT
          ( \a -> a >>= case _ of
              Left _err -> pure (Right unit)
              Right _ -> Except.runExceptT $ runGit_ [ "pull", "--rebase", "--autostash" ] (Just path)
          )
          (runGit_ [ "symbolic-ref", "-q", "HEAD" ] (Just path))

      pure case result of
        Left err -> Left
          [ "Error while fetching the repo '" <> git <> "' at ref '" <> ref <> "':"
          , "  " <> err
          ]
        Right _ -> Right unit

getCleanTag :: forall a. Maybe FilePath -> Spago (GitEnv a) (Either Docc String)
getCleanTag cwd = do
  let opts = Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, cwd = cwd }
  { git } <- ask
  Cmd.exec git.cmd [ "status", "--porcelain" ] opts >>= case _ of
    Left err -> do
      pure $ Left $ toDoc [ "Could not run `git status`. Error:", err.shortMessage ]
    Right res -> do
      case res.stdout of
        "" -> do
          -- Tree is clean, get the ref
          -- TODO: once we ditch `purs publish`, we don't have a requirement for a tag anymore,
          -- but we can use any ref. We can then use `getRef` here instead of `tagCheckedOut`
          tagCheckedOut cwd
        _ -> pure $ Left $ toDoc
          [ toDoc "Git tree is not clean, aborting. Commit or stash these files:"
          , indent $ toDoc (String.split (Pattern "\n") res.stdout)
          ]

getRef :: forall a. Maybe FilePath -> Spago (GitEnv a) (Either Docc String)
getRef cwd = do
  let opts = Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, cwd = cwd }
  { git } <- ask
  Cmd.exec git.cmd [ "rev-parse", "HEAD" ] opts >>= case _ of
    Left err -> pure $ Left $ toDoc
      [ "Could not run `git rev-parse HEAD` to determine the current ref. Error:"
      , err.shortMessage
      ]
    Right res' -> pure $ Right res'.stdout

tagCheckedOut :: forall a. Maybe FilePath -> Spago (GitEnv a) (Either Docc String)
tagCheckedOut cwd = do
  let opts = Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, cwd = cwd }
  { git } <- ask
  Cmd.exec git.cmd [ "describe", "--tags" ] opts >>= case _ of
    Left err -> pure $ Left $ toDoc "The git ref currently checked out is not a tag."
    Right res' -> pure $ Right res'.stdout

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
        Left err -> pure $ Left $ toDoc
          [ "Could not push the tag 'v" <> Version.print version <> "' to the remote."
          , "Error:"
          , err.shortMessage
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
    Right { exitCode: 0 } -> pure true
    -- Git will fail with exitCode 128 if this is not a git repo or if it's dealing with a link.
    -- We ignore links - I mean, do we really want to deal with recursive links?!?
    Left { exitCode: Just 128 } -> do
      -- Sigh. Even if something is behind a link Node will not tell us that,
      -- so we need to check all the paths between the cwd and the provided path
      -- Just beautiful
      paths <- liftEffect do
        cwd <- Process.cwd
        absolutePath <- Path.resolve [] path
        FS.getInBetweenPaths cwd absolutePath
      Array.any identity <$> traverse FS.isLink paths
    -- Git will fail with 1 when a file is just, like, normally ignored
    Left { exitCode: Just 1 } -> pure false
    _ -> do
      logDebug "IsIgnored encountered an interesting exitCode"
      logDebug $ show result
      -- We still do not ignore it, just in case
      pure false

getGit :: forall a. Spago (LogEnv a) Git
getGit = do
  Cmd.exec "git" [ "--version" ] Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false } >>= case _ of
    Right r -> pure { cmd: "git", version: r.stdout }
    Left err -> do
      logDebug $ show err
      die [ "Failed to find git. Have you installed it, and is it in your PATH?" ]
