module Spago.Git
  ( Git
  , GitEnv
  , fetchRepo
  , getGit
  , getRef
  , isIgnored
  ) where

import Spago.Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Data.String (Pattern(..))
import Data.String as String
import Spago.Cmd as Cmd
import Spago.FS as FS

type Git = { cmd :: String, version :: String }

type GitEnv a = { git :: Git, logOptions :: LogOptions | a }

runGit_ :: forall a. Array String -> Maybe FilePath -> ExceptT String (Spago (GitEnv a)) Unit
runGit_ args cwd = void $ runGit args cwd

runGit :: forall a. Array String -> Maybe FilePath -> ExceptT String (Spago (GitEnv a)) String
runGit args cwd = ExceptT do
  { git } <- ask
  result <- Cmd.exec git.cmd args (Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, cwd = cwd })
  pure $ bimap _.stderr _.stdout result

fetchRepo :: forall a b. { git :: String, ref :: String | a } -> FilePath -> Spago (GitEnv b) Unit
fetchRepo { git, ref } path = do
  repoExists <- FS.exists path
  cloneOrFetchResult <- case repoExists of
    true -> do
      logDebug $ "Found " <> git <> " locally, pulling..."
      Except.runExceptT $ runGit_ [ "fetch", "origin" ] (Just path)
    false -> do
      logInfo $ "Cloning " <> git
      Except.runExceptT $ runGit_ [ "clone", git, path ] Nothing
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

  case result of
    Left err -> throwError err
    Right _ -> pure unit

getRef :: forall a. Spago (GitEnv a) (Either Docc String)
getRef = do
  let opts = Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false }
  { git } <- ask
  Cmd.exec git.cmd [ "status", "--porcelain" ] opts >>= case _ of
    Left err -> do
      pure $ Left $ toDoc [ "Could not run `git status`. Error:", err.shortMessage ]
    Right res -> do
      case res.stdout of
        "" -> do
          -- Tree is clean, get the ref
          Cmd.exec git.cmd [ "rev-parse", "HEAD" ] opts >>= case _ of
            Left err -> pure $ Left $ toDoc
              [ "Could not run `git rev-parse HEAD` to determine the current ref. Error:"
              , err.shortMessage
              ]
            Right res' -> pure $ Right res'.stdout
        _ -> pure $ Left $ toDoc
          [ toDoc "Git tree is not clean, aborting. Commit or stash these files:"
          , indent $ toDoc (String.split (Pattern "\n") res.stdout)
          ]

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
    -- Git will fail with exitCode 128 if dealing with a link.
    -- We ignore those! I mean, do we really want to deal with recursive links?!?
    Left { exitCode: 128 } -> pure true
    -- Git will fail with 1 when a file is just, like, normally ignored
    Left { exitCode: 1 } -> pure false
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
