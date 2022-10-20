module Spago.Git where

import Spago.Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Spago.Cmd as Cmd
import Spago.FS as FS

runGit_ :: Array String -> Maybe FilePath -> ExceptT String Aff Unit
runGit_ args cwd = void $ runGit args cwd

runGit :: Array String -> Maybe FilePath -> ExceptT String Aff String
runGit args cwd = ExceptT do
  -- TODO which git
  result <- Cmd.exec "git" args (Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, cwd = cwd })
  pure $ bimap _.stderr _.stdout result

runGitSilent :: Array String -> Maybe FilePath -> ExceptT String Aff String
runGitSilent args cwd = ExceptT do
  -- TODO: which git
  result <- Cmd.exec "git" args (Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, cwd = cwd })
  pure $ bimap (\_ -> "Failed to run git command via runGitSilent.") _.stdout result

fetchRepo :: forall a b. { git :: String, ref :: String | a } -> FilePath -> Spago (LogEnv b) Unit
fetchRepo { git, ref } path = do
  let runE = liftAff <<< Except.runExceptT
  repoExists <- liftEffect (FS.exists path)
  cloneOrFetchResult <- case repoExists of
    true -> do
      logDebug $ "Found the " <> git <> " repo locally, pulling..."
      runE $ runGit_ [ "fetch", "origin" ] (Just path)
    false -> do
      logInfo $ "Didn't find " <> git <> " repo, cloning..."
      runE $ runGit_ [ "clone", git, path ] Nothing
  result <- runE do
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

-- | Check if the path is ignored by git
--
-- `git check-ignore` exits with 1 when path is not ignored, and 128 when
-- a fatal error occurs (i.e. when not in a git repository).
isIgnored :: forall a. FilePath -> Spago (LogEnv a) Boolean
isIgnored path = do
  let args = [ "check-ignore", "--quiet", path ]
  -- TODO which git
  result <- liftAff $ Cmd.exec "git" args (Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false })
  pure $ case result of
    Right { exitCode: 0 } -> true
    _ -> false
