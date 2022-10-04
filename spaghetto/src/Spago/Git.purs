module Spago.Git where

import Spago.Prelude

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Data.String as String
import Node.ChildProcess as NodeProcess
import Node.FS.Sync as FS.Sync
import Sunde as Process

runGit_ :: Array String -> Maybe FilePath -> ExceptT String Aff Unit
runGit_ args cwd = void $ runGit args cwd

runGit :: Array String -> Maybe FilePath -> ExceptT String Aff String
runGit args cwd = ExceptT do
  result <- Process.spawn { cmd: "git", args, stdin: Nothing } (NodeProcess.defaultSpawnOptions { cwd = cwd })
  let stdout = String.trim result.stdout
  let stderr = String.trim result.stderr
  case result.exit of
    NodeProcess.Normally 0 -> do
      pure $ Right stdout
    _ -> do
      pure $ Left stderr

runGitSilent :: Array String -> Maybe FilePath -> ExceptT String Aff String
runGitSilent args cwd = ExceptT do
  result <- Process.spawn { cmd: "git", args, stdin: Nothing } (NodeProcess.defaultSpawnOptions { cwd = cwd })
  case result.exit of
    NodeProcess.Normally 0 -> do
      let stdout = String.trim result.stdout
      pure $ Right stdout
    _ -> pure $ Left $ "Failed to run git command via runGitSilent."

fetchRepo :: forall a b. { git :: String, ref :: String | a } -> FilePath -> Spago (LogEnv b) Unit
fetchRepo { git, ref } path = do
  let runE = liftAff <<< Except.runExceptT
  repoExists <- liftEffect (FS.Sync.exists path)
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
