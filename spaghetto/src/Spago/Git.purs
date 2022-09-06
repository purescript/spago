module Spago.Git where

import Spago.Prelude

import Control.Monad.Except as Except
import Effect.Aff as Aff
import Foreign.Git as Registry
import Node.FS.Sync as FS.Sync

-- TODO: port the rest of the git stuff here from the registry

-- TODO: refactor this thing. Like there should be a common path after the checkout
fetchRepo :: forall a. { git :: String, ref :: String | a } -> FilePath -> Aff Unit
fetchRepo { git, ref } path = liftEffect (FS.Sync.exists path) >>= case _ of
  true -> do
    log $ "Found the " <> git <> " repo locally, pulling..."
    result <- Except.runExceptT do
      Registry.runGit_ [ "fetch", "origin" ] (Just path)
      _ <- Registry.runGit [ "checkout", ref ] (Just path)
      -- if we are on a branch and not on a detached head, then we need to pull
      -- the following command will fail if on a detached head, and succeed if on a branch
      Except.mapExceptT
        ( \a -> a >>= case _ of
            Left _err -> pure (Right unit)
            Right _ -> Except.runExceptT $ Registry.runGit_ [ "pull", "--rebase", "--autostash" ] (Just path)
        )
        (Registry.runGit_ [ "symbolic-ref", "-q", "HEAD" ] (Just path))
    case result of
      Left err -> Aff.throwError $ Aff.error err
      Right _ -> pure unit
  _ -> do
    log $ "Didn't find " <> git <> " repo, cloning..."
    result <- Except.runExceptT do
      _ <- Registry.runGit [ "clone", git, path ] Nothing
      Registry.runGit [ "checkout", ref ] (Just path)
    case result of
      Left err -> Aff.throwError $ Aff.error err
      Right _ -> pure unit