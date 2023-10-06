module Spago.Command.Upgrade where

import Spago.Prelude

import Registry.Version as Version
import Spago.Command.Fetch (FetchEnv)
import Spago.Config as Config
import Spago.Core.Config as Core
import Spago.Db as Db

run :: forall a. Spago (FetchEnv a) Unit
run = do
  { workspace, db, purs } <- ask
  case workspace.workspaceConfig.package_set of
    Just (Core.SetFromRegistry { registry }) -> do
      maybeLatestPackageSet <- liftEffect $ Db.selectLatestPackageSetByCompiler db purs.version
      case maybeLatestPackageSet of
        Nothing -> die "No package set found for the current compiler version."
        Just latestPackageSet
          | latestPackageSet.version <= registry -> logSuccess "Nothing to upgrade, you already have the latest package set."
          | otherwise -> do
              logInfo $ "Upgrading the package set to the latest version: " <> Version.print latestPackageSet.version
              Config.setPackageSetVersionInConfig workspace.doc latestPackageSet.version
              logSuccess "Upgrade successful!"
    Just _ -> die "This command is not yet implemented for projects using a custom package set."
    Nothing -> die "This command is not yet implemented for projects using a solver. See https://github.com/purescript/spago/issues/1001"
