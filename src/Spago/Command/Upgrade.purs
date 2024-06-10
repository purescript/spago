module Spago.Command.Upgrade where

import Spago.Prelude

import Registry.Version as Version
import Spago.Command.Fetch (FetchEnv)
import Spago.Config as Config
import Spago.Core.Config as Core
import Spago.Registry as Registry

run :: forall a. Spago (FetchEnv a) Unit
run = do
  { workspace } <- ask
  case workspace.workspaceConfig.packageSet of
    Just (Core.SetFromRegistry { registry: currentPackageSet }) -> do
      latestPackageSet <- Registry.findPackageSet Nothing
      case latestPackageSet <= currentPackageSet of
        true -> logSuccess "Nothing to upgrade, you already have the latest package set."
        false -> do
          logInfo $ "Upgrading the package set to the latest version: " <> Version.print latestPackageSet
          Config.setPackageSetVersionInConfig workspace.doc latestPackageSet
          logSuccess "Upgrade successful!"
    Just _ -> die "This command is not yet implemented for projects using a custom package set."
    Nothing -> die "This command is not yet implemented for projects using a solver. See https://github.com/purescript/spago/issues/1001"
