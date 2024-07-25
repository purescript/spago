module Spago.Command.Upgrade where

import Spago.Prelude

import Registry.Version as Version
import Spago.Command.Fetch (FetchEnv)
import Spago.Config as Config
import Spago.Core.Config as Core
import Spago.Registry as Registry

type UpgradeArgs =
  { setVersion :: Maybe Version
  }

run :: ∀ a. UpgradeArgs -> Spago (FetchEnv a) Unit
run args = do
  { workspace } <- ask
  case workspace.workspaceConfig.packageSet of
    Just (Core.SetFromRegistry { registry: currentPackageSet }) -> do
      latestPackageSet <- Registry.findPackageSet args.setVersion

      let whichVersion = args.setVersion # maybe "latest" (const "specified")

      case latestPackageSet <= currentPackageSet of
        true -> logSuccess $ "Nothing to upgrade, you already have the " <> whichVersion <> " package set."
        false -> do
          logInfo $ "Upgrading the package set to the " <> whichVersion <> " version: " <> Version.print latestPackageSet
          Config.setPackageSetVersionInConfig workspace.doc latestPackageSet
          logSuccess "Upgrade successful!"
    Just _ -> die "This command is not yet implemented for projects using a custom package set."
    Nothing -> die "This command is not yet implemented for projects using a solver. See https://github.com/purescript/spago/issues/1001"
