module Spago.Commands.Sources where

import Spago.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Registry.PackageName (PackageName)
import Spago.Commands.Fetch (FetchEnv)
import Spago.Commands.Fetch as Fetch
import Spago.Config (Package(..), Dependencies(..))
import Spago.Config as Config

run :: forall a. Spago (FetchEnv a) Unit
run = do
  { workspace } <- ask
  -- lookup the dependencies in the package set, so we get their version numbers
  let
    selectedPackages = case workspace.selected of
      Just selected -> [ selected ]
      Nothing -> Config.getWorkspacePackages workspace.packageSet

    (Dependencies deps) = foldMap Fetch.getWorkspacePackageDeps selectedPackages

  -- TODO: here we are throwing away the ranges from the config, but we should care about them
  transitiveDeps <- Fetch.getTransitiveDeps (Set.toUnfoldable $ Map.keys deps)

  let transitivePackages = Map.union (Map.fromFoldable (map (\p -> Tuple (p.package.name) (WorkspacePackage p)) selectedPackages)) transitiveDeps

  let
    globs = Array.foldMap
      (\(Tuple packageName package) -> Config.sourceGlob packageName package)
      (Map.toUnfoldable transitivePackages :: Array (Tuple PackageName Package))

  void $ for globs \g -> output g
