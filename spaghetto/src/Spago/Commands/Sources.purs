module Spago.Commands.Sources where

import Spago.Prelude

import Data.Map as Map
import Data.Set as Set
import Spago.Commands.Fetch (FetchEnv)
import Spago.Commands.Fetch as Fetch
import Spago.Config (Package(..), Dependencies(..))
import Spago.Config as Config
import Registry.PackageName (PackageName)

run :: forall a. Spago (FetchEnv a) Unit
run = do
  { workspace } <- ask
  -- lookup the dependencies in the package set, so we get their version numbers
  let
    selectedPackages = case workspace.selected of
      Just selected -> [ selected ]
      Nothing -> Config.getWorkspacePackages workspace.packageSet

    (Dependencies deps) = foldMap _.package.dependencies selectedPackages

  -- TODO: here we are throwing away the ranges from the config, but we should care about them
  transitiveDeps <- Fetch.getTransitiveDeps (Set.toUnfoldable $ Map.keys deps)

  let transitivePackages = Map.union (Map.fromFoldable (map (\p -> Tuple (p.package.name) (WorkspacePackage p)) selectedPackages)) transitiveDeps

  void $ for (Map.toUnfoldable transitivePackages :: Array (Tuple PackageName Package))
    \(Tuple packageName package) -> do
      output (Config.sourceGlob packageName package)
