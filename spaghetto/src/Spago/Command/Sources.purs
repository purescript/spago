module Spago.Command.Sources where

import Spago.Prelude

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Map as Map
import Data.Set as Set
import Spago.Command.Fetch (FetchEnv)
import Spago.Command.Fetch as Fetch
import Spago.Config (Dependencies(..), Package(..), WithTestGlobs(..))
import Spago.Config as Config

type SourcesOpts = { json :: Boolean }

run :: forall a. SourcesOpts -> Spago (FetchEnv a) Unit
run { json } = do
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
      (\(Tuple packageName package) -> Config.sourceGlob WithTestGlobs packageName package)
      (Map.toUnfoldable transitivePackages :: Array (Tuple PackageName Package))

  output case json of
    true -> OutputJson (CA.array CA.string) globs
    false -> OutputLines globs
