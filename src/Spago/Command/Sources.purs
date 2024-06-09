module Spago.Command.Sources where

import Spago.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Codec.JSON as CJ
import Data.Map as Map
import Spago.Command.Fetch (FetchEnv)
import Spago.Command.Fetch as Fetch
import Spago.Config (Package(..), WithTestGlobs(..))
import Spago.Config as Config

type SourcesOpts = { json :: Boolean }

run :: forall a. SourcesOpts -> Spago (FetchEnv a) Unit
run { json } = do
  { workspace } <- ask
  -- lookup the dependencies in the package set, so we get their version numbers
  let
    selectedPackages = case workspace.selected of
      Just selected -> NEA.singleton selected
      Nothing -> Config.getWorkspacePackages workspace.packageSet

  transitiveDeps <- traverse Fetch.getTransitiveDeps
    $ Map.fromFoldable
    $ map (\p -> Tuple p.package.name p) selectedPackages

  let transitivePackages = Map.union (Map.fromFoldable (map (\p -> Tuple (p.package.name) (WorkspacePackage p)) selectedPackages)) (Fetch.toAllDependencies transitiveDeps)

  let
    globs = Array.foldMap
      (\(Tuple packageName package) -> Config.sourceGlob WithTestGlobs packageName package)
      (Map.toUnfoldable transitivePackages :: Array (Tuple PackageName Package))

  output case json of
    true -> OutputJson (CJ.array CJ.string) globs
    false -> OutputLines globs
