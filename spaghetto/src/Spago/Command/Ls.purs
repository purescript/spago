module Spago.Command.Ls (listPackages, IncludeTransitive(..)) where

import Spago.Prelude

import Data.Map as Map
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Spago.Command.Build (BuildEnv)
import Spago.Command.Registry (RegistryEnv)
import Spago.Config (Package)

data IncludeTransitive = IncludeTransitive | NoIncludeTransitive

type Env = { dependencies :: Map PackageName Package }

listPackages :: forall a. IncludeTransitive -> String -> Spago Env Unit
listPackages includeTransitive jsonFlag = do
  -- logDebug "Running `listPackages`"
  { dependencies } <- ask
  let packagesToList = Map.toUnfoldable dependencies
  case packagesToList of
    -- [] -> logWarn "There are no dependencies listed in your spago.dhall"
    [] -> pure unit
    _ -> output $ OutputLines $ formatPackageNames jsonFlag packagesToList

formatPackageNames :: String -> Array (PackageName /\ Package) -> Array String
formatPackageNames jsonFlag packagesToList = render <$> packagesToList
  where
  render (packageName /\ _) = PackageName.print packageName

-- listPackages :: forall a. IncludeTransitive -> String -> Spago (RegistryEnv a) Unit
-- listPackages packagesFilter jsonFlag = do
--   logDebug "Running `listPackages`"
--   packagesToList :: List (PackageName /\ Package) <- case packagesFilter of
--     IncludeTransitive -> Packages.getProjectDeps
--     _ -> do
--       { workspace } <- ask

--       let packagesDB = config.packageSet.packagesDB
--       let dependencies = config.dependencies
--       pure $ Map.toList $ Map.restrictKeys packagesDB dependencies

--   case packagesToList of
--     [] -> logWarn "There are no dependencies listed in your spago.dhall"
--     _ -> traverse_ output $ formatPackageNames jsonFlag packagesToList
