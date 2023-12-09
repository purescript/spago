module Spago.Repl
  ( supportPackage
  ) where

import Spago.Prelude

import Data.Map as Map
import Registry.PackageName as PackageName
import Spago.Config (BuildType(..), Package(..), PackageMap, PackageSet)
import Spago.Registry (RegistryEnv)

-- TODO I guess this should be configurable
supportPackageName :: PackageName
supportPackageName = unsafeFromRight $ PackageName.parse "psci-support"

supportPackage :: forall a. PackageSet -> Spago (RegistryEnv a) PackageMap
supportPackage packageSet = do
  { getMetadata, logOptions } <- ask
  case packageSet.buildType of
    PackageSetBuild _info packages -> pure $ Map.filterWithKey (\k _v -> k == supportPackageName) packages
    -- TODO: we should look in the "other" packages first
    RegistrySolverBuild _other -> do
      maybeMetadata <- runSpago { logOptions } (getMetadata supportPackageName)
      pure case maybeMetadata of
        Right (Metadata metadata) -> case Map.findMax metadata.published of
          Nothing -> Map.empty
          Just { key } -> Map.singleton supportPackageName (RegistryVersion key)
        Left _err -> Map.empty
