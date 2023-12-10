module Spago.Repl
  ( supportPackage
  ) where

import Spago.Prelude

import Data.Map as Map
import Registry.PackageName as PackageName
import Spago.Config (BuildType(..), Package(..), PackageMap, PackageSet)
import Spago.Registry (RegistryEnv)
import Spago.Registry as Registry

-- TODO I guess this should be configurable
supportPackageName :: PackageName
supportPackageName = unsafeFromRight $ PackageName.parse "psci-support"

supportPackage :: PackageSet -> Spago (RegistryEnv _) PackageMap
supportPackage packageSet = do
  case packageSet.buildType of
    PackageSetBuild _info packages -> pure $ Map.filterWithKey (\k _v -> k == supportPackageName) packages
    -- TODO: we should look in the "other" packages first
    RegistrySolverBuild _other -> do
      maybeMetadata <- Registry.getMetadata supportPackageName
      pure case maybeMetadata of
        Right (Metadata metadata) -> case Map.findMax metadata.published of
          Nothing -> Map.empty
          Just { key } -> Map.singleton supportPackageName (RegistryVersion key)
        Left _err -> Map.empty
