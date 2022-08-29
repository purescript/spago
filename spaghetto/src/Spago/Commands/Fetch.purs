module Spago.Commands.Fetch where

import Spago.Prelude

import Registry.Index as Index
import Registry.PackageName (PackageName)

type FetchEnv a =
  { registryIndex :: Index.RegistryIndex
  | a
  }

run :: forall a. Array PackageName -> Spago (FetchEnv a) Unit
run packages = do
  logShow packages
