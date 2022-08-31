module Spago.Commands.Fetch where

import Spago.Prelude

import Control.Monad.Error.Class (try)
import Effect.Class (liftEffect)
import Node.Path as Path
import Node.Process as Process
import Registry.API as Registry.API
import Registry.Index as Registry
import Registry.PackageName (PackageName)
import Spago.Config (Config)
import Spago.Config as Config
import Spago.FS as FS
import Yaml as Yaml

type FetchEnv a =
  { registryIndex :: Registry.RegistryIndex
  , globalCachePath :: FilePath
  , localCachePath :: FilePath
  | a
  }

run :: forall a. Array PackageName -> Spago (FetchEnv a) Unit
run packages = do
  logShow packages
  -- make a cache dir
  globalCachePath <- asks _.globalCachePath
  localCachePath <- asks _.localCachePath
  log $ "Global cache: " <> show globalCachePath
  log $ "Local cache: " <> show localCachePath
  liftAff do
    -- clone the registry and index repo, or update them
    try (Registry.API.fetchRepo { owner: "purescript", repo: "registry-index" } (Path.concat [ globalCachePath, "registry-index" ])) >>= case _ of
      Right _ -> pure unit
      Left _err -> do
        log "Couldn't refresh the registry-index, will proceed anyways"
    try (Registry.API.fetchRepo { owner: "purescript", repo: "registry-preview" } (Path.concat [ globalCachePath, "registry" ])) >>= case _ of
      Right _ -> pure unit
      Left _err -> do
        log "Couldn't refresh the registry, will proceed anyways"
  -- read the config
  log "Reading config.."
  eitherConfig :: Either String Config <- liftAff $ Yaml.readYamlFile "./spago.yaml"
  log "Config read"
  case eitherConfig of
    Left err -> do
      log $ "Can't read config: " <> err -- TODO: better error here
      liftEffect $ Process.exit 1
    Right conf -> do
      log "Read config:"
      log (Yaml.printYaml conf)
-- TODO: lookup the packages in the set
-- TODO: then look them up in the index
-- TODO: then download the tars
-- TODO: probably with a process pool to limit concurrency
-- TODO: then check the shas against the metadata
-- TODO: then stash them in the global cache
-- TODO: then decompress them in the local one