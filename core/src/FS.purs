module Spago.FS
  ( chmod
  , ensureFileSync
  , exists
  , getInBetweenPaths
  , isLink
  , ls
  , mkdirp
  , moveSync
  , readJsonFile
  , readTextFile
  , readYamlDocFile
  , readYamlFile
  , stat
  , writeFile
  , writeJsonFile
  , writeTextFile
  , writeYamlDocFile
  , writeYamlFile
  ) where

import Spago.Core.Prelude

import Data.Codec.Argonaut as CA
import Data.String as String
import Effect.Aff as Aff
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Node.FS.Aff as FS.Aff
import Node.FS.Perms (Perms)
import Node.FS.Perms as Perms
import Node.FS.Stats (Stats)
import Node.FS.Stats as Stats
import Node.FS.Sync as FS.Sync
import Spago.Json as Json
import Spago.Yaml as Yaml

mkdirp :: forall m. MonadAff m => FilePath -> m Unit
mkdirp = liftAff <<< flip FS.Aff.mkdir' { recursive: true, mode: Perms.mkPerms Perms.all Perms.all Perms.all }

foreign import moveSyncImpl :: String -> String -> Effect Unit

moveSync :: forall m. MonadEffect m => { src :: FilePath, dst :: FilePath } -> m Unit
moveSync { src, dst } = liftEffect $ moveSyncImpl src dst

foreign import ensureFileSyncImpl :: String -> Effect Unit

ensureFileSync :: forall m. MonadEffect m => FilePath -> m Unit
ensureFileSync file = liftEffect $ ensureFileSyncImpl file

exists :: forall m. MonadEffect m => String -> m Boolean
exists = liftEffect <<< FS.Sync.exists

writeTextFile :: forall m. MonadEffect m => FilePath -> String -> m Unit
writeTextFile path text = liftEffect $ FS.Sync.writeTextFile UTF8 path text

readTextFile :: forall m. MonadAff m => FilePath -> m String
readTextFile path = liftAff $ FS.Aff.readTextFile UTF8 path

writeFile :: forall m. MonadAff m => FilePath -> Buffer -> m Unit
writeFile path buf = liftAff $ FS.Aff.writeFile path buf

ls :: forall m. MonadAff m => FilePath -> m (Array FilePath)
ls = liftAff <<< FS.Aff.readdir

chmod :: forall m. MonadAff m => FilePath -> Perms -> m Unit
chmod path perms = liftAff $ FS.Aff.chmod path perms

-- | Encode data as formatted JSON and write it to the provided filepath
writeJsonFile :: forall a. JsonCodec a -> FilePath -> a -> Aff Unit
writeJsonFile codec path = FS.Aff.writeTextFile UTF8 path <<< (_ <> "\n") <<< Json.printJson codec

-- | Decode data from a JSON file at the provided filepath
readJsonFile :: forall a. JsonCodec a -> FilePath -> Aff (Either String a)
readJsonFile codec path = do
  result <- Aff.attempt $ FS.Aff.readTextFile UTF8 path
  pure (lmap Aff.message result >>= Json.parseJson codec >>> lmap CA.printJsonDecodeError)

-- | Encode data as formatted YAML and write it to the provided filepath
writeYamlFile :: forall a. JsonCodec a -> FilePath -> a -> Aff Unit
writeYamlFile codec path = FS.Aff.writeTextFile UTF8 path <<< (_ <> "\n") <<< String.trim <<< Yaml.printYaml codec

-- | Decode data from a YAML file at the provided filepath
readYamlFile :: forall a. JsonCodec a -> FilePath -> Aff (Either String a)
readYamlFile codec path = do
  result <- Aff.attempt $ FS.Aff.readTextFile UTF8 path
  pure (lmap Aff.message result >>= Yaml.parseYaml codec >>> lmap CA.printJsonDecodeError)

writeYamlDocFile :: forall a. FilePath -> Yaml.YamlDoc a -> Aff Unit
writeYamlDocFile path = FS.Aff.writeTextFile UTF8 path <<< (_ <> "\n") <<< String.trim <<< Yaml.toString

readYamlDocFile :: forall a. JsonCodec a -> FilePath -> Aff (Either String { doc :: Yaml.YamlDoc a, yaml :: a })
readYamlDocFile codec path = do
  result <- Aff.attempt $ FS.Aff.readTextFile UTF8 path
  pure (lmap Aff.message result >>= Yaml.parseYamlDoc codec >>> lmap CA.printJsonDecodeError)

stat :: forall m. MonadAff m => FilePath -> m (Either Error Stats)
stat path = liftAff $ try (FS.Aff.stat path)

isLink :: forall m. MonadEffect m => FilePath -> m Boolean
isLink path = liftEffect $ try (FS.Sync.lstat path) >>= case _ of
  Left _err -> pure true -- TODO: we should bubble this up instead
  Right stats -> pure $ Stats.isSymbolicLink stats

foreign import getInBetweenPathsImpl :: EffectFn2 FilePath FilePath (Array FilePath)

getInBetweenPaths :: forall m. MonadEffect m => FilePath -> FilePath -> m (Array FilePath)
getInBetweenPaths a b = liftEffect $ runEffectFn2 getInBetweenPathsImpl a b
