module Spago.FS
  ( chmod
  , copyFile
  , copyFileSync
  , copyTree
  , unlink
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

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Codec.JSON as CJ
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

copyFileSync :: forall m. MonadEffect m => { src :: FilePath, dst :: FilePath } -> m Unit
copyFileSync { src, dst } = liftEffect $ FS.Sync.copyFile src dst

copyFile :: forall m. MonadAff m => { src :: FilePath, dst :: FilePath } -> m Unit
copyFile { src, dst } = liftAff $ FS.Aff.copyFile src dst

foreign import cpImpl :: String -> String -> Effect Unit

-- | Copy a file or directory. The directory can have contents.
-- | Note: if `src` is a directory it will copy everything inside of this directory,
-- | not the entire directory itself.
-- | Note: if `src` is a file, `dst` cannot be a directory
copyTree :: forall m. MonadEffect m => { src :: FilePath, dst :: FilePath } -> m Unit
copyTree { src, dst } = liftEffect $ cpImpl src dst

foreign import ensureFileSyncImpl :: String -> Effect Unit

ensureFileSync :: forall m. MonadEffect m => FilePath -> m Unit
ensureFileSync file = liftEffect $ ensureFileSyncImpl file

exists :: forall m. MonadEffect m => String -> m Boolean
exists = liftEffect <<< FS.Sync.exists

unlink :: ∀ m. MonadAff m => String -> m Unit
unlink = liftAff <<< FS.Aff.unlink

writeTextFile :: forall m. MonadAff m => FilePath -> String -> m Unit
writeTextFile path text = liftAff $ FS.Aff.writeTextFile UTF8 path text

readTextFile :: forall m. MonadAff m => FilePath -> m String
readTextFile path = liftAff $ FS.Aff.readTextFile UTF8 path

writeFile :: forall m. MonadAff m => FilePath -> Buffer -> m Unit
writeFile path buf = liftAff $ FS.Aff.writeFile path buf

ls :: forall m. MonadAff m => FilePath -> m (Array FilePath)
ls = liftAff <<< FS.Aff.readdir

chmod :: forall m. MonadAff m => FilePath -> Perms -> m Unit
chmod path perms = liftAff $ FS.Aff.chmod path perms

-- | Encode data as formatted JSON and write it to the provided filepath
writeJsonFile :: forall a. CJ.Codec a -> FilePath -> a -> Aff Unit
writeJsonFile codec path = FS.Aff.writeTextFile UTF8 path <<< (_ <> "\n") <<< Json.printJson codec

-- | Decode data from a JSON file at the provided filepath
readJsonFile :: forall a. CJ.Codec a -> FilePath -> Aff (Either String a)
readJsonFile codec path = do
  result <- Aff.attempt $ FS.Aff.readTextFile UTF8 path
  pure (lmap Aff.message result >>= Json.parseJson codec >>> lmap CJ.DecodeError.print)

-- | Encode data as formatted YAML and write it to the provided filepath
writeYamlFile :: forall a. CJ.Codec a -> FilePath -> a -> Aff Unit
writeYamlFile codec path = FS.Aff.writeTextFile UTF8 path <<< (_ <> "\n") <<< String.trim <<< Yaml.printYaml codec

-- | Decode data from a YAML file at the provided filepath
readYamlFile :: forall a. CJ.Codec a -> FilePath -> Aff (Either String a)
readYamlFile codec path = do
  result <- Aff.attempt $ FS.Aff.readTextFile UTF8 path
  pure (lmap Aff.message result >>= Yaml.parseYaml codec >>> lmap CJ.DecodeError.print)

writeYamlDocFile :: forall a. FilePath -> Yaml.YamlDoc a -> Aff Unit
writeYamlDocFile path = FS.Aff.writeTextFile UTF8 path <<< (_ <> "\n") <<< String.trim <<< Yaml.toString

readYamlDocFile :: forall a. CJ.Codec a -> FilePath -> Aff (Either String { doc :: Yaml.YamlDoc a, yaml :: a })
readYamlDocFile codec path = do
  result <- Aff.attempt $ FS.Aff.readTextFile UTF8 path
  pure (lmap Aff.message result >>= Yaml.parseYamlDoc codec >>> lmap CJ.DecodeError.print)

stat :: forall m. MonadAff m => FilePath -> m (Either Error Stats)
stat path = liftAff $ try (FS.Aff.stat path)

isLink :: forall m. MonadEffect m => FilePath -> m Boolean
isLink path = liftEffect $ try (FS.Sync.lstat path) >>= case _ of
  Left _err -> pure true -- TODO: we should bubble this up instead
  Right stats -> pure $ Stats.isSymbolicLink stats

foreign import getInBetweenPathsImpl :: EffectFn2 FilePath FilePath (Array FilePath)

getInBetweenPaths :: forall m. MonadEffect m => FilePath -> FilePath -> m (Array FilePath)
getInBetweenPaths a b = liftEffect $ runEffectFn2 getInBetweenPathsImpl a b
