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
  , readTextFileSync
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
import Spago.Path (toRaw)
import Spago.Path as Path
import Spago.Yaml as Yaml

mkdirp :: forall m path. Path.IsPath path => MonadAff m => path -> m Unit
mkdirp path = liftAff $
  FS.Aff.mkdir' (toRaw path) { recursive: true, mode: Perms.mkPerms Perms.all Perms.all Perms.all }

foreign import moveSyncImpl :: String -> String -> Effect Unit

moveSync :: ∀ m src dst. Path.IsPath src => Path.IsPath dst => MonadEffect m => { src :: src, dst :: dst } -> m Unit
moveSync { src, dst } = liftEffect $ moveSyncImpl (toRaw src) (toRaw dst)

copyFileSync :: ∀ m src dst. Path.IsPath src => Path.IsPath dst => MonadEffect m => { src :: src, dst :: dst } -> m Unit
copyFileSync { src, dst } = liftEffect $ FS.Sync.copyFile (toRaw src) (toRaw dst)

copyFile :: ∀ m src dst. Path.IsPath src => Path.IsPath dst => MonadAff m => { src :: src, dst :: dst } -> m Unit
copyFile { src, dst } = liftAff $ FS.Aff.copyFile (toRaw src) (toRaw dst)

foreign import cpImpl :: String -> String -> Effect Unit

-- | Copy a file or directory. The directory can have contents.
-- | Note: if `src` is a directory it will copy everything inside of this directory,
-- | not the entire directory itself.
-- | Note: if `src` is a file, `dst` cannot be a directory
copyTree :: ∀ m src dst. Path.IsPath src => Path.IsPath dst => MonadEffect m => { src :: src, dst :: dst } -> m Unit
copyTree { src, dst } = liftEffect $ cpImpl (toRaw src) (toRaw dst)

foreign import ensureFileSyncImpl :: String -> Effect Unit

ensureFileSync :: forall m path. Path.IsPath path => MonadEffect m => path -> m Unit
ensureFileSync file = liftEffect $ ensureFileSyncImpl $ toRaw file

exists :: forall m path. Path.IsPath path => MonadEffect m => path -> m Boolean
exists = liftEffect <<< FS.Sync.exists <<< toRaw

unlink :: ∀ m path. Path.IsPath path => MonadAff m => path -> m Unit
unlink = liftAff <<< FS.Aff.unlink <<< toRaw

writeTextFile :: forall m path. Path.IsPath path => MonadAff m => path -> String -> m Unit
writeTextFile path text = liftAff $ FS.Aff.writeTextFile UTF8 (toRaw path) text

readTextFile :: forall m path. Path.IsPath path => MonadAff m => path -> m String
readTextFile path = liftAff $ FS.Aff.readTextFile UTF8 (toRaw path)

readTextFileSync :: ∀ m path. Path.IsPath path => MonadEffect m => path -> m String
readTextFileSync path = liftEffect $ FS.Sync.readTextFile UTF8 (toRaw path)

writeFile :: forall m path. Path.IsPath path => MonadAff m => path -> Buffer -> m Unit
writeFile path buf = liftAff $ FS.Aff.writeFile (toRaw path) buf

ls :: forall m path. Path.IsPath path => MonadAff m => path -> m (Array AdHocFilePath)
ls = liftAff <<< FS.Aff.readdir <<< toRaw

chmod :: forall m path. Path.IsPath path => MonadAff m => path -> Perms -> m Unit
chmod path perms = liftAff $ FS.Aff.chmod (toRaw path) perms

-- | Encode data as formatted JSON and write it to the provided filepath
writeJsonFile :: forall a path. Path.IsPath path => CJ.Codec a -> path -> a -> Aff Unit
writeJsonFile codec path = FS.Aff.writeTextFile UTF8 (toRaw path) <<< (_ <> "\n") <<< Json.printJson codec

-- | Decode data from a JSON file at the provided filepath
readJsonFile :: forall a path. Path.IsPath path => CJ.Codec a -> path -> Aff (Either String a)
readJsonFile codec path = do
  result <- Aff.attempt $ FS.Aff.readTextFile UTF8 (toRaw path)
  pure (lmap Aff.message result >>= Json.parseJson codec >>> lmap CJ.DecodeError.print)

-- | Encode data as formatted YAML and write it to the provided filepath
writeYamlFile :: forall a path. Path.IsPath path => CJ.Codec a -> path -> a -> Aff Unit
writeYamlFile codec path = FS.Aff.writeTextFile UTF8 (toRaw path) <<< (_ <> "\n") <<< String.trim <<< Yaml.printYaml codec

-- | Decode data from a YAML file at the provided filepath
readYamlFile :: forall a path. Path.IsPath path => CJ.Codec a -> path -> Aff (Either String a)
readYamlFile codec path = do
  result <- Aff.attempt $ FS.Aff.readTextFile UTF8 (toRaw path)
  pure (lmap Aff.message result >>= Yaml.parseYaml codec >>> lmap CJ.DecodeError.print)

writeYamlDocFile :: forall a path. Path.IsPath path => path -> Yaml.YamlDoc a -> Aff Unit
writeYamlDocFile path = FS.Aff.writeTextFile UTF8 (toRaw path) <<< (_ <> "\n") <<< String.trim <<< Yaml.toString

readYamlDocFile :: forall a path. Path.IsPath path => CJ.Codec a -> path -> Aff (Either String { doc :: Yaml.YamlDoc a, yaml :: a })
readYamlDocFile codec path = do
  result <- Aff.attempt $ FS.Aff.readTextFile UTF8 (toRaw path)
  pure (lmap Aff.message result >>= Yaml.parseYamlDoc codec >>> lmap CJ.DecodeError.print)

stat :: forall m path. Path.IsPath path => MonadAff m => path -> m (Either Error Stats)
stat path = liftAff $ try (FS.Aff.stat $ toRaw path)

isLink :: forall m path. Path.IsPath path => MonadEffect m => path -> m Boolean
isLink path = liftEffect $ try (FS.Sync.lstat $ toRaw path) >>= case _ of
  Left _err -> pure true -- TODO: we should bubble this up instead
  Right stats -> pure $ Stats.isSymbolicLink stats

foreign import getInBetweenPathsImpl :: EffectFn2 GlobalPath GlobalPath (Array GlobalPath)

getInBetweenPaths :: forall m. MonadEffect m => GlobalPath -> GlobalPath -> m (Array GlobalPath)
getInBetweenPaths a b = liftEffect $ runEffectFn2 getInBetweenPathsImpl a b
