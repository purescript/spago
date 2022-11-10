module Spago.FS
  ( chmod
  , exists
  , ls
  , mkdirp
  , moveSync
  , readTextFile
  , writeFile
  , writeTextFile
  ) where

import Spago.Prelude

import Node.FS.Aff as FS.Aff
import Node.FS.Perms (Perms)
import Node.FS.Perms as Perms
import Node.FS.Sync as FS.Sync

mkdirp :: forall m. MonadAff m => FilePath -> m Unit
mkdirp = liftAff <<< flip FS.Aff.mkdir' { recursive: true, mode: Perms.mkPerms Perms.all Perms.all Perms.all }

foreign import moveSyncImpl :: String -> String -> Effect Unit

moveSync :: forall m. MonadEffect m => { src :: FilePath, dst :: FilePath } -> m Unit
moveSync { src, dst } = liftEffect $ moveSyncImpl src dst

exists :: forall m. MonadEffect m => String -> m Boolean
exists = liftEffect <<< FS.Sync.exists

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
