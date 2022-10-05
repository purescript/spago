module Spago.FS
  ( module FS
  , mkdirp
  , moveSync
  ) where

import Spago.Prelude

import Node.FS.Aff (writeTextFile, readTextFile, writeFile, mkdir') as FS
import Node.FS.Sync (exists) as FS
import Node.FS.Perms as Perms

mkdirp :: FilePath -> Aff Unit
mkdirp = flip FS.mkdir' { recursive: true, mode: Perms.mkPerms Perms.all Perms.all Perms.all }

foreign import moveSyncImpl :: String -> String -> Effect Unit

moveSync :: { src :: FilePath, dst :: FilePath } -> Effect Unit
moveSync { src, dst } = moveSyncImpl src dst
