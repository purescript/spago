module Spago.FS (module FS, module Spago.FS) where

import Spago.Prelude

import Node.FS.Aff as FS
import Node.FS.Perms as Perms

mkdirp :: FilePath -> Aff Unit
mkdirp = flip FS.mkdir' { recursive: true, mode: Perms.mkPerms Perms.all Perms.all Perms.all }
