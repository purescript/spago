module Node.FS.Sync
  ( access
  , access'
  , copyFile
  , copyFile'
  , mkdtemp
  , mkdtemp'
  , rename
  , truncate
  , chown
  , chmod
  , stat
  , lstat
  , link
  , symlink
  , readlink
  , realpath
  , realpath'
  , unlink
  , rmdir
  , rmdir'
  , rm
  , rm'
  , mkdir
  , mkdir'
  , readdir
  , utimes
  , readFile
  , readTextFile
  , writeFile
  , writeTextFile
  , appendFile
  , appendTextFile
  , exists
  , fdOpen
  , fdRead
  , fdNext
  , fdWrite
  , fdAppend
  , fdFlush
  , fdClose
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Either (blush)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Exception (Error, try)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn5, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn5)
import Node.Buffer (Buffer, size)
import Node.Encoding (Encoding(..), encodingToNode)
import Node.FS (FileDescriptor, ByteCount, FilePosition, BufferLength, BufferOffset, FileMode, SymlinkType, symlinkTypeToNode)
import Node.FS.Constants (AccessMode, CopyMode, FileFlags, defaultAccessMode, defaultCopyMode, fileFlagsToNode)
import Node.FS.Perms (Perms, permsToString, all, mkPerms)
import Node.FS.Stats (StatsObj, Stats(..))
import Node.Path (FilePath)

access :: FilePath -> Effect (Maybe Error)
access = flip access' defaultAccessMode

access' :: FilePath -> AccessMode -> Effect (Maybe Error)
access' path mode = do
  map blush $ try $ runEffectFn2 accessImpl path mode

foreign import accessImpl :: EffectFn2 FilePath AccessMode (Maybe Error)

copyFile :: FilePath -> FilePath -> Effect Unit
copyFile src dest = runEffectFn3 copyFileImpl src dest defaultCopyMode

copyFile' :: FilePath -> FilePath -> CopyMode -> Effect Unit
copyFile' src dest mode = runEffectFn3 copyFileImpl src dest mode

foreign import copyFileImpl :: EffectFn3 FilePath FilePath CopyMode Unit

mkdtemp :: String -> Effect String
mkdtemp prefix = mkdtemp' prefix UTF8

mkdtemp' :: String -> Encoding -> Effect String
mkdtemp' prefix encoding = runEffectFn2 mkdtempImpl prefix (encodingToNode encoding)

foreign import mkdtempImpl :: EffectFn2 String String String

foreign import renameSyncImpl :: EffectFn2 FilePath FilePath Unit
foreign import truncateSyncImpl :: EffectFn2 FilePath Int Unit
foreign import chownSyncImpl :: EffectFn3 FilePath Int Int Unit
foreign import chmodSyncImpl :: EffectFn2 FilePath String Unit
foreign import statSyncImpl :: EffectFn1 FilePath StatsObj
foreign import lstatSyncImpl :: EffectFn1 FilePath StatsObj
foreign import linkSyncImpl :: EffectFn2 FilePath FilePath Unit
foreign import symlinkSyncImpl :: EffectFn3 FilePath FilePath String Unit
foreign import readlinkSyncImpl :: EffectFn1 FilePath FilePath
foreign import realpathSyncImpl :: forall cache. EffectFn2 FilePath { | cache } FilePath
foreign import unlinkSyncImpl :: EffectFn1 FilePath Unit
foreign import rmdirSyncImpl :: EffectFn2 FilePath { maxRetries :: Int, retryDelay :: Int } Unit
foreign import rmSyncImpl :: EffectFn2 FilePath { force :: Boolean, maxRetries :: Int, recursive :: Boolean, retryDelay :: Int } Unit
foreign import mkdirSyncImpl :: EffectFn2 FilePath { recursive :: Boolean, mode :: String } Unit
foreign import readdirSyncImpl :: EffectFn1 FilePath (Array FilePath)
foreign import utimesSyncImpl :: EffectFn3 FilePath Int Int Unit
foreign import readFileSyncImpl :: forall a opts. EffectFn2 FilePath { | opts } a
foreign import writeFileSyncImpl :: forall a opts. EffectFn3 FilePath a { | opts } Unit
foreign import appendFileSyncImpl :: forall a opts. EffectFn3 FilePath a { | opts } Unit
foreign import existsSyncImpl :: EffectFn1 FilePath Boolean
foreign import openSyncImpl :: EffectFn3 FilePath String (Nullable FileMode) FileDescriptor
foreign import readSyncImpl :: EffectFn5 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) ByteCount
foreign import writeSyncImpl :: EffectFn5 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) ByteCount
foreign import fsyncSyncImpl :: EffectFn1 FileDescriptor Unit
foreign import closeSyncImpl :: EffectFn1 FileDescriptor Unit

-- | Renames a file.
rename :: FilePath -> FilePath -> Effect Unit
rename oldFile newFile = runEffectFn2 renameSyncImpl oldFile newFile

-- | Truncates a file to the specified length.
truncate
  :: FilePath
  -> Int
  -> Effect Unit
truncate file len = runEffectFn2 truncateSyncImpl file len

-- | Changes the ownership of a file.
chown
  :: FilePath
  -> Int
  -> Int
  -> Effect Unit
chown file uid gid = runEffectFn3 chownSyncImpl file uid gid

-- | Changes the permissions of a file.
chmod
  :: FilePath
  -> Perms
  -> Effect Unit
chmod file perms = runEffectFn2 chmodSyncImpl file (permsToString perms)

-- | Gets file statistics.
stat
  :: FilePath
  -> Effect Stats
stat file = map Stats $ runEffectFn1 statSyncImpl file

-- | Gets file or symlink statistics. `lstat` is identical to `stat`, except
-- | that if the `FilePath` is a symbolic link, then the link itself is stat-ed,
-- | not the file that it refers to.
lstat
  :: FilePath
  -> Effect Stats
lstat file = map Stats $ runEffectFn1 lstatSyncImpl file

-- | Creates a link to an existing file.
link
  :: FilePath
  -> FilePath
  -> Effect Unit
link src dst = runEffectFn2 linkSyncImpl src dst

-- | Creates a symlink.
symlink
  :: FilePath
  -> FilePath
  -> SymlinkType
  -> Effect Unit
symlink src dst ty = runEffectFn3 symlinkSyncImpl src dst (symlinkTypeToNode ty)

-- | Reads the value of a symlink.
readlink
  :: FilePath
  -> Effect FilePath
readlink path = runEffectFn1 readlinkSyncImpl path

-- | Find the canonicalized absolute location for a path.
realpath
  :: FilePath
  -> Effect FilePath
realpath path = runEffectFn2 realpathSyncImpl path {}

-- | Find the canonicalized absolute location for a path using a cache object for
-- | already resolved paths.
realpath'
  :: forall cache
   . FilePath
  -> { | cache }
  -> Effect FilePath
realpath' path cache = runEffectFn2 realpathSyncImpl path cache

-- | Deletes a file.
unlink
  :: FilePath
  -> Effect Unit
unlink file = runEffectFn1 unlinkSyncImpl file

-- | Deletes a directory.
rmdir
  :: FilePath
  -> Effect Unit
rmdir path = rmdir' path { maxRetries: 0, retryDelay: 100 }

-- | Deletes a directory with options.
rmdir'
  :: FilePath
  -> { maxRetries :: Int, retryDelay :: Int }
  -> Effect Unit
rmdir' path opts = runEffectFn2 rmdirSyncImpl path opts

-- | Deletes a file or directory.
rm
  :: FilePath
  -> Effect Unit
rm path = rm' path { force: false, maxRetries: 100, recursive: false, retryDelay: 1000 }

-- | Deletes a file or directory with options.
rm'
  :: FilePath
  -> { force :: Boolean, maxRetries :: Int, recursive :: Boolean, retryDelay :: Int }
  -> Effect Unit
rm' path opts = runEffectFn2 rmSyncImpl path opts

-- | Makes a new directory.
mkdir
  :: FilePath
  -> Effect Unit
mkdir path = mkdir' path { recursive: false, mode: mkPerms all all all }

-- | Makes a new directory with the specified permissions.
mkdir'
  :: FilePath
  -> { recursive :: Boolean, mode :: Perms }
  -> Effect Unit
mkdir' file { recursive, mode: perms } = runEffectFn2 mkdirSyncImpl file { recursive, mode: permsToString perms }

-- | Reads the contents of a directory.
readdir
  :: FilePath
  -> Effect (Array FilePath)
readdir file = runEffectFn1 readdirSyncImpl file

-- | Sets the accessed and modified times for the specified file.
utimes
  :: FilePath
  -> DateTime
  -> DateTime
  -> Effect Unit
utimes file atime mtime = runEffectFn3 utimesSyncImpl file (fromDate atime) (fromDate mtime)
  where
  fromDate date = ms (toEpochMilliseconds date) / 1000
  ms (Milliseconds n) = round n
  toEpochMilliseconds = unInstant <<< fromDateTime

-- | Reads the entire contents of a file returning the result as a raw buffer.
readFile
  :: FilePath
  -> Effect Buffer
readFile file = runEffectFn2 readFileSyncImpl file {}

-- | Reads the entire contents of a text file with the specified encoding.
readTextFile
  :: Encoding
  -> FilePath
  -> Effect String
readTextFile encoding file = runEffectFn2 readFileSyncImpl file { encoding: show encoding }

-- | Writes a buffer to a file.
writeFile
  :: FilePath
  -> Buffer
  -> Effect Unit
writeFile file buff = runEffectFn3 writeFileSyncImpl file buff {}

-- | Writes text to a file using the specified encoding.
writeTextFile
  :: Encoding
  -> FilePath
  -> String
  -> Effect Unit
writeTextFile encoding file text = runEffectFn3 writeFileSyncImpl file text { encoding: show encoding }

-- | Appends the contents of a buffer to a file.
appendFile
  :: FilePath
  -> Buffer
  -> Effect Unit
appendFile file buff = runEffectFn3 appendFileSyncImpl file buff {}

-- | Appends text to a file using the specified encoding.
appendTextFile
  :: Encoding
  -> FilePath
  -> String
  -> Effect Unit
appendTextFile encoding file buff = runEffectFn3 appendFileSyncImpl file buff { encoding: show encoding }

-- | Check if the path exists.
exists
  :: FilePath
  -> Effect Boolean
exists file = runEffectFn1 existsSyncImpl file

-- | Open a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_opensync_path_flags_mode)
-- | for details.
fdOpen
  :: FilePath
  -> FileFlags
  -> Maybe FileMode
  -> Effect FileDescriptor
fdOpen file flags mode = runEffectFn3 openSyncImpl file (fileFlagsToNode flags) (toNullable mode)

-- | Read from a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_readsync_fd_buffer_offset_length_position)
-- | for details.
fdRead
  :: FileDescriptor
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Maybe FilePosition
  -> Effect ByteCount
fdRead fd buff off len pos =
  runEffectFn5 readSyncImpl fd buff off len (toNullable pos)

-- | Convenience function to fill the whole buffer from the current
-- | file position.
fdNext
  :: FileDescriptor
  -> Buffer
  -> Effect ByteCount
fdNext fd buff = do
  sz <- size buff
  fdRead fd buff 0 sz Nothing

-- | Write to a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_writesync_fd_buffer_offset_length_position)
-- | for details.
fdWrite
  :: FileDescriptor
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Maybe FilePosition
  -> Effect ByteCount
fdWrite fd buff off len pos =
  runEffectFn5 writeSyncImpl fd buff off len (toNullable pos)

-- | Convenience function to append the whole buffer to the current
-- | file position.
fdAppend
  :: FileDescriptor
  -> Buffer
  -> Effect ByteCount
fdAppend fd buff = do
  sz <- size buff
  fdWrite fd buff 0 sz Nothing

-- | Flush a file synchronously.  See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_fsyncsync_fd)
-- | for details.
fdFlush
  :: FileDescriptor
  -> Effect Unit
fdFlush fd = runEffectFn1 fsyncSyncImpl fd

-- | Close a file synchronously. See the [Node documentation](http://nodejs.org/api/fs.html#fs_fs_closesync_fd)
-- | for details.
fdClose
  :: FileDescriptor
  -> Effect Unit
fdClose fd = runEffectFn1 closeSyncImpl fd
