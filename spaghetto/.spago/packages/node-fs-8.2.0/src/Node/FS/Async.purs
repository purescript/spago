module Node.FS.Async
  ( Callback(..)
  , access
  , access'
  , copyFile
  , copyFile'
  , mkdtemp
  , mkdtemp'
  , rename
  , truncate
  , chown
  , chmod
  , lstat
  , stat
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
  , fdOpen
  , fdRead
  , fdNext
  , fdWrite
  , fdAppend
  , fdClose
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Either (Either(..))
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn6, mkEffectFn1, mkEffectFn2, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn6)
import Node.Buffer (Buffer, size)
import Node.Encoding (Encoding(..), encodingToNode)
import Node.FS (FileDescriptor, ByteCount, FilePosition, BufferLength, BufferOffset, FileMode, SymlinkType, symlinkTypeToNode)
import Node.FS.Constants (FileFlags, fileFlagsToNode, AccessMode, CopyMode, defaultAccessMode, defaultCopyMode)
import Node.FS.Perms (Perms, permsToString, all, mkPerms)
import Node.FS.Stats (StatsObj, Stats(..))
import Node.Path (FilePath)

type JSCallback a = EffectFn2 (Nullable Error) a Unit

handleCallback :: forall a. Callback a -> JSCallback a
handleCallback cb = mkEffectFn2 \err a -> case toMaybe err of
  Nothing -> cb (Right a)
  Just err' -> cb (Left err')

-- | Type synonym for callback functions.
type Callback a = Either Error a -> Effect Unit

access :: FilePath -> (Maybe Error -> Effect Unit) -> Effect Unit
access path = access' path defaultAccessMode

access' :: FilePath -> AccessMode -> (Maybe Error -> Effect Unit) -> Effect Unit
access' path mode cb = runEffectFn3 accessImpl path mode $ mkEffectFn1 \err -> do
  cb $ toMaybe err

foreign import accessImpl :: EffectFn3 FilePath AccessMode (EffectFn1 (Nullable Error) Unit) Unit

copyFile :: FilePath -> FilePath -> Callback Unit -> Effect Unit
copyFile src dest = copyFile' src dest defaultCopyMode

copyFile' :: FilePath -> FilePath -> CopyMode -> Callback Unit -> Effect Unit
copyFile' src dest mode cb = runEffectFn4 copyFileImpl src dest mode (handleCallback cb)

foreign import copyFileImpl :: EffectFn4 FilePath FilePath CopyMode (JSCallback Unit) Unit

mkdtemp :: String -> Callback String -> Effect Unit
mkdtemp prefix = mkdtemp' prefix UTF8

mkdtemp' :: String -> Encoding -> Callback String -> Effect Unit
mkdtemp' prefix encoding cb = runEffectFn3 mkdtempImpl prefix (encodingToNode encoding) (handleCallback cb)

foreign import mkdtempImpl :: EffectFn3 FilePath String (JSCallback String) Unit

foreign import renameImpl :: EffectFn3 FilePath FilePath (JSCallback Unit) Unit
foreign import truncateImpl :: EffectFn3 FilePath Int (JSCallback Unit) Unit
foreign import chownImpl :: EffectFn4 FilePath Int Int (JSCallback Unit) Unit
foreign import chmodImpl :: EffectFn3 FilePath String (JSCallback Unit) Unit
foreign import statImpl :: EffectFn2 FilePath (JSCallback StatsObj) Unit
foreign import lstatImpl :: EffectFn2 FilePath (JSCallback StatsObj) Unit
foreign import linkImpl :: EffectFn3 FilePath FilePath (JSCallback Unit) Unit
foreign import symlinkImpl :: EffectFn4 FilePath FilePath String (JSCallback Unit) Unit
foreign import readlinkImpl :: EffectFn2 FilePath (JSCallback FilePath) Unit
foreign import realpathImpl :: forall cache. EffectFn3 FilePath { | cache } (JSCallback FilePath) Unit
foreign import unlinkImpl :: EffectFn2 FilePath (JSCallback Unit) Unit
foreign import rmdirImpl :: EffectFn3 FilePath { maxRetries :: Int, retryDelay :: Int } (JSCallback Unit) Unit
foreign import rmImpl :: EffectFn3 FilePath { force :: Boolean, maxRetries :: Int, recursive :: Boolean, retryDelay :: Int } (JSCallback Unit) Unit
foreign import mkdirImpl :: EffectFn3 FilePath { recursive :: Boolean, mode :: String } (JSCallback Unit) Unit
foreign import readdirImpl :: EffectFn2 FilePath (JSCallback (Array FilePath)) Unit
foreign import utimesImpl :: EffectFn4 FilePath Int Int (JSCallback Unit) Unit
foreign import readFileImpl :: forall a opts. EffectFn3 FilePath { | opts } (JSCallback a) Unit
foreign import writeFileImpl :: forall a opts. EffectFn4 FilePath a { | opts } (JSCallback Unit) Unit
foreign import appendFileImpl :: forall a opts. EffectFn4 FilePath a { | opts } (JSCallback Unit) Unit
foreign import openImpl :: EffectFn4 FilePath String (Nullable FileMode) (JSCallback FileDescriptor) Unit
foreign import readImpl :: EffectFn6 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) (JSCallback ByteCount) Unit
foreign import writeImpl :: EffectFn6 FileDescriptor Buffer BufferOffset BufferLength (Nullable FilePosition) (JSCallback ByteCount) Unit
foreign import closeImpl :: EffectFn2 FileDescriptor (JSCallback Unit) Unit

-- | Renames a file.
rename
  :: FilePath
  -> FilePath
  -> Callback Unit
  -> Effect Unit
rename oldFile newFile cb = runEffectFn3 renameImpl oldFile newFile (handleCallback cb)

-- | Truncates a file to the specified length.
truncate
  :: FilePath
  -> Int
  -> Callback Unit
  -> Effect Unit
truncate file len cb = runEffectFn3 truncateImpl file len (handleCallback cb)

-- | Changes the ownership of a file.
chown
  :: FilePath
  -> Int
  -> Int
  -> Callback Unit
  -> Effect Unit
chown file uid gid cb = runEffectFn4 chownImpl file uid gid (handleCallback cb)

-- | Changes the permissions of a file.
chmod
  :: FilePath
  -> Perms
  -> Callback Unit
  -> Effect Unit
chmod file perms cb = runEffectFn3 chmodImpl file (permsToString perms) (handleCallback cb)

-- | Gets file statistics.
stat
  :: FilePath
  -> Callback Stats
  -> Effect Unit
stat file cb = runEffectFn2 statImpl file (handleCallback $ cb <<< map Stats)

-- | Gets file or symlink statistics. `lstat` is identical to `stat`, except
-- | that if the `FilePath` is a symbolic link, then the link itself is stat-ed,
-- | not the file that it refers to.
lstat
  :: FilePath
  -> Callback Stats
  -> Effect Unit
lstat file cb = runEffectFn2 lstatImpl file (handleCallback $ cb <<< map Stats)

-- | Creates a link to an existing file.
link
  :: FilePath
  -> FilePath
  -> Callback Unit
  -> Effect Unit
link src dst cb = runEffectFn3 linkImpl src dst (handleCallback cb)

-- | Creates a symlink.
symlink
  :: FilePath
  -> FilePath
  -> SymlinkType
  -> Callback Unit
  -> Effect Unit
symlink src dest ty cb = runEffectFn4 symlinkImpl src dest (symlinkTypeToNode ty) (handleCallback cb)

-- | Reads the value of a symlink.
readlink
  :: FilePath
  -> Callback FilePath
  -> Effect Unit
readlink path cb = runEffectFn2 readlinkImpl path (handleCallback cb)

-- | Find the canonicalized absolute location for a path.
realpath
  :: FilePath
  -> Callback FilePath
  -> Effect Unit
realpath path cb = runEffectFn3 realpathImpl path {} (handleCallback cb)

-- | Find the canonicalized absolute location for a path using a cache object
-- | for already resolved paths.
realpath'
  :: forall cache
   . FilePath
  -> { | cache }
  -> Callback FilePath
  -> Effect Unit
realpath' path cache cb = runEffectFn3 realpathImpl path cache (handleCallback cb)

-- | Deletes a file.
unlink
  :: FilePath
  -> Callback Unit
  -> Effect Unit
unlink file cb = runEffectFn2 unlinkImpl file (handleCallback cb)

-- | Deletes a directory.
rmdir
  :: FilePath
  -> Callback Unit
  -> Effect Unit
rmdir path cb = rmdir' path { maxRetries: 0, retryDelay: 100 } cb

-- | Deletes a directory with options.
rmdir'
  :: FilePath
  -> { maxRetries :: Int, retryDelay :: Int }
  -> Callback Unit
  -> Effect Unit
rmdir' path opts cb = runEffectFn3 rmdirImpl path opts (handleCallback cb)

-- | Deletes a file or directory.
rm
  :: FilePath
  -> Callback Unit
  -> Effect Unit
rm path = rm' path { force: false, maxRetries: 100, recursive: false, retryDelay: 1000 }

-- | Deletes a file or directory with options.
rm'
  :: FilePath
  -> { force :: Boolean, maxRetries :: Int, recursive :: Boolean, retryDelay :: Int }
  -> Callback Unit
  -> Effect Unit
rm' path opts cb = runEffectFn3 rmImpl path opts (handleCallback cb)

-- | Makes a new directory.
mkdir
  :: FilePath
  -> Callback Unit
  -> Effect Unit
mkdir path = mkdir' path { recursive: false, mode: mkPerms all all all }

-- | Makes a new directory with the specified permissions.
mkdir'
  :: FilePath
  -> { recursive :: Boolean, mode :: Perms }
  -> Callback Unit
  -> Effect Unit
mkdir' file { recursive, mode: perms } cb = runEffectFn3 mkdirImpl file { recursive, mode: permsToString perms } (handleCallback cb)

-- | Reads the contents of a directory.
readdir
  :: FilePath
  -> Callback (Array FilePath)
  -> Effect Unit
readdir file cb = runEffectFn2 readdirImpl file (handleCallback cb)

-- | Sets the accessed and modified times for the specified file.
utimes
  :: FilePath
  -> DateTime
  -> DateTime
  -> Callback Unit
  -> Effect Unit
utimes file atime mtime cb = runEffectFn4 utimesImpl file (fromDate atime) (fromDate mtime) (handleCallback cb)
  where
  fromDate date = ms (toEpochMilliseconds date) / 1000
  ms (Milliseconds n) = round n
  toEpochMilliseconds = unInstant <<< fromDateTime

-- | Reads the entire contents of a file returning the result as a raw buffer.
readFile
  :: FilePath
  -> Callback Buffer
  -> Effect Unit
readFile file cb = runEffectFn3 readFileImpl file {} (handleCallback cb)

-- | Reads the entire contents of a text file with the specified encoding.
readTextFile
  :: Encoding
  -> FilePath
  -> Callback String
  -> Effect Unit
readTextFile encoding file cb = runEffectFn3 readFileImpl file { encoding: show encoding } (handleCallback cb)

-- | Writes a buffer to a file.
writeFile
  :: FilePath
  -> Buffer
  -> Callback Unit
  -> Effect Unit
writeFile file buff cb = runEffectFn4 writeFileImpl file buff {} (handleCallback cb)

-- | Writes text to a file using the specified encoding.
writeTextFile
  :: Encoding
  -> FilePath
  -> String
  -> Callback Unit
  -> Effect Unit
writeTextFile encoding file buff cb = runEffectFn4 writeFileImpl file buff { encoding: show encoding } (handleCallback cb)

-- | Appends the contents of a buffer to a file.
appendFile
  :: FilePath
  -> Buffer
  -> Callback Unit
  -> Effect Unit
appendFile file buff cb = runEffectFn4 appendFileImpl file buff {} (handleCallback cb)

-- | Appends text to a file using the specified encoding.
appendTextFile
  :: Encoding
  -> FilePath
  -> String
  -> Callback Unit
  -> Effect Unit
appendTextFile encoding file buff cb = runEffectFn4 appendFileImpl file buff { encoding: show encoding } (handleCallback cb)

-- | Open a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_open_path_flags_mode_callback)
-- | for details.
fdOpen
  :: FilePath
  -> FileFlags
  -> Maybe FileMode
  -> Callback FileDescriptor
  -> Effect Unit
fdOpen file flags mode cb = runEffectFn4 openImpl file (fileFlagsToNode flags) (toNullable mode) (handleCallback cb)

-- | Read from a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_read_fd_buffer_offset_length_position_callback)
-- | for details.
fdRead
  :: FileDescriptor
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Maybe FilePosition
  -> Callback ByteCount
  -> Effect Unit
fdRead fd buff off len pos cb = runEffectFn6 readImpl fd buff off len (toNullable pos) (handleCallback cb)

-- | Convenience function to fill the whole buffer from the current
-- | file position.
fdNext
  :: FileDescriptor
  -> Buffer
  -> Callback ByteCount
  -> Effect Unit
fdNext fd buff cb = do
  sz <- size buff
  fdRead fd buff 0 sz Nothing cb

-- | Write to a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_write_fd_buffer_offset_length_position_callback)
-- | for details.
fdWrite
  :: FileDescriptor
  -> Buffer
  -> BufferOffset
  -> BufferLength
  -> Maybe FilePosition
  -> Callback ByteCount
  -> Effect Unit
fdWrite fd buff off len pos cb = runEffectFn6 writeImpl fd buff off len (toNullable pos) (handleCallback cb)

-- | Convenience function to append the whole buffer to the current
-- | file position.
fdAppend
  :: FileDescriptor
  -> Buffer
  -> Callback ByteCount
  -> Effect Unit
fdAppend fd buff cb = do
  sz <- size buff
  fdWrite fd buff 0 sz Nothing cb

-- | Close a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_close_fd_callback)
-- | for details.
fdClose
  :: FileDescriptor
  -> Callback Unit
  -> Effect Unit
fdClose fd cb = runEffectFn2 closeImpl fd (handleCallback cb)
