module Node.FS.Aff
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
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, Error, makeAff, nonCanceler)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.FS as F
import Node.FS.Async as A
import Node.FS.Constants (AccessMode, CopyMode)
import Node.FS.Perms (Perms)
import Node.FS.Stats (Stats)
import Node.Path (FilePath)

toAff
  :: forall a
   . (A.Callback a -> Effect Unit)
  -> Aff a
toAff p = makeAff \k -> p k $> nonCanceler

toAff1
  :: forall a x
   . (x -> A.Callback a -> Effect Unit)
  -> x
  -> Aff a
toAff1 f a = toAff (f a)

toAff2
  :: forall a x y
   . (x -> y -> A.Callback a -> Effect Unit)
  -> x
  -> y
  -> Aff a
toAff2 f a b = toAff (f a b)

toAff3
  :: forall a x y z
   . (x -> y -> z -> A.Callback a -> Effect Unit)
  -> x
  -> y
  -> z
  -> Aff a
toAff3 f a b c = toAff (f a b c)

toAff5
  :: forall a w v x y z
   . (w -> v -> x -> y -> z -> A.Callback a -> Effect Unit)
  -> w
  -> v
  -> x
  -> y
  -> z
  -> Aff a
toAff5 f a b c d e = toAff (f a b c d e)

access :: String -> Aff (Maybe Error)
access path = makeAff \k -> do
  A.access path (k <<< Right)
  pure nonCanceler

access' :: String -> AccessMode -> Aff (Maybe Error)
access' path mode = makeAff \k -> do
  A.access' path mode (k <<< Right)
  pure nonCanceler

copyFile :: String -> String -> Aff Unit
copyFile = toAff2 A.copyFile

copyFile' :: String -> String -> CopyMode -> Aff Unit
copyFile' = toAff3 A.copyFile'

mkdtemp :: String -> Aff String
mkdtemp = toAff1 A.mkdtemp

mkdtemp' :: String -> Encoding -> Aff String
mkdtemp' = toAff2 A.mkdtemp'

-- |
-- | Rename a file.
-- |
rename :: FilePath -> FilePath -> Aff Unit
rename = toAff2 A.rename

-- |
-- | Truncates a file to the specified length.
-- |
truncate :: FilePath -> Int -> Aff Unit
truncate = toAff2 A.truncate

-- |
-- | Changes the ownership of a file.
-- |
chown :: FilePath -> Int -> Int -> Aff Unit
chown = toAff3 A.chown

-- |
-- | Changes the permissions of a file.
-- |
chmod :: FilePath -> Perms -> Aff Unit
chmod = toAff2 A.chmod

-- |
-- | Gets file statistics.
-- |
stat :: FilePath -> Aff Stats
stat = toAff1 A.stat

-- |
-- | Creates a link to an existing file.
-- |
link :: FilePath -> FilePath -> Aff Unit
link = toAff2 A.link

-- |
-- | Creates a symlink.
-- |
symlink
  :: FilePath
  -> FilePath
  -> F.SymlinkType
  -> Aff Unit
symlink = toAff3 A.symlink

-- |
-- | Reads the value of a symlink.
-- |
readlink :: FilePath -> Aff FilePath
readlink = toAff1 A.readlink

-- |
-- | Find the canonicalized absolute location for a path.
-- |
realpath :: FilePath -> Aff FilePath
realpath = toAff1 A.realpath

-- |
-- | Find the canonicalized absolute location for a path using a cache object
-- | for already resolved paths.
-- |
realpath' :: forall cache. FilePath -> { | cache } -> Aff FilePath
realpath' = toAff2 A.realpath'

-- |
-- | Deletes a file.
-- |
unlink :: FilePath -> Aff Unit
unlink = toAff1 A.unlink

-- |
-- | Deletes a directory.
-- |
rmdir :: FilePath -> Aff Unit
rmdir = toAff1 A.rmdir

-- |
-- | Deletes a directory with options.
-- |
rmdir' :: FilePath -> { maxRetries :: Int, retryDelay :: Int } -> Aff Unit
rmdir' = toAff2 A.rmdir'

-- |
-- | Deletes a file or directory.
-- |
rm :: FilePath -> Aff Unit
rm = toAff1 A.rmdir

-- |
-- | Deletes a file or directory with options.
-- |
rm' :: FilePath -> { force :: Boolean, maxRetries :: Int, recursive :: Boolean, retryDelay :: Int } -> Aff Unit
rm' = toAff2 A.rm'

-- |
-- | Makes a new directory.
-- |
mkdir :: FilePath -> Aff Unit
mkdir = toAff1 A.mkdir

-- |
-- | Makes a new directory with all of its options.
-- |
mkdir' :: FilePath -> { recursive :: Boolean, mode :: Perms } -> Aff Unit
mkdir' = toAff2 A.mkdir'

-- |
-- | Reads the contents of a directory.
-- |
readdir :: FilePath -> Aff (Array FilePath)
readdir = toAff1 A.readdir

-- |
-- | Sets the accessed and modified times for the specified file.
-- |
utimes :: FilePath -> DateTime -> DateTime -> Aff Unit
utimes = toAff3 A.utimes

-- |
-- | Reads the entire contents of a file returning the result as a raw buffer.
-- |
readFile :: FilePath -> Aff Buffer
readFile = toAff1 A.readFile

-- |
-- | Reads the entire contents of a text file with the specified encoding.
-- |
readTextFile :: Encoding -> FilePath -> Aff String
readTextFile = toAff2 A.readTextFile

-- |
-- | Writes a buffer to a file.
-- |
writeFile :: FilePath -> Buffer -> Aff Unit
writeFile = toAff2 A.writeFile

-- |
-- | Writes text to a file using the specified encoding.
-- |
writeTextFile :: Encoding -> FilePath -> String -> Aff Unit
writeTextFile = toAff3 A.writeTextFile

-- |
-- | Appends the contents of a buffer to a file.
-- |
appendFile :: FilePath -> Buffer -> Aff Unit
appendFile = toAff2 A.appendFile

-- |
-- | Appends text to a file using the specified encoding.
-- |
appendTextFile :: Encoding -> FilePath -> String -> Aff Unit
appendTextFile = toAff3 A.appendTextFile

-- | Open a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_open_path_flags_mode_callback)
-- | for details.
fdOpen
  :: FilePath
  -> F.FileFlags
  -> Maybe F.FileMode
  -> Aff F.FileDescriptor
fdOpen = toAff3 A.fdOpen

-- | Read from a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_read_fd_buffer_offset_length_position_callback)
-- | for details.
fdRead
  :: F.FileDescriptor
  -> Buffer
  -> F.BufferOffset
  -> F.BufferLength
  -> Maybe F.FilePosition
  -> Aff F.ByteCount
fdRead = toAff5 A.fdRead

-- | Convenience function to fill the whole buffer from the current
-- | file position.
fdNext :: F.FileDescriptor -> Buffer -> Aff F.ByteCount
fdNext = toAff2 A.fdNext

-- | Write to a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_write_fd_buffer_offset_length_position_callback)
-- | for details.
fdWrite
  :: F.FileDescriptor
  -> Buffer
  -> F.BufferOffset
  -> F.BufferLength
  -> Maybe F.FilePosition
  -> Aff F.ByteCount
fdWrite = toAff5 A.fdWrite

-- | Convenience function to append the whole buffer to the current
-- | file position.
fdAppend :: F.FileDescriptor -> Buffer -> Aff F.ByteCount
fdAppend = toAff2 A.fdAppend

-- | Close a file asynchronously. See the [Node Documentation](https://nodejs.org/api/fs.html#fs_fs_close_fd_callback)
-- | for details.
fdClose :: F.FileDescriptor -> Aff Unit
fdClose = toAff1 A.fdClose
