module Node.FS
  ( FileDescriptor(..)
  , FileMode(..)
  , SymlinkType(..)
  , symlinkTypeToNode
  , BufferLength(..)
  , BufferOffset(..)
  , ByteCount(..)
  , FilePosition(..)
  , module Exports
  ) where

import Prelude

import Node.FS.Constants (FileFlags(..), fileFlagsToNode) as Exports

foreign import data FileDescriptor :: Type

type FileMode = Int
type FilePosition = Int
type BufferLength = Int
type BufferOffset = Int
type ByteCount = Int

-- | Symlink varieties.
data SymlinkType = FileLink | DirLink | JunctionLink

-- | Convert a `SymlinkType` to a `String` in the format expected by the
-- | Node.js filesystem API.
symlinkTypeToNode :: SymlinkType -> String
symlinkTypeToNode ty = case ty of
  FileLink -> "file"
  DirLink -> "dir"
  JunctionLink -> "junction"

instance showSymlinkType :: Show SymlinkType where
  show FileLink = "FileLink"
  show DirLink = "DirLink"
  show JunctionLink = "JunctionLink"

instance eqSymlinkType :: Eq SymlinkType where
  eq FileLink FileLink = true
  eq DirLink DirLink = true
  eq JunctionLink JunctionLink = true
  eq _ _ = false
