module Data.Posix where

import Prelude
import Data.Newtype (class Newtype)

-- | A process ID.
newtype Pid = Pid Int

derive instance newtypePid :: Newtype Pid _
derive newtype instance eqPid :: Eq Pid
derive newtype instance ordPid :: Ord Pid

instance showPid :: Show Pid where
  show (Pid pid) = "(Pid " <> show pid <> ")"

-- | A group ID (for a process or a file).
newtype Gid = Gid Int

derive instance newtypeGid :: Newtype Gid _
derive newtype instance eqGid :: Eq Gid
derive newtype instance ordGid :: Ord Gid

instance showGid :: Show Gid where
  show (Gid gid) = "(Gid " <> show gid <> ")"

-- | A user ID (for a process or a file).
newtype Uid = Uid Int

derive instance newtypeUid :: Newtype Uid _
derive newtype instance eqUid :: Eq Uid
derive newtype instance ordUid :: Ord Uid

instance showUid :: Show Uid where
  show (Uid uid) = "(Uid " <> show uid <> ")"
