module Spago.NodeVersion
  ( NodeVersionCheck(..)
  , checkNodeVersion
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (traverse)

data NodeVersionCheck
  = NodeVersionOk
  | NodeVersionTooOld String
  | NodeVersionUnparseable String

derive instance Eq NodeVersionCheck
instance Show NodeVersionCheck where
  show NodeVersionOk = "NodeVersionOk"
  show (NodeVersionTooOld v) = "(NodeVersionTooOld " <> show v <> ")"
  show (NodeVersionUnparseable v) = "(NodeVersionUnparseable " <> show v <> ")"

-- | Check if a version string meets the minimum Node.js version requirement
checkNodeVersion :: { major :: Int, minor :: Int } -> String -> NodeVersionCheck
checkNodeVersion minimum version =
  case traverse Int.fromString (Array.take 2 parts) of
    Just [ major, minor ]
      | major > minimum.major -> NodeVersionOk
      | major == minimum.major && minor >= minimum.minor -> NodeVersionOk
      | otherwise -> NodeVersionTooOld version
    _ -> NodeVersionUnparseable version
  where
  -- version is like "v22.5.0" or "22.5.0"
  versionStr = String.stripPrefix (String.Pattern "v") version # fromMaybe version
  parts = String.split (String.Pattern ".") versionStr
