-- | This module defines data type for the different platforms supported by
-- | Node.js
module Node.Platform where

import Prelude
import Data.Maybe (Maybe(..))

-- | See [the Node docs](https://nodejs.org/dist/latest-v6.x/docs/api/os.html#os_os_platform).
data Platform
  = AIX
  | Darwin
  | FreeBSD
  | Linux
  | OpenBSD
  | SunOS
  | Win32
  | Android

-- | The String representation for a platform, recognised by Node.js.
toString :: Platform -> String
toString AIX     = "aix"
toString Darwin  = "darwin"
toString FreeBSD = "freebsd"
toString Linux   = "linux"
toString OpenBSD = "openbsd"
toString SunOS   = "sunos"
toString Win32   = "win32"
toString Android = "android"

-- | Attempt to parse a `Platform` value from a string, in the format returned
-- | by Node.js' `process.platform`.
fromString :: String -> Maybe Platform
fromString "aix"     = Just AIX
fromString "darwin"  = Just Darwin
fromString "freebsd" = Just FreeBSD
fromString "linux"   = Just Linux
fromString "openbsd" = Just OpenBSD
fromString "sunos"   = Just SunOS
fromString "win32"   = Just Win32
fromString "android" = Just Android
fromString _         = Nothing

instance showPlatform :: Show Platform where
  show AIX     = "AIX"
  show Darwin  = "Darwin"
  show FreeBSD = "FreeBSD"
  show Linux   = "Linux"
  show OpenBSD = "OpenBSD"
  show SunOS   = "SunOS"
  show Win32   = "Win32"
  show Android = "Android"

derive instance eqPlatform :: Eq Platform
derive instance ordPlatform :: Ord Platform
