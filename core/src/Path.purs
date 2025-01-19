module Spago.Path
  ( (</>)
  , AdHocFilePath
  , GlobalPath
  , LocalPath
  , RootPath
  , appendPath
  , basename
  , class AppendPath
  , class IsPath
  , dirname
  , isPrefixOf
  , localPart
  , localPathCodec
  , global
  , mkRoot
  , printLocalPath
  , quote
  , relativeTo
  , replaceExtension
  , rootPart
  , toAbsolute
  , toGlobal
  , toRaw
  , withForwardSlashes
  , withForwardSlashes'
  ) where

import Prelude

import Data.Codec.JSON as CJ
import Data.Function (on)
import Data.Maybe (Maybe(..), isJust)
import Data.Profunctor (dimap)
import Data.String as String
import Effect.Class (class MonadEffect, liftEffect)
import Node.Path as Node.Path
import Node.Path as Path
import Node.Platform (Platform(..)) as Node
import Node.Process (platform) as Node

-- | Normally this represents the root directory of the workspace. All Spago
-- | Workspace-scoped paths are relative to a `RootPath`.
newtype RootPath = RootPath String

derive newtype instance Show RootPath
derive newtype instance Eq RootPath
derive newtype instance Ord RootPath

-- | A Spago Workspace-scoped path, consists of two parts: `RootPath` and local
-- | part, relative to the root. This lets us both have the full path for
-- | actually working with files and the local part for user-facing output.
newtype LocalPath = LocalPath { root :: RootPath, local :: AdHocFilePath }

instance Show LocalPath where
  show (LocalPath p) = p.local

instance Eq LocalPath where
  eq = eq `on` toGlobal

instance Ord LocalPath where
  compare = compare `on` toGlobal

-- | A part that is logically not part of the Spago Workspace, but points to
-- | something "global", such as registry cache, temp directory, and so on.
newtype GlobalPath = GlobalPath String

instance Show GlobalPath where
  show (GlobalPath p) = p

derive newtype instance Eq GlobalPath
derive newtype instance Ord GlobalPath

type AdHocFilePath = String

class (Show path, Eq path, Ord path) <= IsPath path where
  toGlobal :: path -> GlobalPath
  relativeTo :: path -> RootPath -> LocalPath
  quote :: path -> String
  replaceExtension :: String.Pattern -> String.Replacement -> path -> Maybe path
  withForwardSlashes :: path -> path

instance IsPath LocalPath where
  toGlobal (LocalPath { root: RootPath root, local }) =
    GlobalPath $ Path.concat [ root, local ]
  relativeTo path root
    | rootPart path == root = path
    | otherwise = toGlobal path `relativeTo` root
  quote (LocalPath path)
    | path.local == "" = "\".\""
    | otherwise = "\"" <> path.local <> "\""
  replaceExtension p r (LocalPath path) =
    LocalPath <<< path { local = _ } <$> replaceExtension_ p r path.local
  withForwardSlashes (LocalPath path) =
    LocalPath { root: withForwardSlashes path.root, local: withForwardSlashes' path.local }

instance IsPath GlobalPath where
  toGlobal = identity
  relativeTo (GlobalPath path) (RootPath root)
    | areBothAbsolutePathsOnDifferentDrives path root =
        LocalPath { root: RootPath path, local: "" } -- see comments on `areBothAbsolutePathsOnDifferentDrives`
    | otherwise =
        LocalPath { root: RootPath root, local: Path.relative root path }
  quote (GlobalPath path) =
    "\"" <> path <> "\""
  replaceExtension p r (GlobalPath path) =
    GlobalPath <$> replaceExtension_ p r path
  withForwardSlashes (GlobalPath path) =
    GlobalPath $ withForwardSlashes' path

instance IsPath RootPath where
  toGlobal (RootPath path) = GlobalPath path
  relativeTo path root = toGlobal path `relativeTo` root
  quote (RootPath path) = "\"" <> path <> "\""
  replaceExtension p r (RootPath path) = RootPath <$> replaceExtension_ p r path
  withForwardSlashes (RootPath path) = RootPath $ withForwardSlashes' path

class AppendPath base result | base -> result where
  appendPath :: base -> AdHocFilePath -> result

instance AppendPath RootPath LocalPath where
  appendPath root local
    | Path.isAbsolute local = global local `relativeTo` root
    | otherwise = LocalPath { root, local }

instance AppendPath LocalPath LocalPath where
  appendPath (LocalPath { root, local }) path
    | Path.isAbsolute path = global path `relativeTo` root
    | otherwise = LocalPath { root, local: Path.concat [ local, path ] }

instance AppendPath GlobalPath GlobalPath where
  appendPath (GlobalPath path) p
    | Path.isAbsolute p = GlobalPath p
    | otherwise = GlobalPath $ Path.concat [ path, p ]

infixl 5 appendPath as </>

-- | The only publicly available way to create a root path. This function is
-- | intentionally made effectful, even though it doesn't have to be, to make it
-- | difficult to use it accidentally.
mkRoot :: ∀ m path. MonadEffect m => IsPath path => path -> m RootPath
mkRoot path = pure $ RootPath $ toRaw path

global :: String -> GlobalPath
global = GlobalPath

rootPart :: LocalPath -> RootPath
rootPart (LocalPath { root }) = root

localPart :: LocalPath -> AdHocFilePath
localPart (LocalPath { local }) = local

dirname :: ∀ path. IsPath path => path -> GlobalPath
dirname path = global $ Node.Path.dirname $ toRaw path

replaceExtension_ :: String.Pattern -> String.Replacement -> String -> Maybe String
replaceExtension_ p (String.Replacement r) = map (_ <> r) <<< String.stripSuffix p

basename :: ∀ path. IsPath path => path -> String
basename path = Node.Path.basename $ toRaw path

isPrefixOf :: ∀ path1 path2. IsPath path1 => IsPath path2 => path1 -> path2 -> Boolean
isPrefixOf prefix whole = isJust $ String.stripPrefix (String.Pattern $ toRaw prefix) (toRaw whole)

toAbsolute :: ∀ m path. IsPath path => MonadEffect m => path -> m GlobalPath
toAbsolute path = liftEffect $ GlobalPath <$> Node.Path.resolve [] (toRaw path)

toRaw :: ∀ path. IsPath path => path -> String
toRaw p = let (GlobalPath g) = toGlobal p in g

withForwardSlashes' :: String -> String
withForwardSlashes' = String.replaceAll (String.Pattern "\\") (String.Replacement "/")

localPathCodec :: RootPath -> CJ.Codec LocalPath
localPathCodec root = CJ.string # dimap printLocalPath (root </> _)

-- | Formats the local part of the path for user-friendly printing. The only
-- | difference (for now) is that the "root" path is represented to the user as
-- | "./" so as not to get them confused by a seeming absence of output.
printLocalPath :: LocalPath -> String
printLocalPath p =
  let
    l = localPart p
  in
    if l == "" then "./" else l

-- This function is special handling for Windows, which may have multiple
-- different file systems with separate roots and no way to build a relative
-- path from one root to another, for example C:\foo\bar and D:\qux\baz. When
-- this happens as we try to build a `LocalPath` as a relative from a given
-- `RootPath`, and the two paths are from different file systems, we cannot use
-- the same `RootPath` as root of the result.
--
-- For example:
--
--     -- POSIX case:
--     (GlobalPath "/a/b") `relativeTo` (RootPath "/a/x") == LocalPath { root: RootPath "/a/x", local: "../b" }
--
--     -- Windows case on the same drive:
--     (GlobalPath "C:\\a\\b") `relativeTo` (RootPath "C:\\a\\x") == LocalPath { root: RootPath "C:\\a\\x", local: "..\\b" }
--
--     -- But Windows case on different drives:
--     (GlobalPath "C:\\a\\b") `relativeTo` (RootPath "D:\\a\\x") == LocalPath { root: RootPath "C:\\a\\b", local: "" }
--
-- In the last case the root path `D:\\a\\x` could not be used as the root of
-- the resulting `LocalPath`, because there is no way to build a relative path
-- from it to `C:\\a\\b`.
areBothAbsolutePathsOnDifferentDrives :: String -> String -> Boolean
areBothAbsolutePathsOnDifferentDrives a b
  | Node.platform == Just Node.Win32 =
      Path.isAbsolute a && Path.isAbsolute b && driveLetter a /= driveLetter b
      where
      driveLetter s = String.toLower $ String.take 1 s
  | otherwise =
      false
