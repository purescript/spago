module Spago.Lock
  ( Lockfile
  , lockfileCodec
  , LockEntry(..)
  , lockEntryCodec
  , PathLock
  , GitLock
  , RegistryLock
  , WorkspaceLock
  ) where

import Spago.Prelude

import Data.Codec.Argonaut as CA
import Data.Profunctor as Profunctor
import Record as Record
import Registry.Internal.Codec as Registry.Codec
import Registry.PackageName as PackageName
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Spago.Core.Config (Dependencies, ExtraPackage, SetAddress)
import Spago.Core.Config as Config
import Type.Proxy (Proxy(..))

type WorkspaceLock =
  { package_set :: Maybe SetAddress
  , packages ::
      Map PackageName
        { dependencies :: Dependencies
        , test_dependencies :: Dependencies
        , path :: FilePath
        }
  , extra_packages :: Map PackageName ExtraPackage
  }

type Lockfile =
  { workspace :: WorkspaceLock
  , packages :: Map PackageName LockEntry
  }

lockfileCodec :: JsonCodec Lockfile
lockfileCodec = CA.object "Lockfile"
  $ CA.recordProp (Proxy :: _ "workspace") workspaceLockCodec
  $ CA.recordProp (Proxy :: _ "packages") (Registry.Codec.packageMap lockEntryCodec)
  $ CA.record

workspaceLockCodec :: JsonCodec WorkspaceLock
workspaceLockCodec = CA.object "WorkspaceLock"
  $ CA.recordProp (Proxy :: _ "packages") (Registry.Codec.packageMap dependenciesCodec)
  $ CA.recordPropOptional (Proxy :: _ "package_set") Config.setAddressCodec
  $ CA.recordProp (Proxy :: _ "extra_packages") (Registry.Codec.packageMap Config.extraPackageCodec)
  $ CA.record
  where
  dependenciesCodec = CA.object "Dependencies"
    $ CA.recordProp (Proxy :: _ "path") CA.string
    $ CA.recordProp (Proxy :: _ "dependencies") Config.dependenciesCodec
    $ CA.recordProp (Proxy :: _ "test_dependencies") Config.dependenciesCodec
    $ CA.record

data LockEntry
  = FromPath PathLock
  | FromGit GitLock
  | FromRegistry RegistryLock

derive instance Eq LockEntry

lockEntryCodec :: JsonCodec LockEntry
lockEntryCodec = CA.codec' decode encode
  where
  encode = case _ of
    FromPath lock -> CA.encode pathLockCodec lock
    FromGit lock -> CA.encode gitLockCodec lock
    FromRegistry lock -> CA.encode registryLockCodec lock

  decode json =
    map FromPath (CA.decode pathLockCodec json)
      <|> map FromGit (CA.decode gitLockCodec json)
      <|> map FromRegistry (CA.decode registryLockCodec json)

pathLockType :: String
pathLockType = "local"

gitLockType :: String
gitLockType = "git"

registryLockType :: String
registryLockType = "registry"

type PathLock = { path :: FilePath }

pathLockCodec :: JsonCodec PathLock
pathLockCodec = Profunctor.dimap toRep fromRep $ CA.object "PathLock"
  $ CA.recordProp (Proxy :: _ "type") (constant pathLockType)
  $ CA.recordProp (Proxy :: _ "path") CA.string
  $ CA.record
  where
  toRep = Record.insert (Proxy :: _ "type") pathLockType
  fromRep = Record.delete (Proxy :: _ "type")

type GitLock =
  { url :: String
  , rev :: String
  , subdir :: Maybe FilePath
  , dependencies :: Array PackageName
  }

gitLockCodec :: JsonCodec GitLock
gitLockCodec = Profunctor.dimap toRep fromRep $ CA.object "GitLock"
  $ CA.recordProp (Proxy :: _ "type") (constant gitLockType)
  $ CA.recordProp (Proxy :: _ "url") CA.string
  $ CA.recordProp (Proxy :: _ "rev") CA.string
  $ CA.recordPropOptional (Proxy :: _ "subdir") CA.string
  $ CA.recordProp (Proxy :: _ "dependencies") (CA.array PackageName.codec)
  $ CA.record
  where
  toRep = Record.insert (Proxy :: _ "type") gitLockType
  fromRep = Record.delete (Proxy :: _ "type")

type RegistryLock =
  { version :: Version
  , integrity :: Sha256
  , dependencies :: Array PackageName
  }

registryLockCodec :: JsonCodec RegistryLock
registryLockCodec = Profunctor.dimap toRep fromRep $ CA.object "RegistryLock"
  $ CA.recordProp (Proxy :: _ "type") (constant registryLockType)
  $ CA.recordProp (Proxy :: _ "version") Version.codec
  $ CA.recordProp (Proxy :: _ "integrity") Sha256.codec
  $ CA.recordProp (Proxy :: _ "dependencies") (CA.array PackageName.codec)
  $ CA.record
  where
  toRep = Record.insert (Proxy :: _ "type") registryLockType
  fromRep = Record.delete (Proxy :: _ "type")

constant :: String → JsonCodec String
constant val = CA.codec' decode encode
  where
  encode = CA.encode CA.string
  decode json = CA.decode CA.string json >>= case _ of
    decoded | decoded == val -> pure val
    _ -> Left (CA.UnexpectedValue json)
