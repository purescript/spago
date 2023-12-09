module Spago.Lock
  ( Lockfile
  , lockfileCodec
  , LockEntry
  , LockEntryData(..)
  , lockEntryCodec
  , PathLock
  , GitLock
  , PackageSetInfo
  , RegistryLock
  , WorkspaceLock
  , WorkspaceLockPackage
  ) where

import Spago.Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as Common
import Data.Profunctor as Profunctor
import Record as Record
import Registry.Internal.Codec as Registry.Codec
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Spago.Core.Config as Config
import Spago.Core.Config as Core
import Type.Proxy (Proxy(..))

type Lockfile =
  { workspace :: WorkspaceLock
  , packages :: Map PackageName LockEntry
  }

type LockEntry =
  { needed_by :: NonEmptyArray PackageName
  , package :: LockEntryData
  }

data LockEntryData
  = FromPath PathLock
  | FromGit GitLock
  | FromRegistry RegistryLock

derive instance Eq LockEntryData

type WorkspaceLock =
  { package_set :: Maybe PackageSetInfo
  , packages :: Map PackageName WorkspaceLockPackage
  , extra_packages :: Map PackageName Core.ExtraPackage
  }

type PackageSetInfo =
  { address :: Core.SetAddress
  , content :: Map PackageName Core.RemotePackage
  , compiler :: Range
  }

type WorkspaceLockPackage =
  { dependencies :: Core.Dependencies
  , test_dependencies :: Core.Dependencies
  , path :: FilePath
  , needed_by :: Array PackageName
  }

lockfileCodec :: JsonCodec Lockfile
lockfileCodec = CA.object "Lockfile"
  $ CA.recordProp (Proxy :: _ "workspace") workspaceLockCodec
  $ CA.recordProp (Proxy :: _ "packages") (Registry.Codec.packageMap lockEntryCodec)
  $ CA.record

workspaceLockCodec :: JsonCodec WorkspaceLock
workspaceLockCodec = CA.object "WorkspaceLock"
  $ CA.recordProp (Proxy :: _ "packages") (Registry.Codec.packageMap dependenciesCodec)
  $ CA.recordPropOptional (Proxy :: _ "package_set") packageSetCodec
  $ CA.recordProp (Proxy :: _ "extra_packages") (Registry.Codec.packageMap Config.extraPackageCodec)
  $ CA.record
  where
  dependenciesCodec = CA.object "Dependencies"
    $ CA.recordProp (Proxy :: _ "path") CA.string
    $ CA.recordProp (Proxy :: _ "needed_by") (CA.array PackageName.codec)
    $ CA.recordProp (Proxy :: _ "dependencies") Config.dependenciesCodec
    $ CA.recordProp (Proxy :: _ "test_dependencies") Config.dependenciesCodec
    $ CA.record

packageSetCodec :: JsonCodec PackageSetInfo
packageSetCodec = CA.object "PackageSetInfo"
  $ CA.recordProp (Proxy :: _ "address") Config.setAddressCodec
  $ CA.recordProp (Proxy :: _ "compiler") Range.codec
  $ CA.recordProp (Proxy :: _ "content") (Registry.Codec.packageMap Core.remotePackageCodec)
  $ CA.record

lockEntryCodec :: JsonCodec LockEntry
lockEntryCodec = CA.object "LockEntry"
  $ CA.recordProp (Proxy :: _ "needed_by") (Common.nonEmptyArray PackageName.codec)
  $ CA.recordProp (Proxy :: _ "package") (CA.codec' decode encode)
  $ CA.record
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

type PathLock =
  { path :: FilePath
  , dependencies :: Array PackageName
  }

pathLockCodec :: JsonCodec PathLock
pathLockCodec = Profunctor.dimap toRep fromRep $ CA.object "PathLock"
  $ CA.recordProp (Proxy :: _ "type") (constant pathLockType)
  $ CA.recordProp (Proxy :: _ "path") CA.string
  $ CA.recordProp (Proxy :: _ "dependencies") (CA.array PackageName.codec)
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
