module Spago.Lock
  ( GitLock
  , LockEntry(..)
  , Lockfile
  , PackageSetInfo
  , PathLock
  , RegistryLock
  , WorkspaceLock
  , WorkspaceLockPackage
  , WorkspaceLockPackageEnv
  , lockEntryCodec
  , lockfileCodec
  ) where

import Spago.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
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

data LockEntry
  = FromPath PathLock
  | FromGit GitLock
  | FromRegistry RegistryLock

derive instance Eq LockEntry

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
  { core :: WorkspaceLockPackageEnv
  , test :: WorkspaceLockPackageEnv
  , path :: FilePath
  }

type WorkspaceLockPackageEnv =
  { dependencies :: Core.Dependencies
  , build_plan :: Set PackageName
  }

lockfileCodec :: CJ.Codec Lockfile
lockfileCodec = CJ.named "Lockfile" $ CJ.object
  $ CJ.recordProp (Proxy @"workspace") workspaceLockCodec
  $ CJ.recordProp (Proxy @"packages") (Registry.Codec.packageMap lockEntryCodec)
  $ CJ.record

workspaceLockCodec :: CJ.Codec WorkspaceLock
workspaceLockCodec = CJ.named "WorkspaceLock" $ CJ.object
  $ CJ.recordProp (Proxy @"packages") (Registry.Codec.packageMap dependenciesCodec)
  $ CJ.recordPropOptional (Proxy @"package_set") packageSetCodec
  $ CJ.recordProp (Proxy @"extra_packages") (Registry.Codec.packageMap Config.extraPackageCodec)
  $ CJ.record
  where
  dependenciesCodec = CJ.named "Dependencies" $ CJ.object
    $ CJ.recordProp (Proxy @"path") CJ.string
    $ CJ.recordProp (Proxy @"core") envCodec
    $ CJ.recordProp (Proxy @"test") envCodec
    $ CJ.record

  envCodec = CJ.named "Environment" $ CJ.object
    $ CJ.recordProp (Proxy @"dependencies") Config.dependenciesCodec
    $ CJ.recordProp (Proxy @"build_plan") (CJ.Common.set PackageName.codec)
    $ CJ.record

packageSetCodec :: CJ.Codec PackageSetInfo
packageSetCodec = CJ.named "PackageSetInfo" $ CJ.object
  $ CJ.recordProp (Proxy @"address") Config.setAddressCodec
  $ CJ.recordProp (Proxy @"compiler") Range.codec
  $ CJ.recordProp (Proxy @"content") (Registry.Codec.packageMap Core.remotePackageCodec)
  $ CJ.record

lockEntryCodec :: CJ.Codec LockEntry
lockEntryCodec = Codec.codec' decode encode
  where
  encode = case _ of
    FromPath lock -> CJ.encode pathLockCodec lock
    FromGit lock -> CJ.encode gitLockCodec lock
    FromRegistry lock -> CJ.encode registryLockCodec lock

  decode json =
    map FromPath (Codec.decode pathLockCodec json)
      <|> map FromGit (Codec.decode gitLockCodec json)
      <|> map FromRegistry (Codec.decode registryLockCodec json)

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

pathLockCodec :: CJ.Codec PathLock
pathLockCodec = Profunctor.dimap toRep fromRep $ CJ.named "PathLock" $ CJ.object
  $ CJ.recordProp (Proxy @"type") (constant pathLockType)
  $ CJ.recordProp (Proxy @"path") CJ.string
  $ CJ.recordProp (Proxy @"dependencies") (CJ.array PackageName.codec)
  $ CJ.record
  where
  toRep = Record.insert (Proxy @"type") pathLockType
  fromRep = Record.delete (Proxy @"type")

type GitLock =
  { url :: String
  , rev :: String
  , subdir :: Maybe FilePath
  , dependencies :: Array PackageName
  }

gitLockCodec :: CJ.Codec GitLock
gitLockCodec = Profunctor.dimap toRep fromRep $ CJ.named "GitLock" $ CJ.object
  $ CJ.recordProp (Proxy @"type") (constant gitLockType)
  $ CJ.recordProp (Proxy @"url") CJ.string
  $ CJ.recordProp (Proxy @"rev") CJ.string
  $ CJ.recordPropOptional (Proxy @"subdir") CJ.string
  $ CJ.recordProp (Proxy @"dependencies") (CJ.array PackageName.codec)
  $ CJ.record
  where
  toRep = Record.insert (Proxy @"type") gitLockType
  fromRep = Record.delete (Proxy @"type")

type RegistryLock =
  { version :: Version
  , integrity :: Sha256
  , dependencies :: Array PackageName
  }

registryLockCodec :: CJ.Codec RegistryLock
registryLockCodec = Profunctor.dimap toRep fromRep $ CJ.named "RegistryLock" $ CJ.object
  $ CJ.recordProp (Proxy @"type") (constant registryLockType)
  $ CJ.recordProp (Proxy @"version") Version.codec
  $ CJ.recordProp (Proxy @"integrity") Sha256.codec
  $ CJ.recordProp (Proxy @"dependencies") (CJ.array PackageName.codec)
  $ CJ.record
  where
  toRep = Record.insert (Proxy @"type") registryLockType
  fromRep = Record.delete (Proxy @"type")

constant :: String → CJ.Codec String
constant val = Codec.codec' decode encode
  where
  encode = CJ.encode CJ.string
  decode json = except $ CJ.decode CJ.string json >>= case _ of
    decoded | decoded == val -> pure val
    res -> Left $ CJ.DecodeError.basic $ "Unexpected value: " <> res
