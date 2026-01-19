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
import Data.Codec.JSON.Strict as CJS
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
  , path :: RawFilePath
  }

type WorkspaceLockPackageEnv =
  { dependencies :: Core.Dependencies
  , build_plan :: Set PackageName
  }

lockfileCodec :: CJ.Codec Lockfile
lockfileCodec = CJ.named "Lockfile" $ CJS.objectStrict
  $ CJS.recordProp @"workspace" workspaceLockCodec
  $ CJS.recordProp @"packages" (Registry.Codec.packageMap lockEntryCodec)
  $ CJS.record

workspaceLockCodec :: CJ.Codec WorkspaceLock
workspaceLockCodec = CJ.named "WorkspaceLock" $ CJS.objectStrict
  $ CJS.recordProp @"packages" (Registry.Codec.packageMap dependenciesCodec)
  $ CJS.recordPropOptional @"package_set" packageSetCodec
  $ CJS.recordProp @"extra_packages" (Registry.Codec.packageMap Config.extraPackageCodec)
  $ CJS.record
  where
  dependenciesCodec = CJ.named "Dependencies" $ CJS.objectStrict
    $ CJS.recordProp @"path" CJ.string
    $ CJS.recordProp @"core" envCodec
    $ CJS.recordProp @"test" envCodec
    $ CJS.record

  envCodec = CJ.named "Environment" $ CJS.objectStrict
    $ CJS.recordProp @"dependencies" Config.dependenciesCodec
    $ CJS.recordProp @"build_plan" (CJ.Common.set PackageName.codec)
    $ CJS.record

packageSetCodec :: CJ.Codec PackageSetInfo
packageSetCodec = CJ.named "PackageSetInfo" $ CJS.objectStrict
  $ CJS.recordProp @"address" Config.setAddressCodec
  $ CJS.recordProp @"compiler" Range.codec
  $ CJS.recordProp @"content" (Registry.Codec.packageMap Core.remotePackageCodec)
  $ CJS.record

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
  { path :: RawFilePath
  , dependencies :: Array PackageName
  }

pathLockCodec :: CJ.Codec PathLock
pathLockCodec = Profunctor.dimap toRep fromRep $ CJ.named "PathLock" $ CJS.objectStrict
  $ CJS.recordProp @"type" (constant pathLockType)
  $ CJS.recordProp @"path" CJ.string
  $ CJS.recordProp @"dependencies" (CJ.array PackageName.codec)
  $ CJS.record
  where
  toRep = Record.insert (Proxy @"type") pathLockType
  fromRep = Record.delete (Proxy @"type")

type GitLock =
  { url :: String
  , rev :: String
  , subdir :: Maybe RawFilePath
  , dependencies :: Array PackageName
  }

gitLockCodec :: CJ.Codec GitLock
gitLockCodec = Profunctor.dimap toRep fromRep $ CJ.named "GitLock" $ CJS.objectStrict
  $ CJS.recordProp @"type" (constant gitLockType)
  $ CJS.recordProp @"url" CJ.string
  $ CJS.recordProp @"rev" CJ.string
  $ CJS.recordPropOptional @"subdir" CJ.string
  $ CJS.recordProp @"dependencies" (CJ.array PackageName.codec)
  $ CJS.record
  where
  toRep = Record.insert (Proxy @"type") gitLockType
  fromRep = Record.delete (Proxy @"type")

type RegistryLock =
  { version :: Version
  , integrity :: Sha256
  , dependencies :: Array PackageName
  }

registryLockCodec :: CJ.Codec RegistryLock
registryLockCodec = Profunctor.dimap toRep fromRep $ CJ.named "RegistryLock" $ CJS.objectStrict
  $ CJS.recordProp @"type" (constant registryLockType)
  $ CJS.recordProp @"version" Version.codec
  $ CJS.recordProp @"integrity" Sha256.codec
  $ CJS.recordProp @"dependencies" (CJ.array PackageName.codec)
  $ CJS.record
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
