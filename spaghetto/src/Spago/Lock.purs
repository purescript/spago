module Spago.Lock
  ( Lockfile
  , lockfileCodec
  , LockEntry(..)
  , lockEntryCodec
  , PathLock
  , GitLock
  , RegistryLock
  ) where

import Spago.Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.Profunctor as Profunctor
import Record as Record
import Registry.Internal.Codec as Registry.Codec
import Registry.Sha256 as Sha256
import Registry.Version as Version
import Type.Proxy (Proxy(..))

type Lockfile =
  { packages :: Map PackageName LockEntry
  }

lockfileCodec :: JsonCodec Lockfile
lockfileCodec = CA.Record.object "Lockfile"
  { packages: Registry.Codec.packageMap lockEntryCodec
  }

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
  }

gitLockCodec :: JsonCodec GitLock
gitLockCodec = Profunctor.dimap toRep fromRep $ CA.object "GitLock"
  $ CA.recordProp (Proxy :: _ "type") (constant gitLockType)
  $ CA.recordProp (Proxy :: _ "url") CA.string
  $ CA.recordProp (Proxy :: _ "rev") CA.string
  $ CA.recordPropOptional (Proxy :: _ "subdir") CA.string
  $ CA.record
  where
  toRep = Record.insert (Proxy :: _ "type") gitLockType
  fromRep = Record.delete (Proxy :: _ "type")

type RegistryLock =
  { version :: Version
  , integrity :: Sha256
  }

registryLockCodec :: JsonCodec RegistryLock
registryLockCodec = Profunctor.dimap toRep fromRep $ CA.object "RegistryLock"
  $ CA.recordProp (Proxy :: _ "type") (constant registryLockType)
  $ CA.recordProp (Proxy :: _ "version") Version.codec
  $ CA.recordProp (Proxy :: _ "integrity") Sha256.codec
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
