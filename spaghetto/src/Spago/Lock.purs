module Spago.Lock where

import Spago.Prelude

import Data.Argonaut.Core (Json)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CA.Record
import Data.String as String
import Parsing (Parser)
import Parsing as Parsing
import Parsing.Combinators as Parsing.Combinators
import Parsing.String as Parsing.String
import Registry.Internal.Codec as Registry.Internal
import Registry.Sha256 as Sha256
import Registry.Version as Version

type Lockfile =
  { lockfile :: Version
  , packages :: Map PackageName LockEntry
  }

lockfileCodec :: JsonCodec Lockfile
lockfileCodec = CA.Record.object "Lockfile"
  { lockfile: Version.codec
  , packages: Registry.Internal.packageMap lockEntryCodec
  }

data LockEntry
  -- | A package in the local workspace, which we won't hash; workspace packages
  -- | are essentially mutable.
  = WorkspaceLock { path :: FilePath }
  -- | A Git checkout of a package, in which we know the commit SHA-1.
  | GitLock { url :: String, rev :: String, subdir :: Maybe FilePath }
  -- | A registry package at a specific version, in which we can read the hash
  -- | from the registry metadata.
  | RegistryLock { version :: Version, hash :: Sha256 }

derive instance Eq LockEntry

lockEntryCodec :: JsonCodec LockEntry
lockEntryCodec = CA.codec' decode encode
  where
  encode :: LockEntry -> Json
  encode = CA.encode CA.string <<< printLockEntry

  decode :: Json -> Either JsonDecodeError LockEntry
  decode json = do
    string <- CA.decode CA.string json
    lmap CA.TypeMismatch (parseLockEntry string)

printLockEntry :: LockEntry -> String
printLockEntry = case _ of
  WorkspaceLock { path } ->
    workspacePrefix <> path
  GitLock { url, rev, subdir } -> do
    let hashlock = gitPrefix <> url <> "#" <> rev
    maybe hashlock (\path -> hashlock <> "?subdir=" <> path) subdir
  RegistryLock { version, hash } ->
    registryPrefix <> Version.print version <> "#" <> Sha256.print hash

parseLockEntry :: String -> Either String LockEntry
parseLockEntry = lmap Parsing.parseErrorMessage <<< flip Parsing.runParser lockEntryParser

lockEntryParser :: Parser String LockEntry
lockEntryParser = do
  let prefixError = "Expected an entry prefixed with one of: " <> String.joinWith ", " prefixes

  prefix <-
    Parsing.Combinators.choice
      [ Parsing.String.string workspacePrefix
      , Parsing.String.string gitPrefix
      , Parsing.String.string registryPrefix
      , Parsing.fail prefixError
      ]

  if prefix == workspacePrefix then do
    path <- Parsing.String.rest
    pure $ WorkspaceLock { path }

  else if prefix == gitPrefix then do
    Tuple url _ <- Parsing.String.anyTill (Parsing.String.char '#') <|> Parsing.fail "Expected a URL terminated by a #"
    -- Git SHA-1 commit hashes are 40 characters long.
    rev <- Parsing.String.takeN 40 <|> Parsing.fail "Expected a 40-character commit hash"
    -- The subdir key is optional
    subdir <- Parsing.Combinators.choice
      [ do
          _ <- Parsing.String.eof
          pure Nothing
      , do
          _ <- Parsing.String.string "?subdir="
          dir <- Parsing.String.rest
          pure $ Just dir
      , do
          extra <- Parsing.String.rest
          Parsing.fail $ "Expected end of string or a ?subdir= entry but received: " <> extra
      ]
    pure $ GitLock { url, rev, subdir }

  else if prefix == registryPrefix then do
    Tuple rawVersion _ <- Parsing.String.anyTill (Parsing.String.char '#')
    version <- case Version.parse rawVersion of
      Left error -> Parsing.fail error
      Right version -> pure version
    rawHash <- Parsing.String.rest
    hash <- case Sha256.parse rawHash of
      Left error -> Parsing.fail error
      Right hash -> pure hash
    pure $ RegistryLock { version, hash }

  else
    Parsing.fail prefixError

prefixes :: Array String
prefixes = [ workspacePrefix, gitPrefix, registryPrefix ]

workspacePrefix :: String
workspacePrefix = "workspace:"

gitPrefix :: String
gitPrefix = "git:"

registryPrefix :: String
registryPrefix = "registry:"
