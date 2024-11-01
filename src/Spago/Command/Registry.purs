module Spago.Command.Registry where

import Spago.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.DateTime (DateTime(..))
import Data.Formatter.DateTime as DateTime
import Data.Map as Map
import Data.String (Pattern(..))
import Data.String as String
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Registry.Internal.Codec as Internal
import Registry.Internal.Codec as Internal.Codec
import Registry.Internal.Format as Internal.Format
import Registry.Metadata as Metadata
import Registry.Operation as Operation
import Registry.PackageName as PackageName
import Registry.SSH as SSH
import Registry.Version as Version
import Spago.Command.Fetch (FetchEnv)
import Spago.Config as Config
import Spago.Db as Db
import Spago.FS as FS
import Spago.Git as Git
import Spago.Json as Json
import Spago.Registry (RegistryEnv)
import Spago.Registry as Registry

type RegistrySearchArgs =
  { package :: String
  , json :: Boolean
  }

-- TODO: I guess we could also search in (1) the tags and (2) the description
search :: RegistrySearchArgs -> Spago (RegistryEnv _) Unit
search { package: searchString, json } = do
  logInfo $ "Searching for " <> show searchString <> " in the Registry package names..."
  metadataFiles <- Registry.listMetadataFiles

  let matches = Array.filter (String.contains (Pattern searchString)) (Array.mapMaybe (String.stripSuffix (Pattern ".json")) metadataFiles)

  if Array.null matches then do
    logError "Did not find any packages matching the search string."
  else do
    -- We have only the match names, at least we get the time of the last release to be even a little useful
    infos <- map (Map.fromFoldable <<< Array.catMaybes) $ for matches \match -> case PackageName.parse match of
      Left err -> do
        logWarn $ "Couldn't parse package name: " <> err
        pure Nothing
      Right packageName -> Registry.getMetadata packageName >>= case _ of
        Left err -> do
          logWarn $ "Couldn't read metadata for package " <> PackageName.print packageName <> ", error: " <> err
          pure Nothing
        Right (Metadata meta) -> pure $ Just $ case Map.findMax meta.published of
          Nothing -> Tuple packageName { version: Nothing, publishedTime: Nothing }
          Just { key: version, value: { publishedTime } } -> Tuple packageName { version: Just version, publishedTime: Just publishedTime }

    -- Finally print all this stuff
    logInfo "Use `spago registry info $package` to get more details on a package."
    output $ case json of
      true ->
        let
          infoDataCodec = CJ.named "InfoData" $ CJ.Record.object
            { publishedTime: CJ.Record.optional Internal.Codec.iso8601DateTime
            , version: CJ.Record.optional Version.codec
            }
        in
          OutputJson (Internal.packageMap infoDataCodec) infos
      false -> OutputTable
        { titles: [ "NAME", "VERSION", "PUBLISHED TIME" ]
        , rows: infos # Map.toUnfoldable # map \(Tuple name { version, publishedTime }) ->
            [ PackageName.print name
            , maybe "-" Version.print version
            , maybe "-" (DateTime.format Internal.Format.iso8601DateTime) publishedTime
            ]
        }

type RegistryInfoArgs =
  { package :: String
  , json :: Boolean
  }

info :: RegistryInfoArgs -> Spago (RegistryEnv _) Unit
info { package, json } = do
  packageName <- case PackageName.parse package of
    Left err -> die [ toDoc "Could not parse package name, error:", indent (toDoc $ show err) ]
    Right name -> pure name

  Registry.getMetadata packageName >>= case _ of
    Left err -> do
      logDebug err
      die $ "Could not find package " <> PackageName.print packageName
    Right meta -> do
      -- We just print out the metadata file
      output case json of
        true -> OutputJson Metadata.codec meta
        false -> OutputYaml Metadata.codec meta

type RegistryPackageSetsArgs =
  { latest :: Boolean
  , json :: Boolean
  }

packageSets :: RegistryPackageSetsArgs -> Spago (RegistryEnv _) Unit
packageSets { latest, json } = do
  availableSets <- Registry.listPackageSets

  let
    sets = case latest of
      false -> availableSets
      true ->
        -- here we need to keep only the highest version of all the sets with the same compiler version
        Array.fromFoldable
          $ Map.values
          $
            foldl
              ( \acc newSet -> case Map.lookup newSet.compiler acc of
                  Nothing -> Map.insert newSet.compiler newSet acc
                  Just { version } -> case newSet.version > version of
                    true -> Map.insert newSet.compiler newSet acc
                    false -> acc
              )
              Map.empty
              availableSets

  output case json of
    true -> OutputJson (CJ.array Db.packageSetCodec) sets
    false -> OutputTable
      { titles: [ "VERSION", "DATE", "COMPILER" ]
      , rows: sets # map \{ version, date, compiler } ->
          [ Version.print version
          , DateTime.format Internal.Format.iso8601Date $ DateTime date bottom
          , Version.print compiler
          ]
      }

type RegistryTransferArgs = { privateKeyPath :: String }

transfer :: RegistryTransferArgs -> Spago (FetchEnv _) Unit
transfer { privateKeyPath } = do
  logDebug $ "Running package transfer"
  { workspace, offline } <- ask

  selected <- case workspace.selected of
    Just s -> pure s
    Nothing ->
      let
        workspacePackages = Config.getWorkspacePackages workspace.packageSet
      in
        -- If there's only one package, select that one
        case NEA.length workspacePackages of
          1 -> pure $ NEA.head workspacePackages
          _ -> do
            logDebug $ unsafeStringify workspacePackages
            die
              [ toDoc "No package was selected for running. Please select (with -p) one of the following packages:"
              , indent (toDoc $ map _.package.name workspacePackages)
              ]

  newLocation <- case selected.package.publish >>= _.location of
    Just loc -> pure loc
    Nothing -> die
      -- TODO: once we have automatic detection for git remotes we should try that first.
      [ "The package does not have a location set in the config file: add a valid one in `package.publish`."
      , "See the configuration file's documentation: https://github.com/purescript/spago#the-configuration-file"
      ]

  _owners <- case selected.package.publish >>= _.owners of
    Just owners | Array.length owners > 0 -> pure owners
    _ -> die
      [ "The package does not have any owners set in the config file."
      , "Please run `spago auth` to add your SSH public key to the owners list in the spago.yaml file."
      ]

  -- Check that the git tree is clean - since the transfer will obey the new content
  -- of the config file, it makes sense to have it commited before transferring
  Git.getStatus Nothing >>= case _ of
    Left _err -> do
      die $ toDoc
        [ toDoc "Could not verify whether the git tree is clean. Error was:"
        , indent _err
        ]
    Right statusResult | statusResult /= "" ->
      die $ toDoc
        [ toDoc "The git tree is not clean. Please commit or stash these files:"
        , indent $ toDoc (String.split (String.Pattern "\n") statusResult)
        ]
    _ -> pure unit

  -- Has the package ever been published before? We pull the metadata to verify that.
  -- Note! This getMetadata is going through two layers of caching:
  --   1. the registry is only fetched every 15 mins
  --   2. the metadata is then cached in the db for 15 mins
  -- When we transfer a package we want to make sure we have the latest everything,
  -- so we bypass both caches here.
  local (_ { offline = OnlineBypassCache }) (Registry.getMetadata selected.package.name) >>= case _ of
    Left err -> do
      logDebug err
      die
        [ "Could not find package '" <> PackageName.print selected.package.name <> "' in the Registry Index. Has it ever been published?"
        , "If not, please run `spago publish` first. Otherwise this is a bug - please report it on the Spago repo."
        ]
    Right (Metadata { location }) -> do
      -- We have a package, now need to check that the new location is different from the current one
      when (newLocation == location) do
        die
          [ "Cannot transfer package: the new location is the same as the current one."
          , "Please edit the `publish.location` field of your `spago.yaml` with the new location."
          ]

      -- We construct the payload that we'll later sign
      let dataToSign = { name: selected.package.name, newLocation }
      let rawPayload = Json.stringifyJson Operation.transferCodec dataToSign

      key <- getPrivateKeyForSigning privateKeyPath
      -- We have a key! We can sign the payload with it, and submit the whole package to the Registry
      let signature = SSH.sign key rawPayload

      -- At this point we check if the offline flag has been set. If it has, we abort the operation.
      -- Crucially, this is done _after_ the signing, which allows us to test that too.
      case offline of
        Offline -> die [ "Cannot perform Registry operations while offline." ]
        _ -> Registry.submitRegistryOperation $ Operation.Authenticated
          { signature
          , rawPayload
          , payload: Operation.Transfer dataToSign
          }

getPrivateKeyForSigning :: forall e. FilePath -> Spago (LogEnv e) SSH.PrivateKey
getPrivateKeyForSigning privateKeyPath = do
  -- If all is well we read in the private key
  privateKey <- try (FS.readTextFile privateKeyPath) >>= case _ of
    Right key -> pure key
    Left err -> do
      logDebug $ show err
      die "Could not read the private key at the given path. Please check it and try again."

  let
    decodeKeyInteractive { requiresPassword, attemptsLeft } = do
      case requiresPassword of
        -- If there are no attempts yet we first try to decode the key without a passphrase, silently.
        -- In case we succeed then happy days, can just proceed. If not, we move to asking the user for
        -- the key.
        false -> do
          case SSH.parsePrivateKey { key: privateKey, passphrase: Nothing } of
            Right key -> pure key
            Left _ -> do
              decodeKeyInteractive { requiresPassword: true, attemptsLeft }
        true -> do
          let prompt = "Enter passphrase for " <> privateKeyPath <> ": "
          passphrase <- liftEffect $ runEffectFn1 question prompt

          case SSH.parsePrivateKey { key: privateKey, passphrase: Just passphrase } of
            Left SSH.RequiresPassphrase -> case attemptsLeft of
              0 -> die [ "Too many incorrect attempts, exiting." ]
              _ -> do
                logError "The passphrase you entered is incorrect. Please trygain."
                decodeKeyInteractive { requiresPassword: true, attemptsLeft: attemptsLeft - 1 }
            Left err -> die [ toDoc "Could not parse the private key:", indent $ toDoc $ SSH.printPrivateKeyParseError err ]
            Right key -> pure key

  decodeKeyInteractive { requiresPassword: false, attemptsLeft: 3 }

type RegistryUnpublishArgs = { version :: Version, reason :: Maybe String }

unpublish :: RegistryUnpublishArgs -> Spago (RegistryEnv _) Unit
unpublish _a = do -- { version, reason } = do
  logError "Unpublishing packages is not supported yet."
  die [ "Please contact the maintainers if you need to unpublish a package." ]

-- We have custom FFI here because we want to ask for the passphrase in the terminal,
-- and the stock ReadLine implementation is not good at passwords
foreign import question :: EffectFn1 String String
