module Spago.Command.Auth where

import Spago.Prelude

import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String
import Node.Path as Path
import Registry.SSH as SSH
import Spago.Command.Fetch (FetchEnv)
import Spago.Config as Config
import Spago.FS as FS

type AuthArgs = { keyPath :: FilePath }

run :: AuthArgs -> Spago (FetchEnv _) Unit
run { keyPath } = do
  logDebug $ "Authenticating with key at path " <> keyPath
  let
    -- we don't want to accidentally read the private key, so we always point to the public
    path = case String.stripSuffix (Pattern ".pub") keyPath of
      Just _rest -> keyPath
      Nothing -> keyPath <> ".pub"

  newOwner <- FS.exists path >>= case _ of
    false -> do
      die $ "Cannot read public key at path " <> show path <> ": file does not exist."
    true -> do
      content <- FS.readTextFile path
      let result = SSH.parsePublicKey content
      case result of
        Left err -> die [ "Could not parse SSH public key. Error was:", err ]
        Right public -> pure $ SSH.publicKeyToOwner public
  logDebug $ "Parsed owner: " <> show (unwrap newOwner)

  { workspace } <- ask
  { doc, package, configPath } <- case workspace.selected, workspace.rootPackage of
    Just { doc, package, path: packagePath }, _ -> pure { doc, package, configPath: Path.concat [ packagePath, "spago.yaml" ] }
    Nothing, Just rootPackage -> pure { doc: workspace.doc, package: rootPackage, configPath: "spago.yaml" }
    Nothing, Nothing -> die "No package was selected. Please select a package with the -p flag"

  case package.publish of
    Nothing -> die
      [ "The package you are trying to authenticate for is not set up for publishing."
      , "Please set the `publish` field in the spago.yaml file - see the docs for more info:"
      , "https://github.com/purescript/spago#the-configuration-file"
      ]
    Just { owners: maybeOwners } -> do
      let currentOwners = fromMaybe [] maybeOwners
      case Array.elem newOwner currentOwners of
        true -> logWarn "Selected key is already present in the config file."
        false -> do
          logInfo $ "Adding selected key to the list of the owners: " <> path
          Config.addOwner configPath doc newOwner
          logSuccess "The selected key has been added to the list of the owners."
          logInfo "Once you publish a new version with this configuration you'll be able to transfer and unpublish packages using this key."
