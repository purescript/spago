module Spago.Command.Ls (listPackages, listPackageSet, LsEnv(..), LsDepsArgs, LsPackagesArgs) where

import Spago.Prelude

import Data.Array (replicate)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Foldable (elem, maximum)
import Data.Map (filterKeys)
import Data.Map as Map
import Data.Set (unions)
import Data.String.CodeUnits (fromCharArray, length)
import Data.Tuple.Nested (type (/\))
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Config (Dependencies(..), Package(..), PackageSet, Workspace, getPackageLocation)
import Spago.Config as Config
import Spago.Json (stringifyJson)

type LsPackagesArgs =
  { json :: Boolean
  }

type LsDepsArgs =
  { json :: Boolean
  , transitive :: Boolean
  }

type LsEnv =
  { dependencies :: PackageSet
  , logOptions :: LogOptions
  , workspace :: Workspace
  }

type JsonPackageOutput =
  { packageName :: String
  -- TODO: Was Json Value, has tag
  -- See https://github.com/purescript/spago/blob/ec22308d181340382494e5bd6ef9102a3c87c0bb/src/Spago/Types.hs#L38-L47
  , repo :: String
  , version :: String
  }

jsonPackageOutputCodec :: JsonCodec JsonPackageOutput
jsonPackageOutputCodec = CAR.object "JsonPackageOutput"
  { packageName: CA.string
  , repo: CA.string
  , version: CA.string
  }

listPackageSet :: LsPackagesArgs -> Spago LsEnv Unit
listPackageSet { json } = do
  logDebug "Running `listPackageSet`"
  { workspace } <- ask
  output $ formatPackageNames json (Map.toUnfoldable workspace.packageSet)

listPackages :: LsDepsArgs -> Spago LsEnv Unit
listPackages { transitive, json } = do
  logDebug "Running `listPackages`"
  { dependencies, workspace } <- ask
  let
    packagesToList = Map.toUnfoldable case transitive of
      true -> dependencies
      false ->
        let
          workspacePackages = Config.getWorkspacePackages workspace.packageSet
          toDependencyNames (Dependencies deps) = Map.keys deps
          directDependencies = unions $ ((toDependencyNames <<< _.dependencies <<< _.package) <$> workspacePackages)
        in
          filterKeys (_ `elem` directDependencies) dependencies
  case packagesToList of
    [] -> logWarn "There are no dependencies listed in your spago.dhall"
    _ -> output $ formatPackageNames json packagesToList

formatPackageNames :: forall a. Boolean -> Array (PackageName /\ Package) -> OutputFormat a
formatPackageNames json = case json of
  true -> OutputLines <<< formatPackageNamesJson
  false -> OutputLines <<< formatPackageNamesText
  where
  formatPackageNamesJson :: Array (PackageName /\ Package) -> Array String
  formatPackageNamesJson pkgs = (stringifyJson jsonPackageOutputCodec <<< asJson <$> pkgs)
    where
    asJson (name /\ package) =
      { packageName: PackageName.print name
      -- TODO: Do we want to stay backward compatible and have a tagged remote and local type?
      -- Or do we create a codec for package (with 4 variants)?
      , repo: "TODO"
      , version: showVersion package
      }

  -- | Format all the package names from the configuration
  formatPackageNamesText :: Array (PackageName /\ Package) -> Array String
  formatPackageNamesText pkgs =
    let
      -- TODO: Currently prints git/remote packages as local packages
      showLocation :: PackageName -> Package -> String
      showLocation _ (GitPackage gitPackage) = "Remote " <> surroundQuote gitPackage.git
      showLocation packageName package = "Local " <> surroundQuote (getPackageLocation packageName package)

      -- Calculated for indentation
      longestName = fromMaybe 0 $ maximum $ (length <<< PackageName.print <<< fst) <$> pkgs
      longestVersion = fromMaybe 0 $ maximum $ (length <<< showVersion <<< snd) <$> pkgs

      renderPkg :: PackageName /\ Package -> String
      renderPkg (packageName /\ package) =
        leftPad longestName (PackageName.print packageName)
          <> " "
          <> leftPad longestVersion (showVersion package)
          <> "   "
          <> showLocation packageName package
    in
      map renderPkg pkgs

  showVersion :: Package -> String
  showVersion (RegistryVersion version) = "v" <> Version.print version
  showVersion (LocalPackage _) = "local"
  showVersion (GitPackage _) = "git"
  showVersion (WorkspacePackage _) = "workspace"

  leftPad :: Int -> String -> String
  leftPad n s
    | length s < n = s <> (fromCharArray $ replicate (n - length s) ' ')
    | otherwise = s

  surroundQuote :: String -> String
  surroundQuote y = "\"" <> y <> "\""
