module Spago.Command.Ls (listPackages, listPackageSet, LsEnv(..), JsonFlag(..), IncludeTransitive(..)) where

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
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Config (Dependencies(..), Package(..), PackageSet, Workspace, getPackageLocation)
import Spago.Config as Config

data IncludeTransitive = IncludeTransitive | NoIncludeTransitive
data JsonFlag = JsonOutputNo | JsonOutputYes

type LsEnv =
  { dependencies :: PackageSet
  , logOptions :: LogOptions
  , workspace :: Workspace
  }

type JsonPackageOutput =
  { packageName :: String
  , repo :: String -- TODO: Was Value, has tag
  , version :: String
  }

jsonPackageOutputCodec :: JsonCodec JsonPackageOutput
jsonPackageOutputCodec = CAR.object "JsonPackageOutput"
  { packageName: CA.string
  , repo: CA.string
  , version: CA.string
  }

listPackageSet :: JsonFlag -> Spago LsEnv Unit
listPackageSet jsonFlag = do
  logDebug "Running `listPackageSet`"
  { workspace } <- ask
  output $ formatPackageNames jsonFlag (Map.toUnfoldable workspace.packageSet)

listPackages :: IncludeTransitive -> JsonFlag -> Spago LsEnv Unit
listPackages packagesFilter jsonFlag = do
  logDebug "Running `listPackages`"
  { dependencies, workspace } <- ask
  let
    packagesToList = Map.toUnfoldable case packagesFilter of
      IncludeTransitive -> dependencies
      NoIncludeTransitive ->
        let
          workspacePackages = Config.getWorkspacePackages workspace.packageSet
          toDependencyNames (Dependencies deps) = Map.keys deps
          directDependencies = unions $ ((toDependencyNames <<< _.dependencies <<< _.package) <$> workspacePackages)
        in
          filterKeys (\k -> k `elem` directDependencies) dependencies
  case packagesToList of
    [] -> logWarn "There are no dependencies listed in your spago.dhall"
    _ -> output $ formatPackageNames jsonFlag packagesToList

formatPackageNames :: forall a. JsonFlag -> Array (PackageName /\ Package) -> OutputFormat a
formatPackageNames = case _ of
  JsonOutputYes -> OutputLines <<< formatPackageNamesText -- TODO: formatPackageNamesJson
  JsonOutputNo -> OutputLines <<< formatPackageNamesText
  where
  -- TODO: JSON encoding
  -- | Format all the packages from the config in JSON
  formatPackageNamesJson :: Array (PackageName /\ Package) -> Array JsonPackageOutput
  formatPackageNamesJson pkgs = (asJson <$> pkgs)
    where
    asJson (name /\ package) = { packageName: PackageName.print name, repo: "TODO", version: "TODO" }

  -- let
  --   asJson (packageName /\ (Package { location: loc@(Remote { repo, version }) })) = JsonPackageOutput
  --     { json_packageName = print packageName
  --     , json_repo = toJSON loc
  --     , json_version = version
  --     }
  --   asJson (packageName /\ Package { location: loc@(Local { localPath }) }) = JsonPackageOutput
  --     { json_packageName = print packageName
  --     , json_repo = toJSON loc
  --     , json_version = "local"
  --     }
  -- in
  --   map (encodeJsonPackageOutput.asJson) pkgs

  -- | Format all the package names from the configuration
  formatPackageNamesText :: Array (PackageName /\ Package) -> Array String
  formatPackageNamesText pkgs =
    let
      showVersion (RegistryVersion version) = Version.print version
      showVersion (LocalPackage _) = "local"
      showVersion (GitPackage _) = "git"
      showVersion (WorkspacePackage _) = "workspace"

      -- TODO: Currently prints all as local packages
      showLocation :: PackageName -> Package -> String
      showLocation _ (GitPackage gitPackage) = "Remote " <> surroundQuote gitPackage.git
      showLocation packageName package = "Local " <> surroundQuote (getPackageLocation packageName package)

      longestName = fromMaybe 0 $ maximum $ (length <<< PackageName.print <<< fst) <$> pkgs
      longestVersion = fromMaybe 0 $ maximum $ (length <<< showVersion <<< snd) <$> pkgs

      renderPkg :: PackageName /\ Package -> String
      renderPkg (packageName /\ package) = leftPad longestName (PackageName.print packageName) <> " "
        <> leftPad longestVersion (showVersion package)
        <> "   "
        <> showLocation packageName package
    in
      map renderPkg pkgs

  leftPad :: Int -> String -> String
  leftPad n s
    | length s < n = s <> (fromCharArray $ replicate (n - length s) ' ')
    | otherwise = s

  surroundQuote :: String -> String
  surroundQuote y = "\"" <> y <> "\""
