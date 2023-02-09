module Spago.Command.Ls (listPackages, listPackageSet, LsEnv(..), JsonFlag(..), IncludeTransitive(..)) where

import Data.String.CodeUnits
import Spago.Prelude

import Data.Array (replicate)
import Data.Foldable (maximum)
import Data.Map as Map
import Data.Traversable (traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Registry.Types (PackageName(..))
import Registry.Version as Version
import Registry.PackageName as PackageName
import Spago.Command.Build (BuildEnv)
import Spago.Command.Registry (RegistryEnv)
import Spago.Config (Package(..), PackageSet(..), getPackageLocation)

data IncludeTransitive = IncludeTransitive | NoIncludeTransitive
data JsonFlag = JsonOutputNo | JsonOutputYes

type LsEnv =
  { dependencies :: PackageSet
  , logOptions :: LogOptions
  }

data JsonPackageOutput = JsonPackageOutput
  { json_packageName :: String
  , json_repo :: String
  , json_version :: String
  }

-- deriving (Eq, Show, Generic)

-- instance ToJSON JsonPackageOutput where
--   toJSON = Json.genericToJSON Json.defaultOptions
--     { fieldLabelModifier = drop 5
--     }

-- encodeJsonPackageOutput :: JsonPackageOutput -> Text
-- encodeJsonPackageOutput = LT.toStrict . LT.decodeUtf8 . Json.encode

listPackageSet :: JsonFlag -> Spago LsEnv Unit
listPackageSet jsonFlag = do
  logDebug "Running `listPackageSet`"
  { dependencies: packagesDB } <- ask
  output $ formatPackageNames jsonFlag (Map.toUnfoldable packagesDB)

-- let packagesToList = Map.toUnfoldable dependencies
-- case packagesToList of
--   -- [] -> logWarn "There are no dependencies listed in your spago.dhall"
--   [] -> pure unit
--   _ -> output $ OutputLines $ formatPackageNames jsonFlag packagesToList

listPackages :: IncludeTransitive -> JsonFlag -> Spago LsEnv Unit
listPackages includeTransitive jsonFlag = do
  logDebug "Running `listPackages`"
  { dependencies } <- ask
  let packagesToList = Map.toUnfoldable dependencies
  case packagesToList of
    -- [] -> logWarn "There are no dependencies listed in your spago.dhall"
    [] -> pure unit
    _ -> output $ formatPackageNames jsonFlag []

formatPackageNames :: forall a. JsonFlag -> Array (PackageName /\ Package) -> OutputFormat a
formatPackageNames = case _ of
  JsonOutputYes -> OutputLines <<< formatPackageNamesJson
  JsonOutputNo -> OutputLines <<< formatPackageNamesText
  where
  -- | Format all the packages from the config in JSON
  formatPackageNamesJson :: Array (PackageName /\ Package) -> Array String
  formatPackageNamesJson pkgs = []

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

  -- data Package
  --   = RegistryVersion Version
  --   | GitPackage GitPackage
  --   | LocalPackage LocalPackage
  --   | WorkspacePackage WorkspacePackage

  -- | Format all the package names from the configuration
  formatPackageNamesText :: Array (PackageName /\ Package) -> Array String
  formatPackageNamesText pkgs =
    let
      showVersion (RegistryVersion version) = Version.print version
      showVersion (LocalPackage _) = "local"
      showVersion (GitPackage _) = "git"
      showVersion (WorkspacePackage _) = "workspace"

      -- showLocation (Remote { repo: Repo repo }) = "Remote " <> surroundQuote repo
      -- showLocation (Local { localPath }) = "Local " <> surroundQuote localPath
      -- TODO: Probably not what we want
      showLocation :: PackageName -> Package -> String
      showLocation packageName package = getPackageLocation packageName package

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
