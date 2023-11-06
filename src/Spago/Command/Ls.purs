module Spago.Command.Ls
  ( listPaths
  , listPackages
  , listPackageSet
  , LsEnv(..)
  , LsPathsArgs
  , LsDepsArgs
  , LsPackagesArgs
  ) where

import Spago.Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Foldable (elem, traverse_)
import Data.Map (filterKeys)
import Data.Map as Map
import Data.Tuple.Nested (type (/\))
import Record as Record
import Registry.Internal.Codec (packageMap)
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Command.Fetch as Fetch
import Spago.Config (Package(..), PackageSet(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Paths as Paths
import Type.Proxy (Proxy(..))

type LsPackagesArgs =
  { json :: Boolean
  }

type LsDepsArgs =
  { json :: Boolean
  , transitive :: Boolean
  , selectedPackage :: Maybe String
  }

type LsDepsOpts =
  { json :: Boolean
  , transitive :: Boolean
  }

type LsPathsArgs =
  { json :: Boolean
  }

type LsSetEnv =
  { dependencies :: Fetch.PackageTransitiveDeps
  , logOptions :: LogOptions
  , workspace :: Workspace
  }

type LsEnv =
  { dependencies :: Fetch.PackageTransitiveDeps
  , logOptions :: LogOptions
  , workspace :: Workspace
  , selected :: WorkspacePackage
  }

listPaths :: LsPathsArgs -> Spago { logOptions :: LogOptions } Unit
listPaths { json } = do
  logDebug "Running `listPaths`"
  case json of
    true ->
      output $ OutputJson (CAC.map CA.string CA.string) $ Map.fromFoldable keyValuePairs
    false ->
      output $ OutputTable
        { titles: [ "Name", "Path" ]
        , rows: (\(Tuple k v) -> [ k, v ]) <$> keyValuePairs
        }
  where
  keyValuePairs =
    [ Tuple "Global cache path" Paths.globalCachePath
    , Tuple "Global registry path" Paths.registryPath
    , Tuple "Global registry index path" Paths.registryIndexPath
    , Tuple "Global package sets path" Paths.packageSetsPath
    , Tuple "Global database path" Paths.databasePath
    , Tuple "Local cache path" Paths.localCachePath
    , Tuple "Local cache packages path" Paths.localCachePackagesPath
    ]

-- TODO: add LICENSE field

listPackageSet :: LsPackagesArgs -> Spago LsSetEnv Unit
listPackageSet { json } = do
  logDebug "Running `listPackageSet`"
  { workspace } <- ask
  case workspace.packageSet of
    Registry _extraPackages -> die "Cannot list the packages in the package set, as none is configured for the project."
    PackageSet packageSet -> do
      let packages = Map.toUnfoldable packageSet
      case json of
        true -> formatPackagesJson packages
        false -> formatPackagesTable packages

listPackages :: LsDepsOpts -> Spago LsEnv Unit
listPackages { transitive, json } = do
  logDebug "Running `listPackages`"
  { dependencies, selected } <- ask
  let
    allDependencies = Fetch.toAllDependencies dependencies
    direct = (Map.keys <<< unwrap <<< _.dependencies <<< _.package) selected
    directDependencies = filterKeys (_ `elem` direct) allDependencies

  let packages = Map.toUnfoldable $ if transitive then allDependencies else directDependencies
  case packages of
    [] -> logWarn "There are no dependencies listed in your configuration"
    _ -> case json of
      true -> formatPackagesJson packages
      false -> formatPackagesTable packages

formatPackagesJson :: forall m. MonadEffect m => Array (Tuple PackageName Package) -> m Unit
formatPackagesJson packages = output $ OutputJson (packageMap packageCodec) (map wrapPackage $ Map.fromFoldable packages)
  where
  wrapPackage value =
    { value
    , type: case value of
        RegistryVersion _ -> "registry"
        GitPackage _ -> "git"
        LocalPackage _ -> "local"
        WorkspacePackage _ -> "workspace"
    }

  packageCodec :: JsonCodec { type :: String, value :: Package }
  packageCodec = CAR.object "Package" { type: CA.string, value: innerCodec }
    where
    innerCodec = CA.codec' decode encode

    registryVersionCodec = CAR.object "RegistryVersion" { version: Version.codec }

    encode = case _ of
      RegistryVersion x -> CA.encode registryVersionCodec { version: x }
      GitPackage x -> CA.encode Config.gitPackageCodec x
      LocalPackage x -> CA.encode Config.localPackageCodec x
      WorkspacePackage x -> CA.encode workspacePackageCodec x

    decode json =
      map (RegistryVersion <<< _.version) (CA.decode registryVersionCodec json)
        <|> map GitPackage (CA.decode Config.gitPackageCodec json)
        <|> map LocalPackage (CA.decode Config.localPackageCodec json)
        <|> map WorkspacePackage (CA.decode workspacePackageCodec json)

  workspacePackageCodec = CA.codec' decode encode
    where
    decode _json = Left CA.MissingValue
    encode =
      CA.encode
        ( CAR.object "WorkspacePackage"
            { path: CA.string
            , package: Config.packageConfigCodec
            , hasTests: CA.boolean
            }
        ) <<< Record.delete (Proxy :: _ "doc")

formatPackagesTable :: forall m. MonadEffect m => Array (Tuple PackageName Package) -> m Unit
formatPackagesTable pkgs = output $ OutputTable
  { titles: [ "Package", "Version", "Location" ]
  , rows: toRow <$> pkgs
  }
  where
  toRow :: (PackageName /\ Package) -> Array String
  toRow (packageName /\ package) =
    [ PackageName.print packageName
    , showVersion package
    , showLocation package
    ]

  showLocation :: Package -> String
  showLocation = case _ of
    RegistryVersion _ -> "-"
    GitPackage { git } -> git
    LocalPackage { path } -> path
    WorkspacePackage { path } -> path

  showVersion :: Package -> String
  showVersion = case _ of
    RegistryVersion version -> Version.print version
    GitPackage { ref } -> ref
    LocalPackage _ -> "local"
    WorkspacePackage _ -> "workspace"
