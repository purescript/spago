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

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Codec as Codec
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Common as CJ.Common
import Data.Codec.JSON.Record as CJ.Record
import Data.Foldable (elem)
import Data.Map (filterKeys)
import Data.Map as Map
import Record as Record
import Registry.Internal.Codec (packageMap)
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Command.Fetch as Fetch
import Spago.Config (BuildType(..), Package(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Paths as Paths
import Type.Proxy (Proxy(..))

type LsPackagesArgs =
  { json :: Boolean
  , pure :: Boolean
  }

type LsDepsArgs =
  { json :: Boolean
  , transitive :: Boolean
  , selectedPackage :: Maybe String
  , pure :: Boolean
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
      output $ OutputJson (CJ.Common.map CJ.string CJ.string) $ Map.fromFoldable keyValuePairs
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
  case workspace.packageSet.buildType of
    RegistrySolverBuild _extraPackages -> die "Cannot list the packages in the package set, as none is configured for the project."
    PackageSetBuild _info packageSet -> do
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

  packageCodec :: CJ.Codec { type :: String, value :: Package }
  packageCodec = CJ.named "Package" $ CJ.Record.object { type: CJ.string, value: innerCodec }
    where
    innerCodec = Codec.codec' decode encode

    registryVersionCodec = CJ.named "RegistryVersion" $ CJ.Record.object { version: Version.codec }

    encode = case _ of
      RegistryVersion x -> CJ.encode registryVersionCodec { version: x }
      GitPackage x -> CJ.encode Config.gitPackageCodec x
      LocalPackage x -> CJ.encode Config.localPackageCodec x
      WorkspacePackage x -> CJ.encode workspacePackageCodec x

    decode json =
      map (RegistryVersion <<< _.version) (Codec.decode registryVersionCodec json)
        <|> map GitPackage (Codec.decode Config.gitPackageCodec json)
        <|> map LocalPackage (Codec.decode Config.localPackageCodec json)
        <|> map WorkspacePackage (Codec.decode workspacePackageCodec json)

  workspacePackageCodec = Codec.codec' decode encode
    where
    decode _json = except $ Left $ CJ.DecodeError.basic "Decoding workspace packages is not supported."
    encode =
      CJ.encode
        ( CJ.named "WorkspacePackage" $ CJ.Record.object
            { path: CJ.string
            , package: Config.packageConfigCodec
            , hasTests: CJ.boolean
            }
        ) <<< Record.delete (Proxy @"doc")

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
