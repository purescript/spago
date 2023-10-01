module Spago.Command.Ls (listPackages, listPackageSet, LsEnv(..), LsDepsArgs, LsPackagesArgs) where

import Spago.Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Foldable (elem)
import Data.Map (filterKeys)
import Data.Map as Map
import Data.Tuple.Nested (type (/\))
import Record as Record
import Registry.Internal.Codec (packageMap)
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Command.Fetch as Fetch
import Spago.Config (Package(..), PackageSet(..), Workspace, WorkspacePackage, PackageMap)
import Spago.Config as Config
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

type LsSetEnv =
  { packageDependencies :: Map PackageName PackageMap
  , logOptions :: LogOptions
  , workspace :: Workspace
  }

type LsEnv =
  { packageDependencies :: Map PackageName PackageMap
  , logOptions :: LogOptions
  , workspace :: Workspace
  , selected :: WorkspacePackage
  }

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
  { packageDependencies, selected } <- ask
  let
    dependencies = Fetch.getAllDependencies packageDependencies
    direct = (Map.keys <<< unwrap <<< _.dependencies <<< _.package) selected
    directDependencies = filterKeys (_ `elem` direct) dependencies

  let packages = Map.toUnfoldable $ if transitive then dependencies else directDependencies
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
