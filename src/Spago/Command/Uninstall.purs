module Spago.Command.Uninstall
  ( run
  , UninstallEnv
  , UninstallArgs
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map as Map
import Data.Set.NonEmpty as NonEmptySet
import Node.Path as Path
import Registry.PackageName as PackageName
import Spago.Config (Dependencies, PackageConfig, Workspace)
import Spago.Config as Config
import Spago.Config as Core
import Spago.FS as FS

type UninstallArgs =
  { dependenciesToRemove :: Set PackageName
  , testDeps :: Boolean
  }

type UninstallEnv =
  { workspace :: Workspace
  , logOptions :: LogOptions
  }

run :: UninstallArgs -> Spago UninstallEnv Unit
run args = do
  logDebug "Running `spago uninstall`"
  { workspace } <- ask
  let
    modifyConfig
      :: FilePath
      -> YamlDoc Core.Config
      -> String
      -> NonEmptyArray PackageName
      -> Spago UninstallEnv Unit
    modifyConfig configPath yamlDoc sourceOrTestString = \removedPackages -> do
      logInfo
        [ "Removing the following " <> sourceOrTestString <> " dependencies:"
        , "  " <> intercalateMap ", " PackageName.print removedPackages
        ]
      logDebug $ "Editing config file at path: " <> configPath
      liftEffect $ Config.removePackagesFromConfig yamlDoc args.testDeps $ NonEmptySet.fromFoldable1 removedPackages
      liftAff $ FS.writeYamlDocFile configPath yamlDoc
      where
      intercalateMap sep f = _.val <<< foldl go { init: true, val: "" }
        where
        go acc next = { init: false, val: if acc.init then f next else acc.val <> sep <> f next }

    toContext
      :: FilePath
      -> YamlDoc Core.Config
      -> PackageConfig
      -> Either
           PackageName
           { name :: PackageName
           , deps :: Dependencies
           , sourceOrTestString :: String
           , modifyDoc :: NonEmptyArray PackageName -> Spago UninstallEnv Unit
           }
    toContext configPath yamlDoc pkgConfig
      | args.testDeps = case pkgConfig.test of
          Nothing ->
            Left pkgConfig.name
          Just { dependencies } -> do
            let sourceOrTestString = "test"
            Right
              { name: pkgConfig.name
              , deps: dependencies
              , sourceOrTestString
              , modifyDoc: modifyConfig configPath yamlDoc sourceOrTestString
              }
      | otherwise = do
          let sourceOrTestString = "source"
          Right
            { name: pkgConfig.name
            , deps: pkgConfig.dependencies
            , sourceOrTestString
            , modifyDoc: modifyConfig configPath yamlDoc sourceOrTestString
            }
  missingTestConfigOrContext <- case workspace.selected of
    Just p ->
      pure $ toContext (Path.concat [ p.path, "spago.yaml" ]) p.doc p.package
    Nothing -> do
      case workspace.rootPackage of
        Nothing ->
          die "No package was selected. Please select a package."
        Just p ->
          pure $ toContext "spago.yaml" workspace.doc p
  case missingTestConfigOrContext of
    Left pkgName ->
      logWarn $ "Could not uninstall test dependencies for " <> PackageName.print pkgName <> " because it does not have a test configuration."
    Right context -> do
      logDebug $ "Existing " <> context.sourceOrTestString <> " dependencies are: " <> (Array.intercalate ", " $ foldlWithIndex (\k a _ -> Array.snoc a $ PackageName.print k) [] $ unwrap context.deps)
      let
        { warn, removed } = foldl separate init args.dependenciesToRemove
          where
          init = { warn: [], removed: [] }

          separate :: _ -> PackageName -> _
          separate acc next
            | Map.member next $ unwrap context.deps = acc { removed = Array.snoc acc.removed next }
            | otherwise = acc { warn = Array.snoc acc.warn next }
      for_ (NEA.fromArray warn) \undeclaredPkgs ->
        logWarn
          [ "The following packages cannot be uninstalled because they are not declared in the package's " <> context.sourceOrTestString <> " dependencies:"
          , "  " <> NEA.intercalate ", " (map PackageName.print undeclaredPkgs)
          ]

      case NEA.fromArray removed of
        Nothing ->
          logInfo $ "The package config for " <> PackageName.print context.name <> " was not updated."
        Just removed' ->
          context.modifyDoc removed'

