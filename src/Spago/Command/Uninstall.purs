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
import Registry.PackageName as PackageName
import Spago.Config (Dependencies(..), PackageConfig, Workspace)
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
    toContext
      :: FilePath
      -> PackageConfig
      -> Either
           PackageName
           { configPath :: FilePath
           , name :: PackageName
           , deps :: Dependencies
           , sourceOrTestString :: String
           , modifyConfig :: Dependencies -> PackageConfig -> PackageConfig
           }
    toContext configPath pkgConfig
      | args.testDeps = case pkgConfig.test of
          Nothing ->
            Left pkgConfig.name
          Just { dependencies } ->
            Right
              { configPath
              , name: pkgConfig.name
              , deps: dependencies
              , sourceOrTestString: "test"
              , modifyConfig: \newDeps packageConfig ->
                  packageConfig
                    { test = packageConfig.test <#> \t ->
                        t { dependencies = newDeps }
                    }
              }
      | otherwise =
          Right
            { configPath
            , name: pkgConfig.name
            , deps: pkgConfig.dependencies
            , sourceOrTestString: "source"
            , modifyConfig: \newDeps packageConfig ->
                packageConfig { dependencies = newDeps }
            }
  missingTestConfigOrContext <- case workspace.selected of
    Just p -> pure $ toContext p.path p.package
    Nothing -> do
      case workspace.rootPackage of
        Nothing ->
          die "No package was selected. Please select a package."
        Just p ->
          pure $ toContext "spago.yaml" p
  case missingTestConfigOrContext of
    Left pkgName ->
      logWarn $ "Could not uninstall test dependencies for " <> PackageName.print pkgName <> " because it does not have a test configuration."
    Right context -> do
      logDebug $ "Existing " <> context.sourceOrTestString <> " dependencies are: " <> (Array.intercalate ", " $ foldlWithIndex (\k a _ -> Array.snoc a $ PackageName.print k) [] $ unwrap context.deps)
      let
        { warn, removed, newDeps } = foldl deleteOrWarn init args.dependenciesToRemove
          where
          init = { warn: [], removed: [], newDeps: unwrap context.deps }
          deleteOrWarn acc next
            | Just (Tuple _ newDeps) <- Map.pop next acc.newDeps = acc { newDeps = newDeps, removed = Array.snoc acc.removed next }
            | otherwise = acc { warn = Array.snoc acc.warn next }
      for_ (NEA.fromArray warn) \undeclaredPkgs ->
        logWarn
          [ "The following packages cannot be uninstalled because they are not declared in the package's " <> context.sourceOrTestString <> " dependencies:"
          , "  " <> NEA.intercalate ", " (map PackageName.print undeclaredPkgs)
          ]

      case NEA.fromArray removed of
        Nothing ->
          logInfo $ "The package config for " <> PackageName.print context.name <> " was not updated."
        Just removed' -> do
          logInfo
            [ "Removing the following " <> context.sourceOrTestString <> " dependencies:"
            , "  " <> NEA.intercalate ", " (map PackageName.print removed')
            ]
          editSpagoYaml context.name context.configPath \config ->
            config { package = map (context.modifyConfig (Dependencies newDeps)) config.package }

editSpagoYaml :: PackageName -> String -> (Core.Config -> Core.Config) -> Spago UninstallEnv Unit
editSpagoYaml p path f = do
  content <- liftAff $ FS.readYamlDocFile Core.configCodec path
  case content of
    Left err ->
      die $ "Error decoding package config for package " <> PackageName.print p <> ":\n" <> err
    Right { yaml: config } ->
      liftAff $ FS.writeYamlFile Core.configCodec path $ f config
