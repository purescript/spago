module Spago.Command.Graph where

import Spago.Prelude
import Spago.Env

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.FilePath.Glob as Glob
import qualified System.IO as Sys

import qualified Spago.Packages as Packages


graphNotSupportedError :: IsString p => p
graphNotSupportedError = "`spago graph` only works on PureScript v0.14.0 or higher. Please upgrade your compiler"

graphModules :: HasBuildEnv env => RIO env ()
graphModules = do
  logDebug "Running `spago graph modules`"
  view (the @Graph) >>= \case
    Nothing -> logError graphNotSupportedError
    Just moduleGraph -> output $ jsonToTextPretty moduleGraph

graphPackages :: HasBuildEnv env => RIO env ()
graphPackages = do
  logDebug "Running `spago graph packages`"
  maybeGraph <- view (the @Graph)
  case maybeGraph of
    Nothing -> logError graphNotSupportedError
    Just (ModuleGraph moduleGraph) -> do
      BuildOptions{ depsOnly } <- view (the @BuildOptions)
      Config{ name, configSourcePaths } <- view (the @Config)
      deps <- Packages.getProjectDeps
      let Packages.Globs{..} = Packages.getGlobs deps depsOnly configSourcePaths
      let
        matchesGlob :: Sys.FilePath -> SourcePath -> Bool
        matchesGlob path sourcePath =
          Glob.match (Glob.compile (Text.unpack (unSourcePath sourcePath))) path

        getPackageFromPath :: Text -> PackageName
        getPackageFromPath path =
          let maybeDependency = fmap fst
                $ find (\(_, sourcePath) -> matchesGlob (Text.unpack path) sourcePath)
                $ Map.toList depsGlobs
          in case maybeDependency of
            -- if we didn't match a dependency path, then it must come from our project
            Nothing -> PackageName name
            Just dep -> dep

        -- We first build a mapping from ModuleName to PackageName, so we can look it up easily
        moduleToPackageMap :: Map ModuleName PackageName
        moduleToPackageMap = Map.map (\ModuleGraphNode{ graphNodePath } -> getPackageFromPath graphNodePath) moduleGraph

        -- Then we take the module graph Map, and:
        -- * key becomes package
        -- * value.path becomes the path of the package
        -- * value.depends now contains packages
        moduleToMaybePackage :: (ModuleName, ModuleGraphNode) -> Maybe (Map PackageName ModuleGraphNode)
        moduleToMaybePackage a@(moduleName, ModuleGraphNode{ graphNodeDepends, graphNodePath }) =
          case Map.lookup moduleName moduleToPackageMap of
            Nothing -> traceShow a Nothing
            Just pkgName -> Just $ Map.singleton pkgName $
              ModuleGraphNode (toPackagePath graphNodePath)
                $ Set.fromList
                $ mapMaybe (\m -> ModuleName . packageName <$> Map.lookup m moduleToPackageMap)
                $ Set.toList graphNodeDepends
          where
            toPackagePath = id -- FIXME

        -- We fold all the singletons together merging their dependencies
        packageGraph
          = Map.unionsWith (<>)
          $ mapMaybe moduleToMaybePackage
          $ Map.toList moduleGraph

      output $ jsonToTextPretty packageGraph