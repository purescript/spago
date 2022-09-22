module Spago.Command.Build where

import Spago.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.String as String
import Data.Tuple as Tuple
import Node.FS.Aff as FS
import Node.Path as Path
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Config (Package(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Generated.BuildInfo as BuildInfo
import Spago.Paths as Paths

type BuildEnv a =
  { purs :: FilePath
  , git :: FilePath
  , dependencies :: Map PackageName Package
  , logOptions :: LogOptions
  , workspace :: Workspace
  | a
  }

type BuildOptions =
  { depsOnly :: Boolean
  , pursArgs :: Array String
  }

run :: forall a. BuildOptions -> Spago (BuildEnv a) Unit
run opts = do
  logInfo "Building..."
  void $ liftAff $ spawnFromParentWithStdin { command: "purs", args: [ "--version" ], input: Nothing, cwd: Nothing }
  { purs, dependencies, workspace } <- ask
  let command = purs
  let dependencyGlobs = map (Tuple.uncurry Config.sourceGlob) (Map.toUnfoldable dependencies)

  -- Here we select the right globs for a monorepo setup
  let
    workspacePackageGlob :: WorkspacePackage -> String
    workspacePackageGlob p = Config.sourceGlob p.package.name (WorkspacePackage p)
    projectSources =
      if opts.depsOnly then []
      else case workspace.selected of
        Just p -> [ workspacePackageGlob p ]
        -- We just select all the workspace package globs, because it's (1) intuitive and (2) backwards compatible
        Nothing -> map workspacePackageGlob (Config.getWorkspacePackages workspace.packageSet)
  logDebug $ "Project sources: " <> show projectSources

  -- Put together the build info
  let buildInfoPath = Path.concat [ Paths.localCachePath, "BuildInfo.purs" ]
  let specialGlobs = [ buildInfoPath ]
  let
    buildInfo =
      { pursVersion: "TODO"
      , packages: map mkPackageBuildInfo case workspace.selected of
          Just p -> [ p ]
          Nothing -> Config.getWorkspacePackages workspace.packageSet
      }
  liftAff $ FS.writeTextFile UTF8 buildInfoPath (mkBuildInfo buildInfo)

  -- Finally, build:
  let args = [ "compile" ] <> opts.pursArgs <> projectSources <> dependencyGlobs <> specialGlobs
  logDebug [ "Running command: purs", "With args: " <> show args ]
  void $ liftAff $ spawnFromParentWithStdin
    { command
    , args
    , input: Nothing
    , cwd: Nothing
    }
  logSuccess "Build succeeded."

-- Inspiration: https://github.com/sbt/sbt-buildinfo
-- See https://github.com/purescript/spago/issues/599
type BuildInfo =
  { packages ::
      Array
        { name :: String
        , version :: String
        }
  -- , buildTime :: String -- TODO
  , pursVersion :: String
  }

-- TODO: use tidy-codegen eventually
mkBuildInfo :: BuildInfo -> String
mkBuildInfo { packages, pursVersion } = String.joinWith "\n"
  [ "module Spago.Generated.BuildInfo where"
  , ""
  , "buildInfo :: { packages :: Array { name :: String, version :: String }, pursVersion :: String, spagoVersion :: String }"
  , "buildInfo ="
  , "  { packages: [" <> String.joinWith ", " (map renderPackage packages) <> "]"
  , "  , pursVersion: \"" <> pursVersion <> "\""
  , "  , spagoVersion: \"" <> currentSpagoVersion <> "\""
  , "  }"
  , ""
  ]
  where
  renderPackage p = "{ name: \"" <> p.name <> "\", version: \"" <> p.version <> "\"}"

-- TODO: maybe more elegant
currentSpagoVersion :: String
currentSpagoVersion = fromMaybe "0.0.1" $ map _.version $ Array.head BuildInfo.buildInfo.packages

mkPackageBuildInfo :: WorkspacePackage -> { name :: String, version :: String }
mkPackageBuildInfo { package } =
  { name: PackageName.print package.name
  , version: Version.printVersion package.version
  }
