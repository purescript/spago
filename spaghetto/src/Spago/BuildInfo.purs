module Spago.BuildInfo where

import Spago.Prelude

import Data.Array as Array
import Data.String as String
import Node.Path as Path
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Config (Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.FS as FS
import Spago.Generated.BuildInfo as BuildInfo
import Spago.Paths as Paths
import Spago.Purs (Purs)

-- Inspiration: https://github.com/sbt/sbt-buildinfo
-- See https://github.com/purescript/spago/issues/599
type BuildInfo =
  { packages ::
      Array
        { name :: String
        , version :: String
        }
  -- , buildTime :: String -- TODO add build time to build info
  , pursVersion :: String
  }

type BuildInfoEnv a =
  { workspace :: Workspace
  , logOptions :: LogOptions
  , purs :: Purs
  | a
  }

writeBuildInfo :: forall a. Spago (BuildInfoEnv a) Unit
writeBuildInfo = do
  { workspace, purs } <- ask
  let
    buildInfo =
      { pursVersion: Version.print purs.version
      , packages: map mkPackageBuildInfo case workspace.selected of
          Just p -> [ p ]
          Nothing -> Config.getWorkspacePackages workspace.packageSet
      }
    buildInfoString = mkBuildInfo buildInfo
    writeIt = FS.writeTextFile buildInfoPath buildInfoString
  -- try to write the new build info only if necessary
  FS.exists buildInfoPath >>= case _ of
    false -> writeIt
    true -> do
      currentContent <- FS.readTextFile buildInfoPath
      when (currentContent /= buildInfoString) do
        writeIt

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

buildInfoPath ∷ FilePath
buildInfoPath = Path.concat [ Paths.localCachePath, "BuildInfo.purs" ]

-- TODO: maybe more elegant
currentSpagoVersion :: String
currentSpagoVersion = fromMaybe "0.0.0" $ map _.version $ Array.head BuildInfo.buildInfo.packages

mkPackageBuildInfo :: WorkspacePackage -> { name :: String, version :: String }
mkPackageBuildInfo { package } =
  { name: PackageName.print package.name
  , version: fromMaybe "0.0.0" $ map Version.print (package.publish >>= _.version)
  }
