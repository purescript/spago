module Spago.BuildInfo where

import Spago.Prelude

import Data.Array.NonEmpty as NEA
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
  -- , commitHash :: String -- TODO add commit hash to build info
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
      , packages: map mkPackageBuildInfo $ NEA.toUnfoldable $ Config.getWorkspacePackages workspace.packageSet
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
  [ "-- @inline export packages always"
  , "-- @inline export pursVersion always"
  , "-- @inline export spagoVersion always"
  , "module Spago.Generated.BuildInfo where"
  , ""
  , "packages :: " <> recordType
  , "packages ="
  , "  { " <> String.joinWith "\n  , " (map renderPackage packages) <> "\n  }"
  , ""
  , "pursVersion :: String"
  , "pursVersion = \"" <> pursVersion <> "\""
  , ""
  , "spagoVersion :: String"
  , "spagoVersion = \"" <> currentSpagoVersion <> "\""
  , ""
  ]
  where
  recordType = "{ " <> String.joinWith ", " (map renderPackageType packages) <> " }"
  renderPackage p = "\"" <> p.name <> "\": \"" <> p.version <> "\""
  renderPackageType p = "\"" <> p.name <> "\" :: String"
  currentSpagoVersion = BuildInfo.packages."spago-bin"

buildInfoPath ∷ FilePath
buildInfoPath = Path.concat [ Paths.localCachePath, "BuildInfo.purs" ]

mkPackageBuildInfo :: WorkspacePackage -> { name :: String, version :: String }
mkPackageBuildInfo { package } =
  { name: PackageName.print package.name
  , version: fromMaybe "0.0.0" $ map Version.print (map _.version package.publish)
  }
