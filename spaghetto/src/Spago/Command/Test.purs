module Spago.Command.Test where

import Spago.Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Codec.Argonaut as CA
import Data.Map as Map
import Registry.PackageName as PackageName
import Spago.Command.Build as Build
import Spago.Command.Run (Node)
import Spago.Command.Run as Run
import Spago.Config (Package, Workspace, WorkspacePackage)
import Spago.Paths as Paths
import Spago.Purs (ModuleGraph(..), Purs)
import Spago.Purs as Purs

type TestEnv a =
  { logOptions :: LogOptions
  , workspace :: Workspace
  , selectedPackages :: NonEmptyArray SelectedTest
  , dependencies :: Map PackageName Package
  , node :: Node
  , purs :: Purs
  | a
  }

type SelectedTest =
  { execArgs :: Array String
  , moduleName :: String
  , selected :: WorkspacePackage
  }

run :: forall a. Spago (TestEnv a) Unit
run = do
  { workspace, logOptions, node, selectedPackages, dependencies, purs } <- ask
  void $ for selectedPackages \{ execArgs, moduleName, selected } -> do

    let
      name = selected.package.name
      runOptions =
        { successMessage: Just $ "Test succeeded for package \"" <> PackageName.print name <> "\"."
        , failureMessage: "Tests failed for package \"" <> PackageName.print name <> "\"."
        , executeDir: selected.path
        , sourceDir: Paths.cwd
        , execArgs
        , moduleName
        }

      runEnv = { logOptions, workspace, selected, node, runOptions, dependencies, purs }

    -- We check if the test module is included in the build and spit out a nice error if it isn't (see #383)
    let
      globs = Build.getBuildGlobs
        { withTests: true, selected: [ selected ], dependencies, depsOnly: false }
    maybeGraph <- runSpago { purs, logOptions } $ Purs.graph globs []
    case maybeGraph of
      Left err -> do
        logWarn $ "Could not decode the output of `purs graph`, error: " <> CA.printJsonDecodeError err
      Right (ModuleGraph moduleMap) -> do
        when (isNothing $ Map.lookup moduleName moduleMap) do
          die [ "Module '" <> moduleName <> "' not found! Are you including it in your build?" ]

    logInfo $ "Running tests for package: " <> PackageName.print name
    runSpago runEnv Run.run

-- TODO: it'd be nice to have a --coverage flag.
-- For triggering that we'd have to call purs asking for `sourcemaps`, then use `rimraf` and `c8`, likely as FFI bindings
-- See https://github.com/rowtype-yoga/purescript-yoga-json/blob/main/package.json
