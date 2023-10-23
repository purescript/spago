module Spago.Command.Test where

import Spago.Prelude

import Registry.PackageName as PackageName
import Spago.Command.Fetch as Fetch
import Spago.Command.Run (Node)
import Spago.Command.Run as Run
import Spago.Config (Workspace, WorkspacePackage)
import Spago.Paths as Paths
import Spago.Purs (Purs)

type TestEnv a =
  { logOptions :: LogOptions
  , workspace :: Workspace
  , selectedPackages :: NonEmptyArray SelectedTest
  , dependencies :: Fetch.PackageTransitiveDeps
  , node :: Node
  , purs :: Purs
  | a
  }

type SelectedTest =
  { exec_args :: Array String
  , moduleName :: String
  , selected :: WorkspacePackage
  }

run :: forall a. Spago (TestEnv a) Unit
run = do
  { workspace, logOptions, node, selectedPackages, dependencies, purs } <- ask
  void $ for selectedPackages \{ exec_args, moduleName, selected } -> do

    let
      name = selected.package.name
      runOptions =
        { successMessage: Just $ "Test succeeded for package \"" <> PackageName.print name <> "\"."
        , failureMessage: "Tests failed for package \"" <> PackageName.print name <> "\"."
        , executeDir: Paths.cwd
        , sourceDir: Paths.cwd
        , exec_args
        , moduleName
        }

      runEnv = { logOptions, workspace, selected, node, runOptions, dependencies, purs }

    logInfo $ "Running tests for package: " <> PackageName.print name
    runSpago runEnv Run.run

-- TODO: it'd be nice to have a --coverage flag.
-- For triggering that we'd have to call purs asking for `sourcemaps`, then use `rimraf` and `c8`, likely as FFI bindings
-- See https://github.com/rowtype-yoga/purescript-yoga-json/blob/main/package.json
