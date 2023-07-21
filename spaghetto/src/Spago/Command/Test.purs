module Spago.Command.Test where

import Spago.Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Registry.PackageName as PackageName
import Spago.Command.Run (Node)
import Spago.Command.Run as Run
import Spago.Config (Package, Workspace, WorkspacePackage)
import Spago.Paths as Paths
import Spago.Purs (Purs)

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

    logInfo $ "Running tests for package: " <> PackageName.print name
    runSpago runEnv Run.run

-- TODO: it'd be nice to have a --coverage flag.
-- For triggering that we'd have to call purs asking for `sourcemaps`, then use `rimraf` and `c8`, likely as FFI bindings
-- See https://github.com/rowtype-yoga/purescript-yoga-json/blob/main/package.json
