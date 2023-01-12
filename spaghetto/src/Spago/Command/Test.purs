module Spago.Command.Test where

import Spago.Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Registry.PackageName as PackageName
import Spago.Command.Run (Node)
import Spago.Command.Run as Run
import Spago.Config (Workspace, WorkspacePackage)
import Spago.Paths as Paths

type TestEnv a =
  { logOptions :: LogOptions
  , workspace :: Workspace
  , selectedPackages :: NonEmptyArray SelectedTest
  , node :: Node
  | a
  }

type SelectedTest =
  { execArgs :: Array String
  , moduleName :: String
  , selected :: WorkspacePackage
  }

run :: forall a. Spago (TestEnv a) Unit
run = do
  { workspace, logOptions, node, selectedPackages } <- ask
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

      runEnv = { logOptions, workspace, selected, node, runOptions }

    -- TODO: graph
    --   -- We check if the test module is included in the build and spit out a nice error if it isn't (see #383)
    --   maybeGraph <- view (the @Graph)
    --   for_ maybeGraph $ \(ModuleGraph moduleMap) -> when (isNothing $ Map.lookup moduleName moduleMap) $
    --     die [ "Module '" <> (display . unModuleName) moduleName <> "' not found! Are you including it in your build?" ]

    logInfo $ "Running tests for package: " <> PackageName.print name
    runSpago runEnv Run.run
