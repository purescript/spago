module Spago.Command.Test where

import Spago.Prelude

import Spago.Command.Run as Run

run :: forall a. Spago (Run.RunEnv a) Unit
run = do
  runEnv <- ask
  let
    testEnv =
      runEnv
        { runOptions
            { successMessage = Just "Test succeeded."
            , failureMessage = "Tests failed."
            , executeDir = runEnv.selected.path
            }
        }

  -- TODO: graph
  --   -- We check if the test module is included in the build and spit out a nice error if it isn't (see #383)
  --   maybeGraph <- view (the @Graph)
  --   for_ maybeGraph $ \(ModuleGraph moduleMap) -> when (isNothing $ Map.lookup moduleName moduleMap) $
  --     die [ "Module '" <> (display . unModuleName) moduleName <> "' not found! Are you including it in your build?" ]

  runSpago testEnv Run.run
