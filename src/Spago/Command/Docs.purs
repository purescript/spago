module Spago.Command.Docs
  ( run
  , DocsEnv
  ) where

import Spago.Prelude

import Node.Process as Process
import Spago.Command.Build (getBuildGlobs)
import Spago.Config (Package, Workspace)
import Spago.Config as Config
import Spago.Purs (Purs, DocsFormat(..))
import Spago.Purs as Purs

type DocsEnv =
  { purs :: Purs
  , workspace :: Workspace
  , dependencies :: Map PackageName Package
  , logOptions :: LogOptions
  , docsFormat :: DocsFormat
  , depsOnly :: Boolean
  }

run :: Spago DocsEnv Unit
run = do
  logDebug "Running `spago docs`"
  logInfo "Generating documentation for the project. This might take a while..."
  { workspace, dependencies, docsFormat, depsOnly } <- ask
  let
    globs = getBuildGlobs
      { withTests: true
      , selected: Config.getWorkspacePackages workspace.packageSet
      , dependencies
      , depsOnly
      }

  result <- Purs.docs globs docsFormat
  case result of
    Left err -> die err.message
    Right _ -> pure unit

  when (docsFormat == Html) $ do
    currentDir <- liftEffect Process.cwd
    let link = "file://" <> currentDir <> "/generated-docs/html/index.html"
    logInfo $ "Link: " <> link

-- TODO: reimplement --open

-- The haskell version used:
-- https://github.com/rightfold/open-browser/tree/master

-- For purescript version looking at FFI to this library:
-- https://github.com/sindresorhus/open
