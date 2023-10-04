module Spago.Command.Docs
  ( run
  , DocsEnv
  ) where

import Spago.Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Node.Process as Process
import Spago.Command.Build as Build
import Spago.Command.Fetch as Fetch
import Spago.Config (Workspace)
import Spago.Purs (Purs, DocsFormat(..))
import Spago.Purs as Purs

type DocsEnv =
  { purs :: Purs
  , workspace :: Workspace
  , dependencies :: Fetch.PackageTransitiveDeps
  , logOptions :: LogOptions
  , docsFormat :: DocsFormat
  , depsOnly :: Boolean
  , open :: Boolean
  }

run :: Spago DocsEnv Unit
run = do
  logDebug "Running `spago docs`"
  logInfo "Generating documentation for the project. This might take a while..."
  { workspace, dependencies, docsFormat, depsOnly, open } <- ask
  let
    globs = Build.getBuildGlobs
      { withTests: true
      , selected: Build.AllWorkspaceGlobs workspace.packageSet
      , dependencies: Fetch.toAllDependencies dependencies
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

    when open do
      openFile link

openFile :: forall m. MonadAff m => String -> m Unit
openFile = liftAff <<< Promise.toAffE <<< runEffectFn1 openImpl

foreign import openImpl :: EffectFn1 String (Promise Unit)
