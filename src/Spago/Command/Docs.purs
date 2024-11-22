module Spago.Command.Docs
  ( run
  , DocsEnv
  ) where

import Spago.Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Docs.Search.Config as DocConfig
import Docs.Search.IndexBuilder as IndexBuilder
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Node.Process as Process
import Spago.Command.Build as Build
import Spago.Command.Fetch as Fetch
import Spago.Config (Workspace)
import Spago.Config as Config
import Spago.Purs (Purs, DocsFormat(..))
import Spago.Purs as Purs

type DocsEnv a =
  { purs :: Purs
  , workspace :: Workspace
  , dependencies :: Fetch.PackageTransitiveDeps
  , logOptions :: LogOptions
  , rootPath :: RootPath
  , docsFormat :: DocsFormat
  , depsOnly :: Boolean
  , open :: Boolean
  | a
  }

run :: ∀ a. Spago (DocsEnv a) Unit
run = do
  logDebug "Running `spago docs`"
  logInfo "Generating documentation for the project. This might take a while..."
  { rootPath, workspace, dependencies, docsFormat, depsOnly, open } <- ask
  let
    globs = Build.getBuildGlobs
      { rootPath
      , withTests: true
      , selected: Config.getWorkspacePackages workspace.packageSet
      , dependencies: Fetch.toAllDependencies dependencies
      , depsOnly
      }

  result <- Purs.docs rootPath globs docsFormat
  case result of
    Left r -> die r.message
    _ -> pure unit

  when (docsFormat == Html) $ do
    liftAff $ IndexBuilder.run'
      { docsFiles: DocConfig.defaultDocsFiles
      , bowerFiles: DocConfig.defaultBowerFiles
      , generatedDocs: "./generated-docs/"
      , packageName: DocConfig.defaultPackageName
      , sourceFiles: DocConfig.defaultSourceFiles
      }

    currentDir <- liftEffect Process.cwd
    let link = "file://" <> currentDir <> "/generated-docs/html/index.html"
    logInfo $ "Link: " <> link

    when open do
      openFile link

openFile :: forall m. MonadAff m => String -> m Unit
openFile = liftAff <<< Promise.toAffE <<< runEffectFn1 openImpl

foreign import openImpl :: EffectFn1 String (Promise Unit)
