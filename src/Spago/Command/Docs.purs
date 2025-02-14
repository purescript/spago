module Spago.Command.Docs
  ( run
  , DocsEnv
  ) where

import Spago.Prelude

import Control.Monad.Reader (runReaderT)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Set as Set
import Docs.Search.Config as DocConfig
import Docs.Search.IndexBuilder as IndexBuilder
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Node.Process as Process
import Spago.Command.Build as Build
import Spago.Command.Fetch as Fetch
import Spago.Command.Graph as Graph
import Spago.Config (Workspace)
import Spago.Config as Config
import Spago.Log (LogVerbosity(..))
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
  env@{ rootPath, workspace, dependencies, docsFormat, depsOnly, open } <- ask

  let
    globs = Build.getBuildGlobs
      { rootPath
      , withTests: true
      , selected: Config.getWorkspacePackages workspace.packageSet
      , dependencies: Fetch.toAllDependencies dependencies
      , depsOnly
      }

  Purs.docs
    { root: rootPath
    , globs
    , format: docsFormat
    , quiet: env.logOptions.verbosity == LogQuiet
    }
    <#> lmap _.message
    >>= rightOrDie
    # void

  when (docsFormat == Html) $ do
    { moduleGraph } <- Graph.graphModules'
    liftAff $ IndexBuilder.run
      { docsFiles: DocConfig.defaultDocsFiles
      , pursJsonFiles: DocConfig.defaultPursFiles
      , generatedDocs: "./generated-docs/"
      , workspacePackages: Set.fromFoldable $ map _.package.name $ Config.getWorkspacePackages workspace.packageSet
      , moduleGraph
      , log: \x -> runReaderT (logInfo x) env
      , die: \x -> runReaderT (die x) env
      }

    currentDir <- liftEffect Process.cwd
    let link = "file://" <> currentDir <> "/generated-docs/html/index.html"
    logInfo $ "Link: " <> link

    when open do
      openFile link

openFile :: forall m. MonadAff m => String -> m Unit
openFile = liftAff <<< Promise.toAffE <<< runEffectFn1 openImpl

foreign import openImpl :: EffectFn1 String (Promise Unit)
