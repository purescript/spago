module Spago.Command.Docs
  ( run
  , DocsEnv
  , OpenDocs(..)
  , Search(..)
  ) where

import Spago.Prelude

import Control.Alternative as Alternative
import Data.Array as Array
import Data.Map as Map
import Data.Set as Set
import Data.Set.NonEmpty as NonEmptySet
import Data.Tuple as Tuple
import Docs.Search.IndexBuilder as IndexBuilder
import Docs.Search.Main (defaultDocsFiles, defaultBowerFiles, defaultSourceFiles)
import Docs.Search.Config (defaultPackageName)
import Node.Process as Process
import Registry.PackageName as PackageName
import Spago.BuildInfo as BuildInfo
import Spago.Cmd as Cmd
import Spago.Command.Build (getBuildGlobs)
import Spago.Config (Package(..), WithTestGlobs(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Git (Git)
import Spago.Paths as Paths
import Spago.Psa as Psa
import Spago.Purs (Purs, DocsFormat(..))
import Spago.Purs as Purs
import Spago.Purs.Graph as Graph

type DocsEnv =
  { purs :: Purs
  , workspace :: Workspace
  , dependencies :: Map PackageName Package
  , logOptions :: LogOptions
  , docsFormat :: DocsFormat
  , open :: OpenDocs
  , search :: Search
  , depsOnly :: Boolean
  }

-- | Flag to include search feature in generated html
data Search = NoSearch | AddSearch

derive instance Eq Search

-- | Flag to open generated HTML documentation in browser
data OpenDocs = NoOpenDocs | DoOpenDocs

derive instance Eq OpenDocs

run :: Spago DocsEnv Unit
run = do
  logDebug "Running `spago docs`"
  logInfo "Generating documentation for the project. This might take a while..."
  { workspace, dependencies, docsFormat, depsOnly, search, open } <- ask
  let
    globs = getBuildGlobs
      { withTests: true
      , selected:
          case workspace.selected of
            Just p -> [ p ]
            Nothing -> Config.getWorkspacePackages workspace.packageSet
      , dependencies
      , depsOnly
      }

  _ <- Purs.docs globs docsFormat

  when (docsFormat == Html) $ do
    when (search == AddSearch) $ do
      logInfo "Making the documentation searchable..."
      liftAff $ IndexBuilder.run'
        { noPatch: false
        , generatedDocs: "./generatedDocs/"
        , sourceFiles: defaultSourceFiles
        , bowerFiles: defaultBowerFiles
        , docsFiles: defaultDocsFiles
        , packageName: defaultPackageName
        }
    
    currentDir <- liftEffect Process.cwd
    let link = "file://" <> currentDir <> "/generated-docs/html/index.html"
    logInfo $ "Link: " <> link

-- TODO: reimplement --open

-- The haskell version used:
-- https://github.com/rightfold/open-browser/tree/master

-- The purescript version should probably FFI to a cross platform library such as:
-- https://github.com/sindresorhus/open
{-
when (open == OpenDocs) $ do
  logInfo "Opening in browser..."
  open link
-}
