module Spago.Command.Build
  ( run
  , BuildEnv
  , getBuildGlobs
  ) where

import Spago.Prelude

import Control.Alternative as Alternative
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.Set as Set
import Data.Traversable (sequence)
import Data.Tuple as Tuple
import Record as Record
import Spago.BuildInfo as BuildInfo
import Spago.Cmd as Cmd
import Spago.Command.Fetch as Fetch
import Spago.Config (Package(..), PackageMap, WithTestGlobs(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Git (Git)
import Spago.Psa as Psa
import Spago.Psa.Types as PsaTypes
import Spago.Purs (Purs)
import Spago.Purs.Graph as Graph

type BuildEnv a =
  { purs :: Purs
  , git :: Git
  , dependencies :: Fetch.PackageTransitiveDeps
  , logOptions :: LogOptions
  , workspace :: Workspace
  , psaCliFlags :: PsaTypes.PsaOutputOptions
  , pedanticPackages :: Boolean
  | a
  }

type BuildOptions =
  { depsOnly :: Boolean
  , pursArgs :: Array String
  , jsonErrors :: Boolean
  }

run :: BuildOptions -> Spago (BuildEnv _) Unit
run opts = do
  logInfo "Building..."
  { dependencies
  , workspace
  , logOptions
  , psaCliFlags
  , pedanticPackages
  } <- ask

  BuildInfo.writeBuildInfo

  -- find the output flag and die if it's there - Spago handles it
  when (isJust $ Cmd.findFlag { flags: [ "-o", "--output" ], args: opts.pursArgs }) do
    die
      [ "Can't pass `--output` option directly to purs."
      , "Use the --output flag for Spago, or add it to your config file."
      ]
  let
    addOutputArgs args = case workspace.buildOptions.output of
      Nothing -> args
      Just output -> args <> [ "--output", output ]

  -- find the `--json-errors` flag and die if it's there - Spago handles it
  when (isJust $ Cmd.findFlag { flags: [ "--json-errors" ], args: opts.pursArgs }) do
    die
      [ "Can't pass `--json-errors` option directly to purs."
      , "Use the --json-errors flag for Spago."
      ]

  {-
  TODO: before, then, else
      buildAction globs = do
        let action = buildBackend globs >> (fromMaybe (pure ()) maybePostBuild)
        runCommands "Before" beforeCommands
        action `onException` (runCommands "Else" elseCommands)
        runCommands "Then" thenCommands
  -}

  when (isJust $ Cmd.findFlag { flags: [ "-g", "--codegen" ], args: opts.pursArgs }) do
    die
      [ "Can't pass the `--codegen` option to purs, Spago already does that for you."
      , "Remove the argument to make this error go away!"
      ]
  let
    args = (addOutputArgs opts.pursArgs) <>
      [ "--codegen"
      , "corefn,docs" <> case workspace.backend of
          -- If there's no backend specified then we do compile to JS, otherwise skip the target.
          -- Note: sourcemaps are JS specific, so they go together
          Nothing -> ",js,sourcemaps"
          Just _ -> ""
      ]

  let
    allDependencies = Fetch.toAllDependencies dependencies
    selectedPackages = case workspace.selected of
      Just p -> NEA.singleton p
      Nothing -> Config.getWorkspacePackages workspace.packageSet
    graphGlobs = getBuildGlobs
      { dependencies: allDependencies
      , depsOnly: opts.depsOnly
      , withTests: true
      , selected: selectedPackages
      }
    globs = getBuildGlobs
      { dependencies: case workspace.selected of
          Just p -> unsafeFromJust $ Map.lookup p.package.name dependencies
          Nothing -> allDependencies
      , depsOnly: opts.depsOnly
      , withTests: true
      , selected: selectedPackages
      }
  pathDecisions <- liftEffect $ sequence $ Psa.toPathDecisions
    { allDependencies
    , selectedPackages: NEA.toArray selectedPackages
    , psaCliFlags
    , censorLibWarnings: workspace.buildOptions.censorLibWarnings
    }
  let
    psaArgs =
      { color: logOptions.color
      , jsonErrors: opts.jsonErrors
      , decisions: join pathDecisions
      , statVerbosity: fromMaybe Psa.defaultStatVerbosity workspace.buildOptions.statVerbosity
      }

  Psa.psaCompile globs args psaArgs

  case workspace.backend of
    Nothing -> pure unit
    Just backend -> do
      logInfo $ "Compiling with backend \"" <> backend.cmd <> "\""
      logDebug $ "Running command `" <> backend.cmd <> "`"
      let
        moreBackendArgs = case backend.args of
          Just as | Array.length as > 0 -> as
          _ -> []
      Cmd.exec backend.cmd (addOutputArgs moreBackendArgs) Cmd.defaultExecOptions >>= case _ of
        Left err -> do
          logDebug $ show err
          die [ "Failed to build with backend " <> backend.cmd ]
        Right _r ->
          logSuccess "Backend build succeeded."

  let
    pedanticPkgs = NEA.toArray selectedPackages # Array.mapMaybe \p -> do
      let reportSrc = pedanticPackages || (fromMaybe false $ p.package.build >>= _.pedantic_packages)
      let reportTest = pedanticPackages || (fromMaybe false $ p.package.test >>= _.pedantic_packages)
      Alternative.guard (reportSrc || reportTest)
      pure $ Tuple p { reportSrc, reportTest }
  unless (Array.null pedanticPkgs || opts.depsOnly) do
    logInfo $ "Looking for unused and undeclared transitive dependencies..."
    eitherGraph <- Graph.runGraph graphGlobs opts.pursArgs
    graph <- either die pure eitherGraph
    env <- ask
    checkResults <- map Array.fold $ for pedanticPkgs \(Tuple selected options) -> do
      Graph.toImportErrors selected options <$> runSpago (Record.union { selected } env) (Graph.checkImports graph)
    unless (Array.null checkResults) do
      die $ Graph.formatImportErrors checkResults

-- TODO: if we are building with all the packages (i.e. selected = Nothing),
-- then we could use the graph to remove outdated modules from `output`!

type BuildGlobsOptions =
  { withTests :: Boolean
  , depsOnly :: Boolean
  , selected :: NonEmptyArray WorkspacePackage
  , dependencies :: PackageMap
  }

getBuildGlobs :: BuildGlobsOptions -> Set FilePath
getBuildGlobs { selected, dependencies, withTests, depsOnly } =
  Set.fromFoldable $ projectGlobs <> monorepoPkgGlobs <> dependencyGlobs <> [ BuildInfo.buildInfoPath ]
  where
  -- Here we select the right globs for a monorepo setup with a bunch of packages
  projectGlobs = case depsOnly of
    true -> []
    false ->
      -- We just select all the workspace package globs, because it's (1) intuitive and (2) backwards compatible
      workspacePackageGlob =<< NEA.toArray selected

  testGlobs = case withTests of
    true -> WithTestGlobs
    false -> NoTestGlobs

  workspacePackageGlob :: WorkspacePackage -> Array String
  workspacePackageGlob p = Config.sourceGlob testGlobs p.package.name (WorkspacePackage p)

  { yes: monorepoPkgs, no: dependencyPkgs } = partition isWorkspacePackage $ Map.toUnfoldable dependencies
  -- depsOnly means "no packages from the monorepo", so we filter out the workspace packages
  dependencyGlobs = (Tuple.uncurry $ Config.sourceGlob NoTestGlobs) =<< dependencyPkgs
  monorepoPkgGlobs
    | depsOnly = []
    | otherwise = (Tuple.uncurry $ Config.sourceGlob testGlobs) =<< monorepoPkgs

isWorkspacePackage :: Tuple PackageName Package -> Boolean
isWorkspacePackage = case _ of
  Tuple _ (WorkspacePackage _) -> true
  _ -> false
