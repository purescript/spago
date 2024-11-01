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
import Registry.PackageName as PackageName
import Spago.BuildInfo as BuildInfo
import Spago.Cmd as Cmd
import Spago.Command.Fetch as Fetch
import Spago.Config (Package(..), PackageMap, WithTestGlobs(..), Workspace, WorkspacePackage)
import Spago.Config as Config
import Spago.Git (Git)
import Spago.Log (prepareToDie)
import Spago.Path as Path
import Spago.Psa as Psa
import Spago.Purs (Purs)
import Spago.Purs.Graph as Graph

type BuildEnv a =
  { purs :: Purs
  , git :: Git
  , dependencies :: Fetch.PackageTransitiveDeps
  , logOptions :: LogOptions
  , rootPath :: RootPath
  , workspace :: Workspace
  , strictWarnings :: Maybe Boolean
  , pedanticPackages :: Boolean
  | a
  }

type BuildOptions =
  { depsOnly :: Boolean
  , pursArgs :: Array String
  , jsonErrors :: Boolean
  }

run :: ∀ a. BuildOptions -> Spago (BuildEnv a) Boolean
run opts = do
  logInfo "Building..."
  { dependencies
  , workspace
  , logOptions
  , rootPath
  , strictWarnings
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
      Just output -> args <> [ "--output", Path.toRaw output ]

  -- find the `--json-errors` flag and die if it's there - Spago handles it
  when (isJust $ Cmd.findFlag { flags: [ "--json-errors" ], args: opts.pursArgs }) do
    die
      [ "Can't pass `--json-errors` option directly to purs."
      , "Use the --json-errors flag for Spago."
      ]

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
    globs = getBuildGlobs
      { rootPath
      , dependencies: case workspace.selected of
          Just p ->
            let
              { core, test } = unsafeFromJust $ Map.lookup p.package.name dependencies
            in
              Map.union core test
          Nothing ->
            allDependencies
      , depsOnly: opts.depsOnly
      , withTests: true
      , selected: selectedPackages
      }
  pathDecisions <- liftEffect $ sequence $ Psa.toPathDecisions
    { rootPath
    , allDependencies
    , selectedPackages: NEA.toArray selectedPackages
    , psaCliFlags: { strict: strictWarnings, statVerbosity: workspace.buildOptions.statVerbosity }
    , censorLibWarnings: workspace.buildOptions.censorLibWarnings
    }
  let
    psaArgs =
      { color: logOptions.color
      , jsonErrors: opts.jsonErrors
      , decisions: join pathDecisions
      , statVerbosity: fromMaybe Psa.defaultStatVerbosity workspace.buildOptions.statVerbosity
      }

  built <- Psa.psaCompile rootPath globs args psaArgs
  backendBuilt <- case workspace.backend of
    _ | not built -> pure false
    Nothing -> pure true
    Just backend -> do
      logInfo $ "Compiling with backend \"" <> backend.cmd <> "\""
      logDebug $ "Running command `" <> backend.cmd <> " build`"
      let
        moreBackendArgs = case backend.args of
          Just as | Array.length as > 0 -> as
          _ -> []
      Cmd.exec (Path.global backend.cmd) (addOutputArgs moreBackendArgs) Cmd.defaultExecOptions >>= case _ of
        Left r -> do
          logDebug $ Cmd.printExecResult r
          prepareToDie [ "Failed to build with backend " <> backend.cmd ] $> false
        Right _ ->
          logSuccess "Backend build succeeded." $> true

  if not backendBuilt then
    pure false
  else do
    let
      pedanticPkgs = NEA.toArray selectedPackages # Array.mapMaybe \p -> do
        let reportSrc = pedanticPackages || (fromMaybe false $ p.package.build >>= _.pedanticPackages)
        let reportTest = pedanticPackages || (fromMaybe false $ p.package.test >>= _.pedanticPackages)
        Alternative.guard (reportSrc || reportTest)
        pure $ Tuple p { reportSrc, reportTest }
    if Array.null pedanticPkgs || opts.depsOnly then
      pure true
    else do
      logInfo "Looking for unused and undeclared transitive dependencies..."
      eitherGraph <- Graph.runGraph rootPath globs opts.pursArgs
      logDebug "Decoded the output of `purs graph` successfully. Analyzing dependencies..."
      eitherGraph # either (prepareToDie >>> (_ $> false)) \graph -> do
        env <- ask
        checkResults <- map Array.fold $ for pedanticPkgs \(Tuple selected options) -> do
          logDebug $ "Checking imports for " <> PackageName.print selected.package.name
          Graph.toImportErrors selected options
            <$> runSpago (Record.union { selected, workspacePackages: selectedPackages } env) (Graph.checkImports graph)
        logDebug "Finished checking imports."
        if Array.null checkResults then
          pure true
        else
          prepareToDie (Graph.formatImportErrors checkResults) $> false

-- TODO: if we are building with all the packages (i.e. selected = Nothing),
-- then we could use the graph to remove outdated modules from `output`!

type BuildGlobsOptions =
  { rootPath :: RootPath
  , withTests :: Boolean
  , depsOnly :: Boolean
  , selected :: NonEmptyArray WorkspacePackage
  , dependencies :: PackageMap
  }

getBuildGlobs :: BuildGlobsOptions -> Set LocalPath
getBuildGlobs { rootPath, selected, dependencies, withTests, depsOnly } =
  Set.fromFoldable $ projectGlobs <> monorepoPkgGlobs <> dependencyGlobs <> [ BuildInfo.buildInfoPath rootPath ]
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

  workspacePackageGlob :: WorkspacePackage -> Array LocalPath
  workspacePackageGlob p = Config.sourceGlob rootPath testGlobs p.package.name (WorkspacePackage p)

  { yes: monorepoPkgs, no: dependencyPkgs } = partition isWorkspacePackage $ Map.toUnfoldable dependencies
  -- depsOnly means "no packages from the monorepo", so we filter out the workspace packages
  dependencyGlobs = (Tuple.uncurry $ Config.sourceGlob rootPath NoTestGlobs) =<< dependencyPkgs
  monorepoPkgGlobs
    | depsOnly = []
    | otherwise = (Tuple.uncurry $ Config.sourceGlob rootPath NoTestGlobs) =<< monorepoPkgs

isWorkspacePackage :: Tuple PackageName Package -> Boolean
isWorkspacePackage = case _ of
  Tuple _ (WorkspacePackage _) -> true
  _ -> false
