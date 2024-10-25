module Spago.Command.Run
  ( getNode
  , run
  , RunEnv
  , Node
  , RunOptions
  ) where

import Spago.Prelude

import Codec.JSON.DecodeError as CJ.DecodeError
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Node.FS.Perms as Perms
import Registry.Version as Version
import Spago.Cmd as Cmd
import Spago.Command.Build as Build
import Spago.Command.Fetch as Fetch
import Spago.Config (Workspace, WorkspacePackage)
import Spago.FS as FS
import Spago.Path as Path
import Spago.Paths as Paths
import Spago.Purs (Purs, ModuleGraph(..))
import Spago.Purs as Purs

type RunEnv a =
  { logOptions :: LogOptions
  , rootPath :: RootPath
  , workspace :: Workspace
  , runOptions :: RunOptions
  , selected :: WorkspacePackage
  , dependencies :: Fetch.PackageTransitiveDeps
  , node :: Node
  , purs :: Purs
  | a
  }

type RunOptions =
  { execArgs :: Array String
  , moduleName :: String
  , executeDir :: GlobalPath
  , successMessage :: Maybe String
  , failureMessage :: String
  }

type Node = { cmd :: GlobalPath, version :: Version }

nodeVersion :: forall a. Spago (LogEnv a) Version
nodeVersion =
  Cmd.exec (Path.global "node") [ "--version" ] Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false } >>= case _ of
    Left r -> do
      logDebug $ Cmd.printExecResult r
      die [ "Failed to find node. Have you installed it, and is it in your PATH?" ]
    Right r -> case parseLenientVersion r.stdout of
      Left _err -> die $ "Failed to parse NodeJS version. Was: " <> r.stdout
      Right v ->
        if Version.major v >= 13 then
          pure v
        else
          die [ "Unsupported Node version " <> Version.print v, "Please install a Node v13 or higher." ]

getNode :: forall a. Spago (LogEnv a) Node
getNode = do
  version <- nodeVersion
  pure { cmd: Path.global "node", version }

run :: forall a. Spago (RunEnv a) Unit
run = do
  { workspace, node, runOptions: opts, dependencies, selected, rootPath } <- ask
  let execOptions = Cmd.defaultExecOptions { pipeStdin = Cmd.StdinPipeParent }

  case workspace.backend of
    Nothing -> do
      logDebug "Running with backend: nodejs"
      let runDir = rootPath </> Paths.localCachePath </> "run"
      FS.mkdirp runDir
      absOutput <- liftEffect $ Path.toAbsolute $ fromMaybe (rootPath </> "output") workspace.buildOptions.output
      let
        runJsPath = runDir </> "run.js"
        packageJsonPath = runDir </> "package.json"
        packageJsonContents = "{\"type\":\"module\" }"

        nodeArgs = [ Path.toRaw runJsPath ] <> opts.execArgs

        nodeContents =
          Array.fold
            [ "import { main } from 'file://"
            , Path.toRaw (withForwardSlashes absOutput)
            , "/"
            , opts.moduleName
            , "/"
            , "index.js"
            , "'\n\n"
            , "main()"
            ]

      -- We check that the module we're about to run is included in the build and spit out a nice error if it isn't (see #383)
      let
        globs = Build.getBuildGlobs
          { rootPath
          , dependencies: Fetch.toAllDependencies dependencies
          , depsOnly: false
          -- Here we include tests as well, because we use this code for `spago run` and `spago test`
          , withTests: true
          , selected: NEA.singleton selected
          }
      Purs.graph globs [] >>= case _ of
        Left err -> logWarn $ "Could not decode the output of `purs graph`, error: " <> CJ.DecodeError.print err
        Right (ModuleGraph graph) -> do
          when (isNothing $ Map.lookup opts.moduleName graph) do
            die [ opts.failureMessage, "Module " <> opts.moduleName <> " not found! Are you including it in your build?" ]

      logDebug $ "Writing " <> show runJsPath
      FS.writeTextFile runJsPath nodeContents
      FS.chmod runJsPath (Perms.mkPerms Perms.all Perms.all Perms.all)
      logDebug $ "Writing " <> show packageJsonPath
      FS.writeTextFile packageJsonPath packageJsonContents
      logDebug $ "Executing from: " <> show opts.executeDir
      logDebug $ "Running node command with args: `" <> show nodeArgs <> "`"
      Cmd.exec node.cmd nodeArgs (execOptions { cwd = Just opts.executeDir }) >>= case _ of
        Right _ -> case opts.successMessage of
          Just m -> logSuccess m
          Nothing -> pure unit
        Left r -> do
          logDebug $ Cmd.printExecResult r
          die opts.failureMessage
    Just backend -> do
      let args = [ "--run", opts.moduleName <> ".main" ] <> opts.execArgs
      logDebug $ "Running command `" <> backend.cmd <> " " <> show args <> "`"
      Cmd.exec (Path.global backend.cmd) args execOptions >>= case _ of
        Right _ -> case opts.successMessage of
          Just m -> logSuccess m
          Nothing -> pure unit
        Left r -> do
          logDebug $ Cmd.printExecResult r
          die [ opts.failureMessage, "Backend " <> show backend <> " exited with error:" <> r.shortMessage ]
