module Spago.Command.Run
  ( getNode
  , run
  , RunEnv
  , Node
  , RunOptions
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Codec.Argonaut as CA
import Node.FS.Perms as Perms
import Node.Path as Path
import Registry.Version as Version
import Spago.Cmd as Cmd
import Spago.Config (Package, Workspace, WorkspacePackage)
import Spago.Purs (Purs, ModuleGraph(..))
import Spago.Purs as Purs
import Spago.FS as FS
import Spago.Paths as Paths
import Spago.Command.Build as Build

type RunEnv a =
  { logOptions :: LogOptions
  , workspace :: Workspace
  , runOptions :: RunOptions
  , selected :: WorkspacePackage
  , dependencies :: Map PackageName Package
  , node :: Node
  , purs :: Purs
  | a
  }

type RunOptions =
  { execArgs :: Array String
  , moduleName :: String
  , sourceDir :: FilePath
  , executeDir :: FilePath
  , successMessage :: Maybe String
  , failureMessage :: String
  }

type Node = { cmd :: String, version :: Version }

nodeVersion :: forall a. Spago (LogEnv a) Version
nodeVersion =
  Cmd.exec "node" [ "--version" ] Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false } >>= case _ of
    Left err -> do
      logDebug $ show err
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
  pure { cmd: "node", version }

run :: forall a. Spago (RunEnv a) Unit
run = do
  { workspace, node, runOptions: opts, dependencies, selected } <- ask
  let execOptions = Cmd.defaultExecOptions { pipeStdin = Cmd.StdinPipeParent }

  case workspace.backend of
    Nothing -> do
      logDebug "Running with backend: nodejs"
      let runDir = Path.concat [ Paths.localCachePath, "run" ]
      FS.mkdirp runDir
      let
        runJsPath = Path.concat [ runDir, "run.js" ]
        packageJsonPath = Path.concat [ runDir, "package.json" ]
        packageJsonContents = "{\"type\":\"module\" }"

        nodeArgs = [ runJsPath ] <> opts.execArgs

        nodeContents =
          Array.fold
            [ "import { main } from 'file://"
            , withForwardSlashes opts.sourceDir
            , "/"
            , fromMaybe "output" workspace.buildOptions.output
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
          { dependencies
          , depsOnly: false
          -- Here we include tests as well, because we use this code for `spago run` and `spago test`
          , withTests: true
          , selected
          }
      Purs.graph globs [] >>= case _ of
        Left err -> logWarn $ "Could not decode the output of `purs graph`, error: " <> CA.printJsonDecodeError err
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
        Right _r -> case opts.successMessage of
          Just m -> logSuccess m
          Nothing -> pure unit
        Left err -> do
          logDebug $ show err
          die opts.failureMessage
    Just backend -> do
      let args = [ "--run", opts.moduleName <> ".main" ] <> opts.execArgs
      logDebug $ "Running command `" <> backend.cmd <> " " <> show args <> "`"
      Cmd.exec backend.cmd args execOptions >>= case _ of
        Right _r -> case opts.successMessage of
          Just m -> logSuccess m
          Nothing -> pure unit
        Left err -> do
          logDebug $ show err
          die [ opts.failureMessage, "Backend " <> show backend <> " exited with error:" <> err.shortMessage ]
