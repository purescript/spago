module Spago.Command.Run
  ( getNode
  , run
  , RunEnv
  , Node
  , RunOptions
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Node.FS.Perms as Perms
import Node.Path as Path
import Registry.Version (Version)
import Registry.Version as Version
import Spago.Cmd as Cmd
import Spago.Config (Workspace, WorkspacePackage)
import Spago.FS as FS
import Spago.Paths as Paths

type RunEnv a =
  { logOptions :: LogOptions
  , workspace :: Workspace
  , runOptions :: RunOptions
  , selected :: WorkspacePackage
  , node :: Node
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
    Right r -> case Version.parseVersion Version.Lenient r.stdout of
      Left _err -> die $ "Failed to parse NodeJS version. Was: " <> r.stdout
      Right v ->
        if Version.major v >= 13 then
          pure v
        else
          die [ "Unsupported Node version " <> Version.printVersion v, "Please install a Node v13 or higher." ]

getNode :: forall a. Spago (LogEnv a) Node
getNode = do
  version <- nodeVersion
  pure { cmd: "node", version }

run :: forall a. Spago (RunEnv a) Unit
run = do
  { workspace, node, runOptions: opts } <- ask
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
            , String.replace (Pattern "\\") (Replacement "/") opts.sourceDir
            , "/"
            , fromMaybe "output" workspace.output
            , "/"
            , opts.moduleName
            , "/"
            , "index.js"
            , "'\n\n"
            , "main()"
            ]

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
      logDebug $ "Running command `" <> backend <> " " <> show args <> "`"
      Cmd.exec backend args execOptions >>= case _ of
        Right _r -> case opts.successMessage of
          Just m -> logSuccess m
          Nothing -> pure unit
        Left err -> do
          logDebug $ show err
          die [ opts.failureMessage, "Backend " <> show backend <> " exited with error:" <> err.shortMessage ]
