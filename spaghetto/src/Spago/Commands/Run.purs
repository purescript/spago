module Spago.Commands.Run where

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

data NodeEsSupport
  = Unsupported Version
  | Experimental
  | Supported

{-
TODO:

hasNodeEsSupport :: (HasLogFunc env) => RIO env NodeEsSupport
hasNodeEsSupport = do
  nodeVersion <- Cmd.getCmdVersion "node"
  case nodeVersion  of
    Left err -> do
      logDebug $ display $ "Unable to get Node.js version: " <> displayShow err
      pure Supported
    Right nv@Version.SemVer{} | Version._svMajor nv < 12 ->
      pure $ Unsupported nv
    Right nv@Version.SemVer{} | Version._svMajor nv >= 12 && Version._svMajor nv < 13 ->
      pure Experimental
    _ -> pure Supported

-}

run :: forall a. Spago (RunEnv a) Unit
run = do
  { workspace, runOptions: opts } <- ask
  let execOptions = Cmd.defaultExecOptions { pipeStdin = Cmd.StdinPipeParent }

  case workspace.backend of
    Nothing -> do
      let runDir = Path.concat [ Paths.localCachePath, "run" ]
      liftAff $ FS.mkdirp runDir
      let
        runJsPath = Path.concat [ runDir, "run.js" ]
        packageJsonPath = Path.concat [ runDir, "package.json" ]
        packageJsonContents = "{\"type\":\"module\" }"

        nodeArgs Experimental = [ "--experimental-modules", runJsPath ] <> opts.execArgs
        nodeArgs _ = [ runJsPath ] <> opts.execArgs

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

      logDebug "Running with backend: nodejs"
      -- TODO: fail if we are not dealing with at least PS 0.15.4
      -- TODO: nodeVersion <- hasNodeEsSupport
      let nodeVersion = Supported
      case nodeVersion of
        Unsupported nv ->
          die [ "Unsupported Node.js version: " <> show (Version.printVersion nv), "Required Node.js version >=12." ]
        _ -> pure unit
      logDebug $ "Writing " <> show runJsPath
      liftAff $ FS.writeTextFile UTF8 runJsPath nodeContents
      liftAff $ FS.chmod runJsPath (Perms.mkPerms Perms.all Perms.all Perms.all)
      logDebug $ "Writing " <> show packageJsonPath
      liftAff $ FS.writeTextFile UTF8 packageJsonPath packageJsonContents
      logDebug $ "Executing from: " <> show opts.executeDir
      logDebug $ "Running node command with args: `" <> show (nodeArgs nodeVersion) <> "`"
      -- TODO: which node
      liftAff (Cmd.exec "node" (nodeArgs nodeVersion) (execOptions { cwd = Just opts.executeDir })) >>= case _ of
        Right _r -> case opts.successMessage of
          Just m -> logSuccess m
          Nothing -> pure unit
        Left err -> do
          logDebug $ show err
          die opts.failureMessage
    Just backend -> do
      let args = [ "--run", opts.moduleName <> ".main" ] <> opts.execArgs
      logDebug $ "Running command `" <> backend <> " " <> show args <> "`"
      liftAff (Cmd.exec backend args execOptions) >>= case _ of
        Right _r -> case opts.successMessage of
          Just m -> logSuccess m
          Nothing -> pure unit
        Left err -> do
          logDebug $ show err
          die [ opts.failureMessage, "Backend " <> show backend <> " exited with error:" <> err.shortMessage ]
