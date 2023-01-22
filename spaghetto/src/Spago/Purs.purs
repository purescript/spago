module Spago.Purs where

import Spago.Prelude

import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.String as String
import Registry.Internal.Codec as Internal.Codec
import Registry.Version as Version
import Spago.Cmd as Cmd

type PursEnv a =
  { purs :: Purs
  , logOptions :: LogOptions
  | a
  }

type Purs =
  { cmd :: FilePath
  , version :: Version
  }

getPurs :: forall a. Spago (LogEnv a) Purs
getPurs =
  Cmd.exec "purs" [ "--version" ] Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false } >>= case _ of
    Left err -> do
      logDebug $ show err
      die [ "Failed to find purs. Have you installed it, and is it in your PATH?" ]
    -- Drop the stuff after a space: dev builds look like this: 0.15.6 [development build; commit: 8da7e96005f717f03d6eee3c12b1f1416659a919]
    Right r -> case Version.parse (fromMaybe "" (Array.head (String.split (String.Pattern " ") r.stdout))) of
      Left _err -> die $ "Failed to parse purs version. Was: " <> r.stdout
      -- Fail if Purs is lower than 0.15.4
      Right v ->
        if Version.minor v >= 15 && Version.patch v >= 4 then
          pure { cmd: "purs", version: v }
        else
          die [ "Unsupported PureScript version " <> Version.print v, "Please install PureScript v0.15.4 or higher." ]

compile :: forall a. Set FilePath -> Array String -> Spago (PursEnv a) Unit
compile globs pursArgs = do
  { purs } <- ask
  let args = [ "compile" ] <> pursArgs <> Set.toUnfoldable globs
  logDebug [ "Running command:", "purs " <> String.joinWith " " args ]
  Cmd.exec purs.cmd args Cmd.defaultExecOptions >>= case _ of
    Right _r -> logSuccess "Build succeeded."
    Left err -> do
      logDebug $ show err
      die [ "Failed to build." ]

--------------------------------------------------------------------------------
-- Graph

type ModuleName = String

newtype ModuleGraph = ModuleGraph (Map ModuleName ModuleGraphNode)

derive instance Newtype ModuleGraph _

moduleGraphCodec :: JsonCodec ModuleGraph
moduleGraphCodec = Profunctor.wrapIso ModuleGraph (Internal.Codec.strMap "ModuleGraph" Just identity moduleGraphNodeCodec)

type ModuleGraphNode =
  { path :: String
  , depends :: Array ModuleName
  }

moduleGraphNodeCodec :: JsonCodec ModuleGraphNode
moduleGraphNodeCodec = CAR.object "ModuleGraphNode"
  { path: CA.string
  , depends: CA.array CA.string
  }

graph :: forall a. Set FilePath -> Array String -> Spago (PursEnv a) (Either JsonDecodeError ModuleGraph)
graph globs pursArgs = do
  { purs } <- ask
  let args = [ "graph" ] <> pursArgs <> Set.toUnfoldable globs
  logDebug [ "Running command:", "purs " <> String.joinWith " " args ]
  let execOpts = Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false }
  Cmd.exec purs.cmd args execOpts >>= case _ of
    Right { stdout } -> do
      logDebug "Called `purs graph`, decoding.."
      pure $ parseJson moduleGraphCodec stdout
    Left err -> do
      logDebug $ show err
      die [ "Failed to call `purs graph`, error: " <> err.shortMessage ]
