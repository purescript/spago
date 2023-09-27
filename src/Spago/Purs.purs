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
getPurs = Cmd.getExecutable "purs" >>= parseVersionOutput

-- Drop the stuff after a space: dev builds look like this: 0.15.6 [development build; commit: 8da7e96005f717f03d6eee3c12b1f1416659a919]
-- Drop the stuff after a hyphen: prerelease builds look like this: 0.15.6-2
parseVersionOutput :: forall a. { cmd :: String, output :: String } -> Spago (LogEnv a) Purs
parseVersionOutput { cmd, output: stdout } = case parseLenientVersion (dropStuff "-" $ dropStuff " " stdout) of
  Left _err -> die $ "Failed to parse purs version. Was: " <> stdout
  -- Fail if Purs is lower than 0.15.4
  Right v ->
    if Version.minor v >= 15 && Version.patch v >= 4 then
      pure { cmd, version: v }
    else
      die [ "Unsupported PureScript version " <> Version.print v, "Please install PureScript v0.15.4 or higher." ]
  where
  dropStuff pattern = fromMaybe "" <<< Array.head <<< String.split (String.Pattern pattern)

compile :: forall a. Set FilePath -> Array String -> Spago (PursEnv a) (Either Cmd.ExecError Cmd.ExecResult)
compile globs pursArgs = do
  { purs } <- ask
  let args = [ "compile" ] <> pursArgs <> Set.toUnfoldable globs
  logDebug [ "Running command:", "purs " <> String.joinWith " " args ]
  -- PureScript (as of v0.14.0) outputs the compiler errors/warnings to `stdout`
  -- and outputs "Compiling..." messages to `stderr`
  -- So, we want to pipe stderr to the parent so we see the "Compiling..." messages in real-time.
  -- However, we do not pipe `stdout` to the parent, so that we don't see the errors reported twice:
  -- once via `purs` and once via spago's pretty-printing of the same errors/warnings.
  Cmd.exec purs.cmd args $ Cmd.defaultExecOptions
    { pipeStdout = false }

repl :: forall a. Set FilePath -> Array String -> Spago (PursEnv a) (Either Cmd.ExecError Cmd.ExecResult)

repl globs pursArgs = do
  { purs } <- ask
  let args = [ "repl" ] <> pursArgs <> Set.toUnfoldable globs
  Cmd.exec purs.cmd args $ Cmd.defaultExecOptions
    { pipeStdout = true
    , pipeStderr = true
    , pipeStdin = Cmd.StdinPipeParent
    }

data DocsFormat
  = Html
  | Markdown
  | Ctags
  | Etags
derive instance Eq DocsFormat

parseDocsFormat :: String -> Maybe DocsFormat
parseDocsFormat = case _ of
  "html"     -> Just Html
  "markdown" -> Just Markdown
  "ctags"    -> Just Ctags
  "etags"    -> Just Etags
  _          -> Nothing

printDocsFormat :: DocsFormat -> String
printDocsFormat = case _ of
  Html     -> "html"
  Markdown -> "markdown"
  Ctags    -> "ctags"
  Etags    -> "etags"

docs :: forall a. Set FilePath -> DocsFormat -> Spago (PursEnv a) (Either Cmd.ExecError Cmd.ExecResult)
docs globs format = do
  { purs } <- ask
  let args = [ "docs", "--format", printDocsFormat format ] <> Set.toUnfoldable globs
  Cmd.exec purs.cmd args $ Cmd.defaultExecOptions
    { pipeStdout = true
    , pipeStderr = true
    , pipeStdin = Cmd.StdinPipeParent
    }

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
