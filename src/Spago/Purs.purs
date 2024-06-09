module Spago.Purs where

import Spago.Prelude

import Data.Array as Array
import Data.Codec.JSON as CJ
import Data.Codec.JSON.Record as CJ.Record
import Data.Profunctor as Profunctor
import Data.Set as Set
import Data.String as String
import Node.Library.Execa (ExecaResult)
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

compile :: forall a. Set FilePath -> Array String -> Spago (PursEnv a) (Either ExecaResult ExecaResult)
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

repl :: forall a. Set FilePath -> Array String -> Spago (PursEnv a) (Either ExecaResult ExecaResult)
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
  "html" -> Just Html
  "markdown" -> Just Markdown
  "ctags" -> Just Ctags
  "etags" -> Just Etags
  _ -> Nothing

printDocsFormat :: DocsFormat -> String
printDocsFormat = case _ of
  Html -> "html"
  Markdown -> "markdown"
  Ctags -> "ctags"
  Etags -> "etags"

docs :: forall a. Set FilePath -> DocsFormat -> Spago (PursEnv a) (Either ExecaResult ExecaResult)
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

moduleGraphCodec :: CJ.Codec ModuleGraph
moduleGraphCodec = Profunctor.wrapIso ModuleGraph (Internal.Codec.strMap "ModuleGraph" Right identity moduleGraphNodeCodec)

type ModuleGraphNode =
  { path :: String
  , depends :: Array ModuleName
  }

moduleGraphNodeCodec :: CJ.Codec ModuleGraphNode
moduleGraphNodeCodec = CJ.named "ModuleGraphNode" $ CJ.Record.object
  { path: CJ.string
  , depends: CJ.array CJ.string
  }

graph :: forall a. Set FilePath -> Array String -> Spago (PursEnv a) (Either CJ.DecodeError ModuleGraph)
graph globs pursArgs = do
  { purs } <- ask
  let args = [ "graph" ] <> pursArgs <> Set.toUnfoldable globs
  logDebug [ "Running command:", "purs " <> String.joinWith " " args ]
  let execOpts = Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false }
  Cmd.exec purs.cmd args execOpts >>= case _ of
    Right r -> do
      logDebug "Called `purs graph`, decoding.."
      pure $ parseJson moduleGraphCodec r.stdout
    Left r -> do
      logDebug $ Cmd.printExecResult r
      die [ "Failed to call `purs graph`, error: " <> r.shortMessage ]
