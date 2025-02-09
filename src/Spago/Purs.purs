module Spago.Purs
  ( DocsFormat(..)
  , Purs
  , PursEnv
  , compile
  , docs
  , getPurs
  , graph
  , moduleGraphCodec
  , moduleGraphNodeCodec
  , parseDocsFormat
  , parseVersionOutput
  , printDocsFormat
  , repl
  , module Types
  ) where

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
import Spago.Path as Path
import Spago.Purs.Types (ModuleGraph(..), ModuleGraphNode) as Types

type PursEnv a =
  { purs :: Purs
  , logOptions :: LogOptions
  | a
  }

type Purs =
  { cmd :: GlobalPath
  , version :: Version
  }

getPurs :: ∀ a. Spago (LogEnv a) Purs
getPurs = Cmd.getExecutable "purs" >>= parseVersionOutput

-- Drop the stuff after a space: dev builds look like this: 0.15.6 [development build; commit: 8da7e96005f717f03d6eee3c12b1f1416659a919]
-- Drop the stuff after a hyphen: prerelease builds look like this: 0.15.6-2
parseVersionOutput :: ∀ a. { cmd :: GlobalPath, output :: String } -> Spago (LogEnv a) Purs
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

compile :: ∀ a. RootPath -> Set LocalPath -> Array String -> Spago (PursEnv a) (Either ExecaResult ExecaResult)
compile cwd globs pursArgs = do
  { purs } <- ask
  let args = [ "compile" ] <> pursArgs <> globsToArgs cwd globs
  logDebug [ "Running command:", "purs " <> String.joinWith " " args ]
  -- PureScript (as of v0.14.0) outputs the compiler errors/warnings to `stdout`
  -- and outputs "Compiling..." messages to `stderr`
  -- So, we want to pipe stderr to the parent so we see the "Compiling..." messages in real-time.
  -- However, we do not pipe `stdout` to the parent, so that we don't see the errors reported twice:
  -- once via `purs` and once via spago's pretty-printing of the same errors/warnings.
  Cmd.exec purs.cmd args $ Cmd.defaultExecOptions
    { pipeStdout = false
    , cwd = Just $ Path.toGlobal cwd
    }

repl :: ∀ a. RootPath -> Set LocalPath -> Array String -> Spago (PursEnv a) (Either ExecaResult ExecaResult)
repl cwd globs pursArgs = do
  { purs } <- ask
  let args = [ "repl" ] <> pursArgs <> globsToArgs cwd globs
  Cmd.exec purs.cmd args $ Cmd.defaultExecOptions
    { pipeStdout = true
    , pipeStderr = true
    , pipeStdin = Cmd.StdinPipeParent
    , cwd = Just $ Path.toGlobal cwd
    }

globsToArgs :: RootPath -> Set LocalPath -> Array String
globsToArgs cwd globs = Path.localPart <<< (_ `Path.relativeTo` cwd) <$> Set.toUnfoldable globs

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

docs
  :: ∀ a
   . { root :: RootPath
     , globs :: Set LocalPath
     , format :: DocsFormat
     , quiet :: Boolean
     }
  -> Spago (PursEnv a) (Either ExecaResult ExecaResult)
docs cfg = do
  { purs } <- ask
  let args = [ "docs", "--format", printDocsFormat cfg.format ] <> globsToArgs cfg.root cfg.globs
  Cmd.exec purs.cmd args $ Cmd.defaultExecOptions
    { cwd = Just $ Path.toGlobal cfg.root
    , pipeStdout = not cfg.quiet
    , pipeStderr = not cfg.quiet
    , pipeStdin = Cmd.StdinPipeParent
    }

--------------------------------------------------------------------------------
-- Graph

moduleGraphCodec :: CJ.Codec Types.ModuleGraph
moduleGraphCodec = Profunctor.wrapIso Types.ModuleGraph (Internal.Codec.strMap "ModuleGraph" Right identity moduleGraphNodeCodec)

moduleGraphNodeCodec :: CJ.Codec Types.ModuleGraphNode
moduleGraphNodeCodec = CJ.named "ModuleGraphNode" $ CJ.Record.object
  { path: CJ.string
  , depends: CJ.array CJ.string
  }

graph :: ∀ a. RootPath -> Set LocalPath -> Array String -> Spago (PursEnv a) (Either CJ.DecodeError Types.ModuleGraph)
graph cwd globs pursArgs = do
  { purs } <- ask
  let args = [ "graph" ] <> pursArgs <> globsToArgs cwd globs
  logDebug [ "Running command:", "purs " <> String.joinWith " " args ]
  result <- Cmd.exec purs.cmd args $ Cmd.defaultExecOptions
    { cwd = Just $ Path.toGlobal cwd
    , pipeStdout = false
    , pipeStderr = false
    }
  case result of
    Right r -> do
      logDebug "Called `purs graph`, decoding.."
      pure $ parseJson moduleGraphCodec r.stdout
    Left r -> do
      logDebug $ Cmd.printExecResult r
      die [ "Failed to call `purs graph`, error: " <> r.shortMessage ]
