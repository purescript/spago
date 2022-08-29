module Main where

import Spago.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Control.Plus (empty)
import Data.Array as Array
import Data.Either (Either(..), isRight)
import Data.Foldable (foldMap, for_, oneOf)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard, power)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, attempt, effectCanceler, error, launchAff_, makeAff, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.FS.Aff as FS
import Node.FS.Perms as Perms
import Node.FS.Stats as Stats
import Node.FS.Stream (createReadStream, createWriteStream)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

type GlobalArgs = {}
type FetchArgs = { packages :: List String }
type InstallArgs = FetchArgs
type BuildArgs = {}

data Command
  = Fetch FetchArgs
  | Install InstallArgs
  | Build BuildArgs

argParser :: ArgParser Command
argParser =
  ArgParser.choose "command"
    [ ArgParser.command [ "fetch" ]
        "Downloads all of the project's dependencies"
        do
          (Fetch <$> fetchArgsParser) <* ArgParser.flagHelp
    , ArgParser.command [ "install" ]
        "Compile the project's dependencies"
        do
          (Install <$> installArgsParser) <* ArgParser.flagHelp
    , ArgParser.command [ "build" ]
        "Compile the project"
        do
          (Build <$> buildArgsParser) <* ArgParser.flagHelp
    ]
    <* ArgParser.flagHelp
    <* ArgParser.flagInfo [ "--version", "-v" ] "Show the current version" "0.0.1" -- TODO: version. Like, with an embedded build meta module

{-

    quiet       = CLI.switch "quiet" 'q' "Suppress all spago logging"
    verbose     = CLI.switch "verbose" 'v' "Enable additional debug logging, e.g. printing `purs` commands"
    veryVerbose = CLI.switch "very-verbose" 'V' "Enable more verbosity: timestamps and source locations"
    noColor     = Opts.switch (Opts.long "no-color" <> Opts.help "Log without ANSI color escape sequences")

-}

globalArgsParser :: ArgParser GlobalArgs
globalArgsParser =
  ArgParser.fromRecord
    {
    }

fetchArgsParser :: ArgParser FetchArgs
fetchArgsParser =
  ArgParser.fromRecord
    { packages:
        ArgParser.anyNotFlag "PACKAGE"
          "Package name to add as dependency"
          # ArgParser.many
    }

installArgsParser :: ArgParser InstallArgs
installArgsParser = fetchArgsParser

buildArgsParser :: ArgParser BuildArgs
buildArgsParser = ArgParser.fromRecord {}

parseArgs :: Effect (Either ArgParser.ArgError Command)
parseArgs = do
  cliArgs <- Array.drop 2 <$> Process.argv
  pure $ ArgParser.parseArgs "spago"
    "PureScript package manager and build tool"
    argParser
    cliArgs

main :: FilePath -> Effect Unit
main _cliRoot =
  parseArgs >>= case _ of
    Left err ->
      Console.error $ ArgParser.printArgError err
    Right cmd -> launchAff_ case cmd of
      Fetch args -> fetchCmd args
      Install args -> installCmd args
      Build args -> buildCmd args

  where
  installCmd _ = pure unit
  fetchCmd args = Console.logShow args
  buildCmd _ = pure unit
