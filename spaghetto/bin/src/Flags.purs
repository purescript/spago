module Flags where

import Spago.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Set as Set
import Data.String as String
import Psa (ErrorCode, StatVerbosity(..))

selectedPackage ∷ ArgParser (Maybe String)
selectedPackage =
  ArgParser.argument [ "--package", "-p" ]
    "Select the local project to build"
    # ArgParser.optional

psaCensorWarnings ∷ ArgParser Boolean
psaCensorWarnings =
  ArgParser.flag [ "--censor-warnings" ]
    "Censor all warnings"
    # ArgParser.boolean

psaCensorLib ∷ ArgParser Boolean
psaCensorLib =
  ArgParser.flag [ "--censor-lib" ]
    "Censor warnings from library sources"
    # ArgParser.boolean

psaCensorSrc ∷ ArgParser Boolean
psaCensorSrc =
  ArgParser.flag [ "--censor-src" ]
    "Censor warnings from project sources"
    # ArgParser.boolean

psaStrict ∷ ArgParser Boolean
psaStrict =
  ArgParser.flag [ "--strict" ]
    "Promotes `src` warnings to errors"
    # ArgParser.boolean

psaAnsi ∷ ArgParser Boolean
psaAnsi =
  ArgParser.flag [ "--no-colors", "--monochrome" ]
    "Disable ANSI colors"
    # ArgParser.boolean
    <#> not
    # ArgParser.default true

psaShowSource ∷ ArgParser Boolean
psaShowSource =
  ArgParser.flag [ "--no-source" ]
    "Disable original source code printing"
    # ArgParser.boolean

psaStash ∷ ArgParser Boolean
psaStash =
  ArgParser.flag [ "--stash" ]
    "Enable persistent warnings (defaults to `.psa-stash`)"
    # ArgParser.boolean

psaCensorCodes :: ArgParser (Set ErrorCode)
psaCensorCodes =
  ArgParser.argument [ "--censor-codes" ]
    "Censor specific error codes (comma-separated list)"
    # ArgParser.unformat "CODE1,CODE2,...,CODEX" (Right <<< foldMap Set.singleton <<< String.split (String.Pattern ","))
    # ArgParser.default Set.empty

psaFilterCodes :: ArgParser (Set ErrorCode)
psaFilterCodes =
  ArgParser.argument [ "--filter-codes" ]
    "Only show specific error codes (comma-separated list)"
    # ArgParser.unformat "CODE1,CODE2,...,CODEX" (Right <<< foldMap Set.singleton <<< String.split (String.Pattern ","))
    # ArgParser.default Set.empty

psaStatVerbosity :: ArgParser StatVerbosity
psaStatVerbosity = ado
  verbose <- verboseFlag
  noStats <- noStatsFlag
  in fromMaybe CompactStats $ verbose <|> noStats
  where
  verboseFlag :: ArgParser (Maybe StatVerbosity)
  verboseFlag =
    map (if _ then Just VerboseStats else Nothing) do
      ArgParser.flag [ "--verbose-stats" ]
        "Show counts for each warning type"
        # ArgParser.boolean

  noStatsFlag :: ArgParser (Maybe StatVerbosity)
  noStatsFlag =
    map (if _ then Just NoStats else Nothing) do
      ArgParser.flag [ "--censor-stats" ]
        "Censor warning/error summary"
        # ArgParser.boolean

psaLibDirs :: ArgParser (Maybe (Array String))
psaLibDirs =
  ArgParser.argument [ "--is-lib" ]
    "Distinguishing library path (defaults to '????')"
    # ArgParser.unformat "DIR1,DIR2,...,DIR3" (Right <<< String.split (String.Pattern ","))
    # ArgParser.optional

psaStashFile :: ArgParser String
psaStashFile =
  ArgParser.argument [ "--stash" ]
    "Enable persistent warnings using a specific stash file"
    -- needed to distinguish the `--stash` flag from `--stash=File` arg
    # ArgParser.unformat "FILE" Right
    # ArgParser.default ".psa-stash"

minify ∷ ArgParser Boolean
minify =
  ArgParser.flag [ "--minify" ]
    "Minify the bundle"
    # ArgParser.boolean

entrypoint ∷ ArgParser (Maybe String)
entrypoint =
  ArgParser.argument [ "--entrypoint" ]
    "The module to bundle as the entrypoint"
    # ArgParser.optional

outfile ∷ ArgParser (Maybe String)
outfile =
  ArgParser.argument [ "--outfile" ]
    "Destination path for the bundle"
    # ArgParser.optional

platform ∷ ArgParser (Maybe String)
platform =
  ArgParser.argument [ "--platform" ]
    "The bundle platform. 'node' or 'browser'"
    # ArgParser.optional

output :: ArgParser (Maybe String)
output =
  ArgParser.argument [ "--output" ]
    "The output directory for compiled files (default: \"output\")"
    # ArgParser.optional

quiet ∷ ArgParser Boolean
quiet =
  ArgParser.flag [ "--quiet", "-q" ]
    "Suppress all spago logging"
    # ArgParser.boolean
    # ArgParser.default false

verbose ∷ ArgParser Boolean
verbose =
  ArgParser.flag [ "--verbose", "-v" ]
    "Enable additional debug logging, e.g. printing `purs` commands"
    # ArgParser.boolean
    # ArgParser.default false

noColor ∷ ArgParser Boolean
noColor =
  ArgParser.flag [ "--no-color" ]
    "Force logging without ANSI color escape sequences"
    # ArgParser.boolean
    # ArgParser.default false

pursArgs ∷ ArgParser (List String)
pursArgs =
  ArgParser.argument [ "--purs-args" ]
    "Arguments to pass to purs compile. Wrap in quotes."
    # ArgParser.many

packages ∷ ArgParser (List String)
packages =
  ArgParser.anyNotFlag "PACKAGE"
    "Package name to add as dependency"
    # ArgParser.many
