module Spago.Bin.Flags where

import Spago.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Array.NonEmpty as NEA
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Data.String as String
import Spago.Core.Config (ShowSourceCode(..))
import Spago.Core.Config as Core

selectedPackage ∷ ArgParser (Maybe String)
selectedPackage =
  ArgParser.argument [ "--package", "-p" ]
    "Select the local project to build"
    # ArgParser.optional

psaStrict ∷ ArgParser (Maybe Boolean)
psaStrict =
  ArgParser.flag [ "--strict" ]
    "Promotes project sources' warnings to errors"
    # ArgParser.boolean
    # ArgParser.optional

psaCensorWarnings ∷ ArgParser (Maybe Boolean)
psaCensorWarnings =
  ArgParser.flag [ "--psa-censor-warnings" ]
    "Censor all warnings"
    # ArgParser.boolean
    # ArgParser.optional

psaCensorLib ∷ ArgParser (Maybe Boolean)
psaCensorLib =
  ArgParser.flag [ "--psa-censor-lib" ]
    "Censor warnings from library sources"
    # ArgParser.boolean
    # ArgParser.optional

psaCensorSrc ∷ ArgParser (Maybe Boolean)
psaCensorSrc =
  ArgParser.flag [ "--psa-censor-src" ]
    "Censor warnings from project sources"
    # ArgParser.boolean
    # ArgParser.optional

psaShowSource ∷ ArgParser (Maybe ShowSourceCode)
psaShowSource =
  NoSourceCode
    <$ ArgParser.flag [ "--psa-no-source" ]
      "Disable original source code printing"
    # ArgParser.optional

psaCensorCodes :: ArgParser (Maybe (NonEmptySet String))
psaCensorCodes =
  ArgParser.argument [ "--psa-censor-codes" ]
    "Censor specific error codes (comma-separated list)"
    # ArgParser.unformat "CODE1,CODE2,...,CODEX"
        (maybe (Left "Did not get a set of values") (Right <<< NonEmptySet.fromFoldable1) <<< NEA.fromArray <<< String.split (String.Pattern ","))
    # ArgParser.optional

psaFilterCodes :: ArgParser (Maybe (NonEmptySet String))
psaFilterCodes =
  ArgParser.argument [ "--psa-filter-codes" ]
    "Only show specific error codes (comma-separated list)"
    # ArgParser.unformat "CODE1,CODE2,...,CODEX"
        (maybe (Left "Did not get a set of values") (Right <<< NonEmptySet.fromFoldable1) <<< NEA.fromArray <<< String.split (String.Pattern ","))
    # ArgParser.optional

psaStatVerbosity :: ArgParser (Maybe Core.StatVerbosity)
psaStatVerbosity = ArgParser.optional $ ArgParser.choose "StatVerbosity"
  [ Core.VerboseStats <$ ArgParser.flag [ "--psa-verbose-stats" ] "Show counts for each warning type"
  , Core.NoStats <$ ArgParser.flag [ "--psa-censor-stats" ] "Censor warning/error summary"
  ]

psaStashFile ∷ ArgParser (Maybe (Either Boolean String))
psaStashFile = ArgParser.optional $ ArgParser.choose "stash"
  [ ArgParser.argument [ "--psa-stash" ] "Enable persistent warnings using a specific stash file"
      # ArgParser.unformat "FILE" (Right <<< Right)
  , Left true <$ ArgParser.flag [ "--psa-stash" ] "Enable persistent warnings using default stash file location"
  ]

jsonErrors ∷ ArgParser Boolean
jsonErrors =
  ArgParser.flag [ "--json-errors" ]
    "Output compiler warnings/errors as JSON"
    # ArgParser.boolean

minify ∷ ArgParser Boolean
minify =
  ArgParser.flag [ "--minify" ]
    "Minify the bundle"
    # ArgParser.boolean

entrypoint ∷ ArgParser (Maybe String)
entrypoint =
  ArgParser.argument [ "--module" ]
    "The module to bundle as the entrypoint"
    # ArgParser.optional

bundleType ∷ ArgParser (Maybe String)
bundleType =
  ArgParser.argument [ "--type" ]
    "The type of the module produced. 'app' will call main, 'module' will just export the contents."
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
  ArgParser.flag [ "--no-color", "--monochrome" ]
    "Force logging without ANSI color escape sequences"
    # ArgParser.boolean
    # ArgParser.default false

json ∷ ArgParser Boolean
json =
  ArgParser.flag [ "--json" ]
    "Format the output as JSON"
    # ArgParser.boolean
    # ArgParser.default false

transitive ∷ ArgParser Boolean
transitive =
  ArgParser.flag [ "--transitive" ]
    "Include transitive dependencies"
    # ArgParser.boolean
    # ArgParser.default false

pedanticPackages ∷ ArgParser Boolean
pedanticPackages =
  ArgParser.flag [ "--pedantic-packages" ]
    "Check for redundant or missing packages in the config and fail the build if any"
    # ArgParser.boolean
    # ArgParser.default false

pursArgs ∷ ArgParser (List String)
pursArgs =
  ArgParser.argument [ "--purs-args" ]
    "Arguments to pass to purs compile. Wrap in quotes."
    # ArgParser.many

execArgs :: ArgParser (Maybe (Array String))
execArgs =
  ArgParser.rest
    "Arguments to pass to the running script"
    # ArgParser.optional

backendArgs :: ArgParser (List String)
backendArgs =
  ArgParser.argument [ "--backend-args" ]
    "Arguments to pass to the backend compile step. Wrap in quotes."
    # ArgParser.many

moduleName :: ArgParser (Maybe String)
moduleName =
  ArgParser.argument [ "--main", "-m" ]
    "Module to be used as the application's entry point"
    # ArgParser.optional

testDeps :: ArgParser Boolean
testDeps =
  ArgParser.flag [ "--test-deps" ]
    "Act on the test config rather than the main one"
    # ArgParser.boolean
    # ArgParser.default false

packages ∷ ArgParser (List String)
packages =
  ArgParser.anyNotFlag "PACKAGE"
    "Package name to add as dependency"
    # ArgParser.many

package :: ArgParser String
package =
  ArgParser.anyNotFlag "PACKAGE"
    "Package name"

maybeVersion :: ArgParser (Maybe String)
maybeVersion =
  ArgParser.anyNotFlag "VERSION"
    "Package version"
    # ArgParser.optional

maybeSetVersion :: ArgParser (Maybe String)
maybeSetVersion =
  ArgParser.argument [ "--set" ]
    "Optional package set version to be used instead of the latest one."
    # ArgParser.optional

ensureRanges :: ArgParser Boolean
ensureRanges =
  ArgParser.flag [ "--ensure-ranges" ]
    "Add version bounds for all the dependencies of the selected project"
    # ArgParser.boolean
    # ArgParser.default false
