module Spago.Bin.Flags where

import Spago.Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser

selectedPackage ∷ ArgParser (Maybe String)
selectedPackage =
  ArgParser.argument [ "--package", "-p" ]
    "Select the local project to build"
    # ArgParser.optional

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
  ArgParser.flag [ "--no-color" ]
    "Force logging without ANSI color escape sequences"
    # ArgParser.boolean
    # ArgParser.default false

json ∷ ArgParser Boolean
json =
  ArgParser.flag [ "--json", "-j" ]
    "Format the output as JSON"
    # ArgParser.boolean
    # ArgParser.default false

-- TODO: Discuss if we want to use sum types for flags or go with boolean
-- Depending on the decision, revert or change the json flag, too
data IncludeTransitive = IncludeTransitive | NoIncludeTransitive

transitive ∷ ArgParser IncludeTransitive
transitive =
  ArgParser.flag [ "--transitive", "-t" ]
    "Include transitive dependencies"
    # ArgParser.boolean
    # ArgParser.default false
    # map (if _ then IncludeTransitive else NoIncludeTransitive)

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
