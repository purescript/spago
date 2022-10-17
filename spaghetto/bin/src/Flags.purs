module Flags where

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

backendArgs :: ArgParser (Maybe (Array String))
backendArgs =
  ArgParser.rest
    "Arguments to pass to the running script"
    # ArgParser.optional

moduleName :: ArgParser (Maybe String)
moduleName =
  ArgParser.argument [ "--main", "-m" ]
    "Module to be used as the application's entry point"
    # ArgParser.optional

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
