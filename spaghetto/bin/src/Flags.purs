module Flags where

import Spago.Prelude

import ArgParse.Basic as ArgParser

selectedPackage =
  ArgParser.argument [ "--package", "-p" ]
    "Select the local project to build"
    # ArgParser.optional

minify =
  ArgParser.flag [ "--minify" ]
    "Minify the bundle"
    # ArgParser.boolean
    # ArgParser.optional

entrypoint =
  ArgParser.argument [ "--entrypoint" ]
    "The module to bundle as the entrypoint"
    # ArgParser.optional

outfile =
  ArgParser.argument [ "--outfile" ]
    "Destination path for the bundle"
    # ArgParser.optional

platform =
  ArgParser.argument [ "--platform" ]
    "The bundle platform. 'node' or 'browser'"
    # ArgParser.optional
