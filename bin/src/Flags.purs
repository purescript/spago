module Spago.Bin.Flags where

import Data.Array as Array
import Data.Function (flip)
import Data.List as List
import Data.Maybe (optional)
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Options.Applicative (Parser, argument, eitherReader, help, long, many, metavar, option, short, str, strArgument, strOption, switch, value)
import Spago.Core.Config (ShowSourceCode(..))
import Spago.Core.Config as Core
import Spago.Prelude (Either(..), List, Maybe, ($), ($>), (<$), (<$>), (<>), (<|>))

selectedPackage :: Parser (Maybe String)
selectedPackage =
  optional $
    strOption
      ( long "package"
          <> short 'p'
          <> help "Select the local project to build"
      )

strict :: Parser (Maybe Boolean)
strict =
  optional
    $ switch
        ( long "strict"
            <> help "Promotes project sources' warnings to errors"
        )

censorBuildWarnings ∷ Parser (Maybe Core.CensorBuildWarnings)
censorBuildWarnings =
  optional $
    flip
      option
      ( long "censor-build-warnings"
          <> help "Censor compiler warnings based on file's location: 'dependency', 'project', or 'all'"
          <> metavar "ARG"
      )
      ( eitherReader
          case _ of
            "all" -> Right Core.CensorAllWarnings
            "project" -> Right Core.CensorProjectWarnings
            "dependency" -> Right Core.CensorDependencyWarnings
            _ -> Left $ "Expected 'all', 'project', or 'dependency'"
      )

showSource ∷ Parser (Maybe ShowSourceCode)
showSource =
  optional
    $ switch
        ( long "no-source"
            <> help "Disable original source code printing"
        )
    $>
      NoSourceCode

censorCodes :: Parser (Maybe (NonEmptySet String))
censorCodes =
  NonEmptySet.fromFoldable
    <$>
      ( many
          $ strOption
              ( long "censor-code"
                  <> metavar "CODE"
                  <> help "Censor a specific error code (e.g. `ShadowedName`)"
              )
      )

filterCodes :: Parser (Maybe (NonEmptySet String))
filterCodes =
  NonEmptySet.fromFoldable
    <$>
      many
        ( strOption
            $ long "filter-code"
            <> metavar "CODE"
            <> help "Only show a specific error code (e.g. `TypesDoNotUnify`)"
        )

statVerbosity :: Parser (Maybe Core.StatVerbosity)
statVerbosity =
  optional
    $ (Core.VerboseStats <$ switch (long "verbose-stats" <> help "Show counts for each warning type"))
    <|>
      (Core.NoStats <$ switch (long "censor-stats" <> help "Censor warning/error summary"))

persistWarnings ∷ Parser (Maybe Boolean)
persistWarnings =
  optional $
    switch
      ( long "persist-warnings"
          <> help "Persist the compiler warnings between multiple underlying `purs compile` calls"
      )

jsonErrors ∷ Parser Boolean
jsonErrors =
  switch
    ( long "json-errors"
        <> help "Output compiler warnings/errors as JSON"
    )

minify ∷ Parser Boolean
minify =
  switch
    ( long "minify"
        <> help "Minify the bundle"
    )

entrypoint ∷ Parser (Maybe String)
entrypoint =
  optional
    $ strOption
        ( long "module"
            <> help "The module to bundle as the entrypoint"
        )

bundleType ∷ Parser (Maybe String)
bundleType =
  optional
    $ strOption
        ( long "bundle-type"
            <> help "The type of the module produced. 'app' will call main, 'module' will just export the contents."
        )

outfile ∷ Parser (Maybe String)
outfile =
  optional
    $ strOption
        ( long "outfile"
            <> help "Destination path for the bundle"
        )

-- TODO make an ADT for node and browser
platform ∷ Parser (Maybe String)
platform =
  optional
    $ option
        ( eitherReader
            case _ of
              "node" -> Right "node"
              "browser" -> Right "browser"
              _ -> Left "Expected \"node\" or \"browser\""
        )
        ( long "platform"
            <> help "The bundle platform. 'node' or 'browser'"
        )

output ∷ Parser (Maybe String)
output =
  optional
    $ strOption
        ( long "output"
            <> help "The output directory for compiled files"
            <> metavar "DIR"
            <> value "output"
        )

quiet ∷ Parser Boolean
quiet =
  switch
    ( long "quiet"
        <> short 'q'
        <> help "Suppress all spago logging"
    )

verbose ∷ Parser Boolean
verbose =
  switch
    ( long "verbose"
        <> short 'v'
        <> help "Enable additional debug logging, e.g. printing `purs` commands"
    )

noColor ∷ Parser Boolean
noColor =
  switch
    ( long "no-color"
        <> long "monochrome"
        <> help "Force logging without ANSI color escape sequences"
    )

json ∷ Parser Boolean
json =
  switch
    ( long "json"
        <> help "Format the output as JSON"
    )

transitive ∷ Parser Boolean
transitive =
  switch
    ( long "transitive"
        <> help "Include transitive dependencies"
    )

pedanticPackages ∷ Parser Boolean
pedanticPackages =
  switch
    ( long "pedantic-packages"
        <> help "Check for redundant or missing packages in the config and fail the build if any"
    )

pursArgs :: Parser (List String)
pursArgs =
  List.fromFoldable
    <$>
      ( many
          $ strOption
              ( long "purs-args"
                  <> metavar "ARGS"
                  <> help "Arguments to pass to purs compile. Wrap in quotes. `--output` and `--json-errors` must be passed to Spago directly."
              )
      )

execArgs :: Parser (Maybe (Array String))
execArgs =
  optional
    $ Array.fromFoldable
    <$> many
      ( strArgument
          ( help "Arguments to pass to the running script"
              <> metavar "ARGS"
          )
      )

backendArgs :: Parser (List String)
backendArgs =
  many $
    strOption
      ( long "backend-args"
          <> help "Arguments to pass to the running script"
          <> metavar "ARGS"
      )

moduleName ∷ Parser (Maybe String)
moduleName =
  optional
    $ strOption
        ( long "main"
            <> short 'm'
            <> help "Module to be used as the application's entry point"
        )

testDeps ∷ Parser Boolean
testDeps =
  switch
    ( long "test-deps"
        <> help "Act on the test config rather than the main one"
    )

useSolver :: Parser Boolean
useSolver =
  switch
    ( long "use-solver"
        <> help "Use the solver instead of package sets"
    )

packages ∷ Parser (List String)
packages =
  many $
    strOption
      ( metavar "PACKAGE"
          <> help "Package name to add as dependency"
      )

package :: Parser String
package =
  strOption
    ( metavar "PACKAGE"
        <> help "Package name"
    )

maybeVersion :: Parser (Maybe String)
maybeVersion =
  optional $
    strOption
      ( metavar "VERSION"
          <> help "Package version"
      )

maybeSetVersion :: Parser (Maybe String)
maybeSetVersion =
  optional $
    strOption
      ( long "package-set"
          <> help "Optional package set version to be used instead of the latest one"
      )

maybePackageName :: Parser (Maybe String)
maybePackageName =
  optional $
    strOption
      ( long "name"
          <> help "Optional package name to be used for the new project"
      )

ensureRanges ∷ Parser Boolean
ensureRanges =
  switch
    ( long "ensure-ranges"
        <> help "Add version bounds for all the dependencies of the selected project"
    )