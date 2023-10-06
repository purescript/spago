module Spago.Bin.Flags where

import Spago.Prelude

import Data.Array as Array
import Data.List as List
import Data.Set.NonEmpty (NonEmptySet)
import Data.Set.NonEmpty as NonEmptySet
import Options.Applicative (FlagFields, Mod, Parser)
import Options.Applicative as O
import Options.Applicative.Types as O
import Spago.Core.Config as Core

flagMaybe ∷ ∀ (a ∷ Type). a -> Mod FlagFields (Maybe a) -> Parser (Maybe a)
flagMaybe a mod = O.flag Nothing (Just a) mod

selectedPackage :: Parser (Maybe String)
selectedPackage =
  O.optional $
    O.strOption
      ( O.long "package"
          <> O.short 'p'
          <> O.help "Select the local project to build"
          <> O.metavar "PACKAGE"
      )

strict :: Parser (Maybe Boolean)
strict =
  flagMaybe true
    ( O.long "strict"
        <> O.help "Promotes project sources' warnings to errors"
    )

censorLibWarnings :: Parser (Maybe Core.CensorBuildWarnings)
censorLibWarnings =
  O.optional $
    O.option
      ( O.eitherReader
          case _ of
            "all" -> Right Core.CensorAllWarnings
            "none" -> Right Core.CensorNoWarnings
            _ -> Left $ "Expected 'all' or 'none'"
      )
      ( O.long "censor-lib-warnings"
          <> O.help "Censor compiler warnings for files from `.spago`: 'all' or 'none'"
          <> O.metavar "ARG"
      )

censorProjectWarnings :: Parser (Maybe Core.CensorBuildWarnings)
censorProjectWarnings =
  O.optional $
    O.option
      ( O.eitherReader
          case _ of
            "all" -> Right Core.CensorAllWarnings
            "none" -> Right Core.CensorNoWarnings
            _ -> Left $ "Expected 'all' or 'none'"
      )
      ( O.long "censor-project-warnings"
          <> O.help "Censor compiler warnings for files from a package defined in this workspace: 'none' or 'all'"
          <> O.metavar "ARG"
      )

censorLibCodes :: Parser (Maybe (NonEmptySet String))
censorLibCodes =
  NonEmptySet.fromFoldable
    <$>
      ( O.many
          $ O.strOption
              ( O.long "censor-library-code"
                  <> O.metavar "CODE"
                  <> O.help "Censor a specific error code (e.g. `ShadowedName`) from `.spago` files"
              )
      )

censorProjectCodes :: Parser (Maybe (NonEmptySet String))
censorProjectCodes =
  NonEmptySet.fromFoldable
    <$>
      ( O.many
          $ O.strOption
              ( O.long "censor-code"
                  <> O.metavar "CODE"
                  <> O.help "Censor a specific error code (e.g. `ShadowedName`) from files in the current workspace"
              )
      )

filterLibCodes :: Parser (Maybe (NonEmptySet String))
filterLibCodes =
  NonEmptySet.fromFoldable
    <$>
      O.many
        ( O.strOption
            $ O.long "filter-library-code"
            <> O.metavar "CODE"
            <> O.help "Only show a specific error code (e.g. `TypesDoNotUnify`) from `.spago` files"
        )

filterProjectCodes :: Parser (Maybe (NonEmptySet String))
filterProjectCodes =
  NonEmptySet.fromFoldable
    <$>
      O.many
        ( O.strOption
            $ O.long "filter-code"
            <> O.metavar "CODE"
            <> O.help "Only show a specific error code (e.g. `TypesDoNotUnify`) from files in this workspace"
        )

statVerbosity :: Parser (Maybe Core.StatVerbosity)
statVerbosity =
  flagMaybe Core.VerboseStats (O.long "verbose-stats" <> O.help "Show counts for each warning type")
    <|> flagMaybe Core.NoStats (O.long "censor-stats" <> O.help "Censor warning/error summary")

jsonErrors :: Parser Boolean
jsonErrors =
  O.switch
    ( O.long "json-errors"
        <> O.help "Output compiler warnings/errors as JSON"
    )

minify :: Parser Boolean
minify =
  O.switch
    ( O.long "minify"
        <> O.help "Minify the bundle"
    )

entrypoint :: Parser (Maybe String)
entrypoint =
  O.optional
    $ O.strOption
        ( O.long "module"
            <> O.help "The module to bundle as the entrypoint"
        )

bundleType :: Parser (Maybe String)
bundleType =
  O.optional
    $ O.strOption
        ( O.long "bundle-type"
            <> O.help "The type of the module produced. 'app' will call main, 'module' will just export the contents."
        )

outfile :: Parser (Maybe String)
outfile =
  O.optional
    $ O.strOption
        ( O.long "outfile"
            <> O.help "Destination path for the bundle"
        )

-- TODO make an ADT for node and browser
platform :: Parser (Maybe String)
platform =
  O.optional
    $ O.option
        ( O.eitherReader
            case _ of
              "node" -> Right "node"
              "browser" -> Right "browser"
              _ -> Left "Expected \"node\" or \"browser\""
        )
        ( O.long "platform"
            <> O.help "The bundle platform. 'node' or 'browser'"
        )

output :: Parser (Maybe String)
output =
  O.optional
    $ O.strOption
        ( O.long "output"
            <> O.help "The output directory for compiled files"
            <> O.metavar "DIR"
            <> O.value "output"
        )

quiet :: Parser Boolean
quiet =
  O.switch
    ( O.long "quiet"
        <> O.short 'q'
        <> O.help "Suppress all spago logging"
    )

verbose :: Parser Boolean
verbose =
  O.switch
    ( O.long "verbose"
        <> O.short 'v'
        <> O.help "Enable additional debug logging, e.g. printing `purs` commands"
    )

noColor :: Parser Boolean
noColor =
  O.switch
    ( O.long "no-color"
        <> O.long "monochrome"
        <> O.help "Force logging without ANSI color escape sequences"
    )

json :: Parser Boolean
json =
  O.switch
    ( O.long "json"
        <> O.help "Format the output as JSON"
    )

latest :: Parser Boolean
latest =
  O.switch
    ( O.long "latest"
        <> O.help "Only include the latest package set for each compiler"
    )

transitive :: Parser Boolean
transitive =
  O.switch
    ( O.long "transitive"
        <> O.help "Include transitive dependencies"
    )

pedanticPackages :: Parser Boolean
pedanticPackages =
  O.switch
    ( O.long "pedantic-packages"
        <> O.help "Check for redundant or missing packages in the config and fail the build if any"
    )

pursArgs :: Parser (List String)
pursArgs =
  List.fromFoldable
    <$>
      ( O.many
          $ O.strOption
              ( O.long "purs-args"
                  <> O.metavar "ARGS"
                  <> O.help "Arguments to pass to purs compile. Wrap in quotes. `--output` and `--json-errors` must be passed to Spago directly."
              )
      )

execArgs :: Parser (Maybe (Array String))
execArgs =
  O.optional
    $ Array.fromFoldable
    <$> O.many
      ( O.strArgument
          ( O.help "Arguments to pass to the running script"
              <> O.metavar "ARGS"
          )
      )

backendArgs :: Parser (List String)
backendArgs =
  O.many $
    O.strOption
      ( O.long "backend-args"
          <> O.help "Arguments to pass to the running script"
          <> O.metavar "ARGS"
      )

moduleName :: Parser (Maybe String)
moduleName =
  O.optional
    $ O.strOption
        ( O.long "main"
            <> O.short 'm'
            <> O.help "Module to be used as the application's entry point"
        )

testDeps :: Parser Boolean
testDeps =
  O.switch
    ( O.long "test-deps"
        <> O.help "Act on the test config rather than the main one"
    )

useSolver :: Parser Boolean
useSolver =
  O.switch
    ( O.long "use-solver"
        <> O.help "Use the solver instead of package sets"
    )

packages :: Parser (List String)
packages =
  O.many $
    O.strArgument
      ( O.metavar "PACKAGE"
          <> O.help "Package name to add as dependency"
      )

package :: Parser String
package =
  O.strArgument
    ( O.metavar "PACKAGE"
        <> O.help "Package name"
    )

maybeVersion :: Parser (Maybe String)
maybeVersion =
  O.optional $
    O.strArgument
      ( O.metavar "VERSION"
          <> O.help "Package version"
      )

maybeSetVersion :: Parser (Maybe String)
maybeSetVersion =
  O.optional $
    O.strOption
      ( O.long "package-set"
          <> O.help "Optional package set version to be used instead of the latest one"
      )

maybePackageName :: Parser (Maybe String)
maybePackageName =
  O.optional $
    O.strOption
      ( O.long "name"
          <> O.help "Optional package name to be used for the new project"
      )

ensureRanges :: Parser Boolean
ensureRanges =
  O.switch
    ( O.long "ensure-ranges"
        <> O.help "Add version bounds for all the dependencies of the selected project"
    )

sourceMaps :: Parser Boolean
sourceMaps =
  O.switch
    ( O.long "source-maps"
        <> O.help "Creates a source map for your bundle"
    )
