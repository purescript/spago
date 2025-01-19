module Test.Spago.Build.BuildInfo where

import Test.Prelude

import Control.Monad.Error.Class as MonadError
import Data.Array as Array
import Data.DateTime.Instant as Instant
import Effect.Exception as Exception
import Registry.Version as Version
import Spago.Command.Init (DefaultConfigOptions(..))
import Spago.Command.Init as Init
import Spago.Core.Config as Config
import Spago.FS as FS
import Spago.Log (LogVerbosity(..))
import Spago.Path as Path
import Spago.Purs (getPurs)
import Test.Spec (SpecT)
import Test.Spec as Spec

spec :: SpecT Aff TestDirs Identity Unit
spec =
  Spec.describe "BuildInfo.purs" do

    let
      mkExpectedStdout { spago, rest } = do
        let logOptions = { logOptions: { color: false, verbosity: LogQuiet, startingTime: Instant.fromDateTime bottom } }
        purs <- runSpago logOptions getPurs
        spagoResult <- spago [ "--version" ]
        sVersion <- case spagoResult of
          Left r -> MonadError.throwError $ Exception.error r.message
          Right r -> pure r.stdout
        pure
          $ Array.intercalate "\n"
          $
            [ "pursVersion: " <> Version.print purs.version
            , "spagoVersion: " <> sVersion
            ]
          <> rest
      pursModuleUsingBuildInfo packages =
        [ "import Prelude"
        , ""
        , "import Spago.Generated.BuildInfo as BI"
        , "import Effect.Console as Console"
        , "import Effect (Effect)"
        , ""
        , "main :: Effect Unit"
        , "main = do"
        , "  Console.log $ \"pursVersion: \" <> BI.pursVersion"
        , "  Console.log $ \"spagoVersion: \" <> BI.spagoVersion"
        ]
          <> -- ensure all packages within the workspace are reference-able
            ( packages <#> \packageName ->
                "  Console.log $ \"" <> packageName <> ": \" <> BI.packages." <> packageName
            )

    Spec.describe "using generated 'BuildInfo.purs' file in single-package context" do

      let
        packageName = "foo"
        srcAndTestContent = pursModuleUsingBuildInfo [ packageName ]

        setupSinglePackage spago = do
          spago [ "init", "--name", packageName ] >>= shouldBeSuccess
          FS.writeTextFile (Path.global "src/Main.purs") $ writeMain srcAndTestContent
          FS.writeTextFile (Path.global "test/Test/Main.purs") $ writeTestMain srcAndTestContent

      Spec.it ("'spago build' works") \{ spago } -> do
        setupSinglePackage spago

        spago [ "build" ] >>= shouldBeSuccess

      Spec.it ("'spago build -p' works") \{ spago } -> do
        setupSinglePackage spago

        spago [ "build", "-p", packageName ] >>= shouldBeSuccess

      let
        runAndTestCommands = do
          command <- [ "run", "test" ]
          selected <- [ false, true ]
          pure case selected of
            false -> [ command ]
            true -> [ command, "-p", packageName ]

      for_ runAndTestCommands \command -> do
        Spec.it ("'spago " <> Array.intercalate " " command <> " works") \{ spago } -> do
          setupSinglePackage spago
          expected <- mkExpectedStdout { spago, rest: [ "foo: 0.0.0" ] }
          spago command >>= checkOutputsStr { stderrStr: Nothing, stdoutStr: Just expected, result: isRight }

    Spec.describe "using generated 'BuildInfo.purs' file in multi-package context" do

      let
        packages = [ "foo", "bar", "baz" ]
        setupPolyrepo = do
          FS.writeYamlFile Config.configCodec (Path.global "spago.yaml")
            $ Init.defaultConfig'
            $ WorkspaceOnly { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1" }
          for_ packages \packageName -> do
            let package = Path.global packageName
            FS.mkdirp package
            FS.writeYamlFile Config.configCodec (package </> "spago.yaml")
              $ mkPackageOnlyConfig { packageName, srcDependencies: [ "prelude", "effect", "console" ] }
                  [ configAddTestMain ]
            let
              src = package </> "src"
              test = package </> "test" </> "Test"
              fileContent = pursModuleUsingBuildInfo packages

            FS.mkdirp src
            FS.mkdirp test
            FS.writeTextFile (src </> "Main.purs") $ writePursFile
              { moduleName: mkSrcModuleName packageName
              , rest: fileContent
              }
            FS.writeTextFile (test </> "Main.purs") $ writePursFile
              { moduleName: mkTestModuleName packageName
              , rest: fileContent
              }

      Spec.it ("'spago build' works") \{ spago } -> do
        setupPolyrepo
        spago [ "build" ] >>= shouldBeSuccess

      Spec.before_ setupPolyrepo do

        let packagesWithVersion = (\p -> p <> ": 0.0.0") <$> packages

        for_ packages \package -> do
          let
            srcMain = mkSrcModuleName package
          Spec.it ("'spago build -p " <> package <> "' works") \{ spago } -> do
            spago [ "build", "-p", package ] >>= shouldBeSuccess

          Spec.it ("'spago run -p " <> package <> " --main " <> srcMain <> "' works") \{ spago } -> do
            expected <- mkExpectedStdout { spago, rest: packagesWithVersion }
            spago [ "run", "-p", package, "--main", srcMain ] >>= checkOutputsStr { stderrStr: Nothing, stdoutStr: Just expected, result: isRight }

          Spec.it ("'spago test -p " <> package <> "' works") \{ spago } -> do
            expected <- mkExpectedStdout { spago, rest: packagesWithVersion }
            spago [ "test", "-p", package ] >>= checkOutputsStr { stderrStr: Nothing, stdoutStr: Just expected, result: isRight }

