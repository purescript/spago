module Test.Spago.Build.BuildInfo where

import Test.Prelude

import Data.Array as Array
import Node.Path as Path
import Registry.Version as Version
import Spago.Command.Init (DefaultConfigOptions(..))
import Spago.Command.Init as Init
import Spago.Core.Config as Config
import Spago.FS as FS
import Test.Spec (SpecT)
import Test.Spec as Spec

spec :: SpecT Aff TestDirs Identity Unit
spec =
  Spec.describe "BuildInfo.purs" do

    let
      pursModuleUsingBuildInfo packages =
        [ "import Prelude"
        , ""
        , "import Spago.Generated.BuildInfo as BI"
        , "import Effect.Console as Console"
        , "import Effect (Effect)"
        , ""
        , "main :: Effect Unit"
        , "main = do"
        , "  Console.log $ \"pursVersion: \" <> BI.buildInfo.pursVersion"
        , "  Console.log $ \"spagoVersion: \" <> BI.buildInfo.spagoVersion"
        ]
          <> -- ensure all packages within the workspace are reference-able
            ( packages <#> \packageName ->
                "  Console.log $ \"" <> packageName <> ": \" <> BI.buildInfo.packages." <> packageName
            )

    Spec.describe "using generated 'BuildInfo.purs' file in single-package context" do

      let
        packageName = "foo"
        srcAndTestContent = pursModuleUsingBuildInfo [ packageName ]

        setupSinglePackage spago = do
          spago [ "init", "--name", packageName ] >>= shouldBeSuccess
          FS.writeTextFile (Path.concat [ "src", "Main.purs" ]) $ writeMain srcAndTestContent
          FS.writeTextFile (Path.concat [ "test", "Test", "Main.purs" ]) $ writeTestMain srcAndTestContent

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
        Spec.it ("'spago " <> Array.intercalate " " command <> " works") \{ spago, fixture } -> do
          setupSinglePackage spago

          spago command >>= shouldBeSuccessOutput (fixture "build-info/single-package-stdout.txt")

    Spec.describe "using generated 'BuildInfo.purs' file in multi-package context" do

      let
        packages = [ "foo", "bar", "baz" ]
        setupPolyrepo = do
          FS.writeYamlFile Config.configCodec "spago.yaml"
            $ Init.defaultConfig'
            $ WorkspaceOnly { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1" }
          for_ packages \packageName -> do
            FS.mkdirp packageName
            FS.writeYamlFile Config.configCodec (Path.concat [ packageName, "spago.yaml" ])
              $ mkPackageOnlyConfig { packageName, srcDependencies: [ "prelude", "effect", "console" ] }
                  [ configAddTestMain ]
            let
              src = Path.concat [ packageName, "src" ]
              test = Path.concat [ packageName, "test", "Test" ]
              fileContent = pursModuleUsingBuildInfo packages

            FS.mkdirp src
            FS.mkdirp test
            FS.writeTextFile (Path.concat [ src, "Main.purs" ]) $ writePursFile
              { moduleName: mkSrcModuleName packageName
              , rest: fileContent
              }
            FS.writeTextFile (Path.concat [ test, "Main.purs" ]) $ writePursFile
              { moduleName: mkTestModuleName packageName
              , rest: fileContent
              }

      Spec.it ("'spago build' works") \{ spago } -> do
        setupPolyrepo
        spago [ "build" ] >>= shouldBeSuccess

      Spec.before_ setupPolyrepo do

        for_ packages \package -> do
          let
            srcMain = mkSrcModuleName package
          Spec.it ("'spago build -p " <> package <> "' works") \{ spago } -> do
            spago [ "build", "-p", package ] >>= shouldBeSuccess

          Spec.it ("'spago run -p " <> package <> " --main " <> srcMain <> "' works") \{ spago, fixture } -> do
            spago [ "run", "-p", package, "--main", srcMain ] >>= shouldBeSuccessOutput (fixture "build-info/multi-package-stdout.txt")

          Spec.it ("'spago test -p " <> package <> "' works") \{ spago, fixture } -> do
            spago [ "test", "-p", package ] >>= shouldBeSuccessOutput (fixture "build-info/multi-package-stdout.txt")

