module Test.Spago.Build.Polyrepo where

import Test.Prelude

import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String
import Node.FS.Aff as FSA
import Node.Path as Path
import Registry.Version as Version
import Spago.Command.Init (DefaultConfigOptions(..))
import Spago.Command.Init as Init
import Spago.Core.Config as Config
import Spago.FS as FS
import Test.Spec (SpecT)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: SpecT Aff TestDirs Identity Unit
spec = Spec.describe "polyrepo" do

  let
    spagoInitCleanupNonPackageFiles spago = do
      spago [ "init" ] >>= shouldBeSuccess
      FSA.unlink $ Path.concat [ "src", "Main.purs" ]
      FSA.unlink $ Path.concat [ "test", "Test", "Main.purs" ]
      FSA.unlink "spago.yaml"
      FS.writeYamlFile Config.configCodec "spago.yaml"
        $ Init.defaultConfig'
        $ WorkspaceOnly { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1" }

    setupDir { packageName, spagoYaml, srcMain, testMain } = do
      let
        src = Path.concat [ packageName, "src" ]
        test = Path.concat [ packageName, "test" ]
        copyTemplate templateStr path = do
          for_ templateStr \fileContent ->
            FS.writeTextFile (Path.concat $ Array.cons packageName path) fileContent

      FS.mkdirp src
      FS.mkdirp test
      FS.writeYamlFile Config.configCodec (Path.concat [ packageName, "spago.yaml" ]) spagoYaml
      copyTemplate srcMain [ "src", "Main.purs" ]
      copyTemplate testMain [ "test", "Main.purs" ]
      pure { src, test }

    mkModuleContent { modName, imports, body } = Just $ Array.intercalate "\n"
      $
        [ "module " <> modName <> " where"
        , ""
        , "import Prelude"
        ]
      <> imports
      <> body

    mkMainModuleContent { modName, imports, logStatement, packageName } =
      mkModuleContent
        { modName
        , imports:
            [ ""
            , "import Effect (Effect)"
            , "import Effect.Console (log)"
            ]
              <> imports
        , body:
            [ ""
            , "main :: Effect Unit"
            , "main = do"
            , "  " <> logStatement
            ]
              <>
                ( packageName # maybe [] \pkgName ->
                    [ ""
                    , "packageName :: String"
                    , "packageName = \"" <> pkgName <> "\""
                    ]
                )
        }

  Spec.it "Case 1 (independent packages) builds" \{ spago } -> do
    spagoInitCleanupNonPackageFiles spago
    void $ setupDir
      { packageName: "package-a"
      , spagoYaml: Init.defaultConfig' $ PackageOnly
          { name: mkPackageName "case-one-package-a"
          , dependencies: [ "console", "effect", "prelude" ]
          , test: Just { moduleMain: "Subpackage.A.Test.Main" }
          }
      , srcMain: mkMainModuleContent
          { modName: "Subpackage.A.Main"
          , imports: []
          , logStatement: "log packageName"
          , packageName: Just "packageA"
          }
      , testMain: mkMainModuleContent
          { modName: "Subpackage.A.Test.Main"
          , imports: [ "import Subpackage.A.Main as Package" ]
          , logStatement: "log $ \"Test for \" <> Package.packageName"
          , packageName: Nothing
          }
      }
    void $ setupDir
      { packageName: "package-b"
      , spagoYaml: Init.defaultConfig' $ PackageOnly
          { name: mkPackageName "case-one-package-b"
          , dependencies: [ "console", "effect", "prelude" ]
          , test: Just { moduleMain: "Subpackage.B.Test.Main" }
          }
      , srcMain: mkMainModuleContent
          { modName: "Subpackage.B.Main"
          , imports: []
          , logStatement: "log packageName"
          , packageName: Just "packageB"
          }
      , testMain: mkMainModuleContent
          { modName: "Subpackage.B.Test.Main"
          , imports: [ "import Subpackage.B.Main as Package" ]
          , logStatement: "log $ \"Test for \" <> Package.packageName"
          , packageName: Nothing
          }
      }
    spago [ "build", "--verbose" ] >>= shouldBeSuccess

  Spec.it "Case 2 (shared dependencies packages) builds" \{ spago } -> do
    spagoInitCleanupNonPackageFiles spago
    void $ setupDir
      { packageName: "package-shared"
      , spagoYaml: Init.defaultConfig' $ PackageOnly
          { name: mkPackageName "case-two-package-shared"
          , dependencies: [ "prelude" ]
          , test: Nothing
          }
      , srcMain: mkModuleContent
          { modName: "Subpackage.Shared.Lib"
          , imports: []
          , body:
              [ ""
              , "packageName :: String"
              , "packageName = \"package\" <> \"Shared\""
              ]
          }
      , testMain: Nothing
      }
    void $ setupDir
      { packageName: "package-a"
      , spagoYaml: Init.defaultConfig' $ PackageOnly
          { name: mkPackageName "case-two-package-a"
          , dependencies: [ "console", "effect", "prelude", "case-two-package-shared" ]
          , test: Just { moduleMain: "Subpackage.A.Test.Main" }
          }
      , srcMain: mkMainModuleContent
          { modName: "Subpackage.A.Main"
          , imports: [ "import Subpackage.Shared.Lib as Shared" ]
          , logStatement: "log packageName"
          , packageName: Just "packageA"
          }
      , testMain: mkMainModuleContent
          { modName: "Subpackage.A.Test.Main"
          , imports:
              [ "import Subpackage.A.Main as Package"
              , "import Subpackage.Shared.Lib as Shared"
              ]
          , logStatement: "log $ \"Test for \" <> Package.packageName <> \", not \" <> Shared.packageName"
          , packageName: Nothing
          }
      }
    void $ setupDir
      { packageName: "package-b"
      , spagoYaml: Init.defaultConfig' $ PackageOnly
          { name: mkPackageName "case-two-package-b"
          , dependencies: [ "console", "effect", "prelude", "case-two-package-shared" ]
          , test: Just { moduleMain: "Subpackage.B.Test.Main" }
          }
      , srcMain: mkMainModuleContent
          { modName: "Subpackage.B.Main"
          , imports: [ "import Subpackage.Shared.Lib as Shared" ]
          , logStatement: "log packageName"
          , packageName: Just "packageB"
          }
      , testMain: mkMainModuleContent
          { modName: "Subpackage.B.Test.Main"
          , imports:
              [ "import Subpackage.B.Main as Package"
              , "import Subpackage.Shared.Lib as Shared"
              ]
          , logStatement: "log $ \"Test for \" <> Package.packageName <> \", not \" <> Shared.packageName"
          , packageName: Nothing
          }
      }
    spago [ "build" ] >>= shouldBeSuccess

  Spec.it "Case 3 (dependencies: A&B -> C; A -> B) builds" \{ spago } -> do
    spagoInitCleanupNonPackageFiles spago
    void $ setupDir
      { packageName: "package-c"
      , spagoYaml: Init.defaultConfig' $ PackageOnly
          { name: mkPackageName "case-three-package-c"
          , dependencies: [ "prelude" ]
          , test: Nothing
          }
      , srcMain: mkModuleContent
          { modName: "Subpackage.C.Lib"
          , imports: []
          , body:
              [ ""
              , "packageName :: String"
              , "packageName = \"package\" <> \"C\""
              ]
          }
      , testMain: Nothing
      }
    void $ setupDir
      { packageName: "package-b"
      , spagoYaml: Init.defaultConfig' $ PackageOnly
          { name: mkPackageName "case-three-package-b"
          , dependencies: [ "console", "effect", "prelude", "case-three-package-c" ]
          , test: Just { moduleMain: "Subpackage.B.Test.Main" }
          }
      , srcMain: mkMainModuleContent
          { modName: "Subpackage.B.Main"
          , imports: [ "import Subpackage.C.Lib as C" ]
          , logStatement: "log packageName"
          , packageName: Just "packageB"
          }
      , testMain: mkMainModuleContent
          { modName: "Subpackage.B.Test.Main"
          , imports:
              [ "import Subpackage.B.Main as Package"
              , "import Subpackage.C.Lib as C"
              ]
          , logStatement: "log $ Package.packageName <> \" is not \" <> C.packageName"
          , packageName: Nothing
          }
      }
    void $ setupDir
      { packageName: "package-a"
      , spagoYaml: Init.defaultConfig' $ PackageOnly
          { name: mkPackageName "case-three-package-a"
          , dependencies: [ "console", "effect", "prelude", "case-three-package-b", "case-three-package-c" ]
          , test: Just { moduleMain: "Subpackage.A.Test.Main" }
          }
      , srcMain: mkMainModuleContent
          { modName: "Subpackage.A.Main"
          , imports:
              [ "import Subpackage.B.Main as B"
              , "import Subpackage.C.Lib as C"
              ]
          , logStatement: "log $ packageName <> \" is not \" <> C.packageName <> \" or \" <> B.packageName"
          , packageName: Just "packageA"
          }
      , testMain: mkMainModuleContent
          { modName: "Subpackage.A.Test.Main"
          , imports:
              [ "import Subpackage.A.Main as Package"
              , "import Subpackage.C.Lib as C"
              ]
          , logStatement: "log $ \"Test for \" <> Package.packageName <> \", not \" <> C.packageName"
          , packageName: Nothing
          }
      }
    let
      hasAllPkgsInRightBuildOrder stdErr = do
        let
          exp = Array.intercalate "\n"
            [ "Building packages in the following order:"
            , "1) case-three-package-c"
            , "2) case-three-package-b"
            , "3) case-three-package-a"
            ]
        unless (String.contains (Pattern exp) stdErr) do
          Assert.fail $ "STDERR did not contain text:\n" <> exp <> "\n\nStderr was:\n" <> stdErr
    spago [ "build" ] >>= check { stdout: mempty, stderr: hasAllPkgsInRightBuildOrder, result: isRight }

  Spec.it "Case 4 (2+ packages modules with the same name) fails to build" \{ spago } -> do
    spagoInitCleanupNonPackageFiles spago
    void $ setupDir
      { packageName: "package-c"
      , spagoYaml: Init.defaultConfig' $ PackageOnly
          { name: mkPackageName "case-four-package-c"
          , dependencies: [ "prelude" ]
          , test: Nothing
          }
      , srcMain: mkModuleContent
          { modName: "Subpackage.C.Lib"
          , imports: []
          , body:
              [ ""
              , "packageName :: String"
              , "packageName = \"package\" <> \"C\""
              ]
          }
      , testMain: Nothing
      }
    void $ setupDir
      { packageName: "package-b"
      , spagoYaml: Init.defaultConfig' $ PackageOnly
          { name: mkPackageName "case-four-package-b"
          , dependencies: [ "console", "effect", "prelude" ]
          , test: Just { moduleMain: "Subpackage.SameName.Test.Main" }
          }
      , srcMain: mkMainModuleContent
          { modName: "Subpackage.SameName.Main"
          , imports: []
          , logStatement: "log packageName"
          , packageName: Just "packageB"
          }
      , testMain: mkMainModuleContent
          { modName: "Subpackage.SameName.Test.Main"
          , imports: [ "import Subpackage.SameName.Main as Package" ]
          , logStatement: "log Package.packageName"
          , packageName: Nothing
          }
      }
    void $ setupDir
      { packageName: "package-a"
      , spagoYaml: Init.defaultConfig' $ PackageOnly
          { name: mkPackageName "case-four-package-a"
          , dependencies: [ "console", "effect", "prelude" ]
          , test: Just { moduleMain: "Subpackage.SameName.Test.Main" }
          }
      , srcMain: mkMainModuleContent
          { modName: "Subpackage.SameName.Main"
          , imports: []
          , logStatement: "log packageName"
          , packageName: Just "packageA"
          }
      , testMain: mkMainModuleContent
          { modName: "Subpackage.SameName.Test.Main"
          , imports: [ "import Subpackage.SameName.Main as Package" ]
          , logStatement: "log Package.packageName"
          , packageName: Nothing
          }
      }
    let
      hasExpectedModules stdErr = do
        let
          exp = Array.intercalate "\n"
            [ "Detected 2 modules with the same module name across 2 or more packages defined in this workspace."
            , "1) Module \"Subpackage.SameName.Main\" was defined in the following packages:"
            , "  - case-four-package-a   at path: " <> Path.concat [ "package-a", "src", "Main.purs" ]
            , "  - case-four-package-b   at path: " <> Path.concat [ "package-b", "src", "Main.purs" ]
            , "2) Module \"Subpackage.SameName.Test.Main\" was defined in the following packages:"
            , "  - case-four-package-a   at path: " <> Path.concat [ "package-a", "test", "Main.purs" ]
            , "  - case-four-package-b   at path: " <> Path.concat [ "package-b", "test", "Main.purs" ]
            ]

        unless (String.contains (Pattern exp) stdErr) do
          Assert.fail $ "STDERR did not contain text:\n" <> exp <> "\n\nStderr was:\n" <> stdErr
    spago [ "build" ] >>= check { stdout: mempty, stderr: hasExpectedModules, result: isLeft }
