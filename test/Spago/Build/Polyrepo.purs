-- | Each test in this module describes the structure for each test via a Mermaid.js diagram.
-- | If the diagram isn't clear enough, paste the diagram code into a GitHub
-- | issue/PR and preview it to visualize it.
-- | For a guide on how to write such a diagram,
-- | see https://mermaid.js.org/syntax/flowchart.html
module Test.Spago.Build.Polyrepo where

import Test.Prelude

import Data.Array as Array
import Data.Set.NonEmpty as NonEmptySet
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Node.Path as Path
import Registry.Version as Version
import Spago.Command.Init (DefaultConfigOptions(..))
import Spago.Command.Init as Init
import Spago.Core.Config as Config
import Spago.FS as FS
import Test.Spec (SpecT)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Spec.Assertions.String as AssertString

spec :: SpecT Aff TestDirs Identity Unit
spec = Spec.describe "polyrepo" do

  let
    writeWorkspaceOnlySpagoYamlFile = do
      -- `spago [ "init" ]` will create files that we will immediately
      -- delete (i.e. `src/Main.purs` and `test/Main.purs`)
      -- or overwrite (i.e. `spago.yaml`). So, we don't call it here.
      FS.writeYamlFile Config.configCodec "spago.yaml"
        $ Init.defaultConfig'
        $ WorkspaceOnly { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1" }

    -- | packageToModuleName "package-name" = "PACKAGE.NAME"
    packageToModuleName packageName =
      String.toUpper (String.replaceAll (Pattern "-") (Replacement ".") packageName)

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

  Spec.describe "topological build order" do

    {-
    ```mermaid
    flowchart TD
      subgraph "Case 1"
        A ---> Dep0["effect, console, prelude"]
        B ---> Dep0
      end
    ```
    -}
    Spec.it "Case 1 (independent packages) builds" \{ spago } -> do
      writeWorkspaceOnlySpagoYamlFile
      void $ setupDir
        { packageName: "package-a"
        , spagoYaml: Init.defaultConfig' $ PackageOnly
            { name: mkPackageName "case-one-package-a"
            , dependencies: [ "console", "effect", "prelude" ]
            , test: Just { moduleMain: "Subpackage.A.Test.Main" }
            , build: Nothing
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
            , build: Nothing
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

    {-
    ```mermaid
    flowchart TD
      subgraph "Case 2"
        A2 ---> effect2
        A2 ---> console2
        A2 ---> prelude2
        A2 ---> Shared2
        B2 ---> effect2
        B2 ---> console2
        B2 ---> prelude2
        B2 ---> Shared2
        Shared2 ---> prelude2
      end
    ```
    -}
    Spec.it "Case 2 (shared dependencies packages) builds" \{ spago } -> do
      writeWorkspaceOnlySpagoYamlFile
      void $ setupDir
        { packageName: "package-shared"
        , spagoYaml: Init.defaultConfig' $ PackageOnly
            { name: mkPackageName "case-two-package-shared"
            , dependencies: [ "prelude" ]
            , test: Nothing
            , build: Nothing
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
            , build: Nothing
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
            , build: Nothing
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

    {-
    ```mermaid
    flowchart TD
      subgraph "Case 3"
        A3 ---> effect3
        A3 ---> console3
        A3 ---> prelude3
        A3 ---> B3
        A3 ---> C3
        B3 ---> effect3
        B3 ---> console3
        B3 ---> prelude3
        B3 ---> C3
        C3 ---> prelude3
      end
    ```
    -}
    Spec.it "Case 3 (dependencies: A&B -> C; A -> B) builds" \{ spago } -> do
      writeWorkspaceOnlySpagoYamlFile
      void $ setupDir
        { packageName: "package-c"
        , spagoYaml: Init.defaultConfig' $ PackageOnly
            { name: mkPackageName "case-three-package-c"
            , dependencies: [ "prelude" ]
            , test: Nothing
            , build: Nothing
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
            , build: Nothing
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
            , build: Nothing
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

  {-
  ```mermaid
  flowchart TD
    subgraph "Case 4 (duplicate module)"
      A4 ---> effect4
      A4 ---> console4
      A4 ---> prelude4
      B4 ---> effect4
      B4 ---> console4
      B4 ---> prelude4
      C4 ---> prelude4
    end
  ```
  -}
  Spec.it "Declaring 2+ modules with the same name across 2+ packages fails to build" \{ spago } -> do
    writeWorkspaceOnlySpagoYamlFile
    void $ setupDir
      { packageName: "package-c"
      , spagoYaml: Init.defaultConfig' $ PackageOnly
          { name: mkPackageName "case-four-package-c"
          , dependencies: [ "prelude" ]
          , test: Nothing
          , build: Nothing
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
          , build: Nothing
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
          , build: Nothing
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

  Spec.describe "warning censoring and error-promotion" do
    let
      setupPackageWithUnusedNameWarning packageName deps strict censorShadowedName = void $ setupDir
        { packageName: packageName
        , spagoYaml: Init.defaultConfig' $ PackageOnly
            { name: mkPackageName packageName
            , dependencies: [ "prelude" ] <> deps
            , build: Just
                { strict: pure strict
                , censorProjectCodes: if censorShadowedName then Just $ NonEmptySet.singleton "UnusedName" else Nothing
                }
            , test: Nothing
            }
        , srcMain: mkModuleContent
            { modName: "Subpackage." <> packageToModuleName packageName
            , imports: []
            , body:
                [ ""
                , "packageName :: String -> String"
                , "packageName foo = "
                , "  \"package\" <> \"" <> packageName <> "\""
                ]
            }
        , testMain: Nothing
        }
    Spec.it "build succeeds when 'strict: true' because warnings were censored" \{ spago } -> do
      writeWorkspaceOnlySpagoYamlFile
      setupPackageWithUnusedNameWarning "package-a" [] true true
      setupPackageWithUnusedNameWarning "package-b" [ "package-a" ] true true
      setupPackageWithUnusedNameWarning "package-c" [ "package-a", "package-b" ] true true
      spago [ "build" ] >>= shouldBeSuccess

    Spec.it "build fails when 'strict: true' and warnings were not censored'" \{ spago } -> do
      writeWorkspaceOnlySpagoYamlFile
      setupPackageWithUnusedNameWarning "package-a" [] true true
      setupPackageWithUnusedNameWarning "package-b" [ "package-a" ] true false -- no censoring, so this will fail to build
      setupPackageWithUnusedNameWarning "package-c" [ "package-a", "package-b" ] true true
      let
        hasUnusedWarningError stdErr = do
          let exp = "[1/1 UnusedName] package-b/src/Main.purs:6:13"
          unless (String.contains (Pattern exp) stdErr) do
            Assert.fail $ "STDERR did not contain texts:\n" <> exp <> "\n\nStderr was:\n" <> stdErr
      spago [ "build" ] >>= check { stdout: mempty, stderr: hasUnusedWarningError, result: isLeft }

  Spec.describe "passing --ensure-ranges flag..." do
    let
      setupNonRootPackage packageName = void $ setupDir
        { packageName: packageName
        , spagoYaml: Init.defaultConfig' $ PackageOnly
            { name: mkPackageName packageName
            , dependencies: [ "prelude" ]
            , test: Nothing
            , build: Nothing
            }
        , srcMain: mkModuleContent
            { modName: "Subpackage." <> packageToModuleName packageName
            , imports: []
            , body:
                [ ""
                , "packageName :: String"
                , "packageName = \"package\" <> \"" <> packageName <> "\""
                ]
            }
        , testMain: Nothing
        }

    Spec.it "when root package exists adds ranges to the root package" \{ spago } -> do
      spago [ "init", "--package-set", "0.0.1" ] >>= shouldBeSuccess
      setupNonRootPackage "non-root-package-a"
      setupNonRootPackage "non-root-package-b"
      spago [ "build", "--ensure-ranges" ] >>= shouldBeSuccess
      spagoYaml <- FS.readTextFile "spago.yaml"
      spagoYaml `AssertString.shouldContain` "- prelude: \">=6.0.1 <7.0.0\""

    Spec.it "when root package does not exist fails to build" \{ spago } -> do
      writeWorkspaceOnlySpagoYamlFile
      -- Note: we have to create at least two subpackages
      -- Otherwise, spago will automatically select the only package available,
      -- even if it's a non-root package.
      setupNonRootPackage "non-root-package-a"
      setupNonRootPackage "non-root-package-b"
      let
        hasNoRootPackageError stdErr = do
          let
            msg = Array.intercalate "\n"
              [ "No package found in the root configuration."
              , "Please use the `-p` flag to select a package in which to add ranges."
              ]
          unless (String.contains (Pattern msg) stdErr) do
            Assert.fail $ "STDERR did not contain text:\n" <> msg <> "\n\nStderr was:\n" <> stdErr
      spago [ "build", "--ensure-ranges" ] >>= check { stdout: mempty, stderr: hasNoRootPackageError, result: isLeft }
