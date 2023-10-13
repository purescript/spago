-- | Each test in this module describes the structure for each test via a Mermaid.js diagram.
-- | If the diagram isn't clear enough, paste the diagram code into a GitHub
-- | issue/PR and preview it to visualize it.
-- | For a guide on how to write such a diagram,
-- | see https://mermaid.js.org/syntax/flowchart.html
module Test.Spago.Build.Polyrepo where

import Test.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Node.Path as Path
import Node.Platform as Platform
import Node.Process as Process
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
    escapePathInErrMsg :: Array String -> String
    escapePathInErrMsg = case Process.platform of
      Just Platform.Win32 -> Array.intercalate "\\"
      _ -> Array.intercalate "/"
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

  Spec.describe "inter-workspace package dependencies" do

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
            , test: Just { moduleMain: "Subpackage.A.Test.Main", strict: Nothing, censorTestWarnings: Nothing, pedanticPackages: Nothing, dependencies: Nothing }
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
            , test: Just { moduleMain: "Subpackage.B.Test.Main", strict: Nothing, censorTestWarnings: Nothing, pedanticPackages: Nothing, dependencies: Nothing }
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
      spago [ "build" ] >>= shouldBeSuccess

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
            , test: Just { moduleMain: "Subpackage.A.Test.Main", strict: Nothing, censorTestWarnings: Nothing, pedanticPackages: Nothing, dependencies: Nothing }
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
            , test: Just { moduleMain: "Subpackage.B.Test.Main", strict: Nothing, censorTestWarnings: Nothing, pedanticPackages: Nothing, dependencies: Nothing }
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
            , test: Just { moduleMain: "Subpackage.B.Test.Main", strict: Nothing, censorTestWarnings: Nothing, pedanticPackages: Nothing, dependencies: Nothing }
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
            , test: Just { moduleMain: "Subpackage.A.Test.Main", strict: Nothing, censorTestWarnings: Nothing, pedanticPackages: Nothing, dependencies: Nothing }
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
      spago [ "build" ] >>= shouldBeSuccess

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
          , test: Just { moduleMain: "Subpackage.SameName.Test.Main", strict: Nothing, censorTestWarnings: Nothing, pedanticPackages: Nothing, dependencies: Nothing }
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
          , test: Just { moduleMain: "Subpackage.SameName.Test.Main", strict: Nothing, censorTestWarnings: Nothing, pedanticPackages: Nothing, dependencies: Nothing }
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
        let exp = "Module Subpackage.SameName.Main has been defined multiple times"

        unless (String.contains (Pattern exp) stdErr) do
          Assert.fail $ "STDERR did not contain text:\n" <> exp <> "\n\nStderr was:\n" <> stdErr
    spago [ "build" ] >>= check { stdout: mempty, stderr: hasExpectedModules, result: isLeft }

  Spec.describe "warning censoring and error-promotion" do
    let
      setupPackageWithUnusedNameWarning packageName deps strict censorShadowedName includeTestCode = do
        let
          mkMainFile testPart = mkModuleContent
            { modName: testPart <> "Subpackage." <> packageToModuleName packageName
            , imports: []
            , body:
                [ ""
                , "packageName :: String -> String"
                , "packageName foo = "
                , "  \"package\" <> \"" <> packageName <> "\""
                ]
            }
        void $ setupDir
          { packageName: packageName
          , spagoYaml: Init.defaultConfig' $ PackageOnly
              { name: mkPackageName packageName
              , dependencies: [ "prelude" ] <> deps
              , build: Just
                  { strict: pure strict
                  , censorProjectWarnings:
                      if censorShadowedName then
                        Just $ Config.CensorSpecificWarnings $ pure $ Config.ByCode "UnusedName"
                      else
                        Nothing
                  , pedanticPackages: Nothing
                  }
              , test: Just
                  { moduleMain: "Test.Subpackage." <> packageToModuleName packageName
                  , strict: pure strict
                  , censorTestWarnings:
                      if censorShadowedName then
                        Just $ Config.CensorSpecificWarnings $ pure $ Config.ByCode "UnusedName"
                      else
                        Nothing
                  , pedanticPackages: Nothing
                  , dependencies: Nothing
                  }
              }
          , srcMain: mkMainFile ""
          , testMain: if includeTestCode then mkMainFile "Test." else Nothing
          }
    Spec.it "build succeeds when 'strict: true' because warnings were censored" \{ spago } -> do
      writeWorkspaceOnlySpagoYamlFile
      setupPackageWithUnusedNameWarning "package-a" [] true true false
      setupPackageWithUnusedNameWarning "package-b" [ "package-a" ] true true false
      setupPackageWithUnusedNameWarning "package-c" [ "package-a", "package-b" ] true true false
      let
        paths =
          [ escapePathInErrMsg [ "package-a", "src", "Main.purs:6:13" ]
          , escapePathInErrMsg [ "package-b", "src", "Main.purs:6:13" ]
          , escapePathInErrMsg [ "package-c", "src", "Main.purs:6:13" ]
          ]
        shouldNotHaveWarning stdErr = do
          when (Array.any (\exp -> String.contains (Pattern exp) stdErr) paths) do
            Assert.fail $ "STDERR contained one or more texts:\n" <> show paths <> "\n\nStderr was:\n" <> stdErr
      spago [ "build" ] >>= check { stdout: mempty, stderr: shouldNotHaveWarning, result: isRight }

    Spec.it "build fails when 'strict: true' and warnings were not censored" \{ spago } -> do
      writeWorkspaceOnlySpagoYamlFile
      setupPackageWithUnusedNameWarning "package-a" [] true true false
      -- no censoring, so this will fail to build
      -- include tests, so that we get 2 errors rather than 1
      setupPackageWithUnusedNameWarning "package-b" [ "package-a" ] true false true
      setupPackageWithUnusedNameWarning "package-c" [ "package-a", "package-b" ] true true false
      let
        errs =
          [ "[1/2 UnusedName] " <> escapePathInErrMsg [ "package-b", "src", "Main.purs:6:13" ]
          , "[2/2 UnusedName] " <> escapePathInErrMsg [ "package-b", "test", "Main.purs:6:13" ]
          ]
        hasUnusedWarningError stdErr = do
          unless (Array.any (\exp -> String.contains (Pattern exp) stdErr) errs) do
            Assert.fail $ "STDERR did not contain texts:\n" <> show errs <> "\n\nStderr was:\n" <> stdErr
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

  Spec.describe "pedantic packages" do
    let
      mkSrcModuleName packageName = "Src." <> packageToModuleName packageName
      mkTestModuleName packageName = "Test." <> packageToModuleName packageName

      {-
                                                            /-- tuples (unused dep by `src`)      
      newtype (transitive dep) <-- control (direct dep) <--+ 
                                                            \-- either (unused dep by `test`)

      - src and test both import `Data.Newtype` (from `newtype` package) unnecessarily,
        thereby triggering the 'unused transitive dependency' warning
      - src imports `tuples`
      - test imports `either` because it inherit's `src`'s dependencies implicitly                                  -}
      setupPackage packageName { src, test } = void $ setupDir
        { packageName: packageName
        , spagoYaml: Init.defaultConfig' $ PackageOnly
            { name: mkPackageName packageName
            , dependencies: [ "prelude", "control", "tuples" ]
            , build: Just
                { pedanticPackages: Just src
                , censorProjectWarnings: Nothing
                , strict: Nothing
                }
            , test: Just
                { pedanticPackages: Just test
                , censorTestWarnings: Nothing
                , strict: Nothing
                , moduleMain: mkTestModuleName packageName
                , dependencies: Just $ Config.Dependencies $ Map.fromFoldable $ map (flip Tuple Nothing <<< mkPackageName) [ "prelude", "control", "either" ]
                }
            }
        , srcMain: mkModuleContent
            { modName: mkSrcModuleName packageName
            , imports:
                [ "import Control.Alt as Control"
                , "import Data.Newtype as Newtype"
                ]
            , body:
                [ ""
                , "packageName :: String"
                , "packageName = \"package\" <> \"" <> packageName <> "\""
                ]
            }
        , testMain: mkModuleContent
            { modName: mkTestModuleName packageName
            , imports:
                [ "import Control.Alt as Control"
                , "import Data.Newtype as Newtype"
                ]
            , body:
                [ ""
                , "packageName :: String"
                , "packageName = \"package\" <> \"" <> packageName <> "\""
                ]
            }
        }

      toMsgPrefix isSrc
        | isSrc = "Sources"
        | otherwise = "Tests"

      mkUnusedDepErr isSrc package =
        Array.intercalate "\n"
          [ toMsgPrefix isSrc <> " for package '" <> package <> "' declares unused dependencies - please remove them from the project config:"
          , "  - " <> (if isSrc then "tuples" else "either")
          ]
      mkTransitiveDepErr isSrc package =
        Array.intercalate "\n"
          [ Array.fold
              [ toMsgPrefix isSrc
              , " for package '"
              , package
              , "' import the following transitive dependencies - please add them to the project dependencies, or remove the imports:"
              ]
          , "  newtype"
          , "    from `" <> (if isSrc then mkSrcModuleName else mkTestModuleName) package <> "`, which imports:"
          , "      Data.Newtype"
          ]
    Spec.it "when package config has 'pedantic_packages: true', build fails with expected errors" \{ spago } -> do
      writeWorkspaceOnlySpagoYamlFile
      setupPackage "package-a" { src: true, test: false }
      setupPackage "package-b" { src: false, test: true }
      setupPackage "package-c" { src: true, test: true }

      let
        errs =
          [ mkUnusedDepErr true "package-a"
          , mkTransitiveDepErr true "package-a"
          , mkUnusedDepErr false "package-b"
          , mkTransitiveDepErr false "package-b"
          , mkUnusedDepErr true "package-c"
          , mkTransitiveDepErr true "package-c"
          , mkUnusedDepErr false "package-c"
          , mkTransitiveDepErr false "package-c"
          ]
        hasExpectedErrors stdErr = do
          let unfoundTexts = Array.filter (\exp -> not $ String.contains (Pattern exp) stdErr) errs
          unless (Array.null unfoundTexts) do
            Assert.fail $ "STDERR did not contain expected texts:\n" <> (Array.intercalate "\n\n" unfoundTexts) <> "\n\nStderr was:\n" <> stdErr
      spago [ "build" ] >>= check { stdout: mempty, stderr: hasExpectedErrors, result: isLeft }

    Spec.it "passing --pedantic-packages overrides package and test configs" \{ spago } -> do
      writeWorkspaceOnlySpagoYamlFile
      setupPackage "package-a" { src: true, test: false }
      setupPackage "package-b" { src: false, test: true }
      setupPackage "package-c" { src: true, test: true }

      let
        errs = do
          pkg <- [ "package-a", "package-b", "package-c" ]
          isSrc <- [ true, false ]
          fn <- [ mkUnusedDepErr, mkTransitiveDepErr ]
          pure $ fn isSrc pkg
        hasExpectedErrors stdErr = do
          let unfoundTexts = Array.filter (\exp -> not $ String.contains (Pattern exp) stdErr) errs
          unless (Array.null unfoundTexts) do
            Assert.fail $ "STDERR did not contain expected texts:\n" <> (Array.intercalate "\n\n" unfoundTexts) <> "\n\nStderr was:\n" <> stdErr
      spago [ "build", "--pedantic-packages" ] >>= check { stdout: mempty, stderr: hasExpectedErrors, result: isLeft }
