-- | Each test in this module describes the structure for each test via a Mermaid.js diagram.
-- | If the diagram isn't clear enough, paste the diagram code into a GitHub
-- | issue/PR and preview it to visualize it.
-- | For a guide on how to write such a diagram,
-- | see https://mermaid.js.org/syntax/flowchart.html
module Test.Spago.Build.Polyrepo (spec) where

import Test.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.String (Pattern(..))
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

    -- | `spago [ "init" ]` will create files that we will immediately
    -- | delete (i.e. `src/Main.purs` and `test/Main.purs`)
    -- | or overwrite (i.e. `spago.yaml`). So, we don't call it here.
    writeWorkspaceOnlySpagoYamlFile = do
      FS.writeYamlFile Config.configCodec "spago.yaml"
        $ Init.defaultConfig'
        $ WorkspaceOnly { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1" }

  Spec.describe "inter-workspace package dependencies" do
    {-
    ```mermaid
    flowchart TD
      subgraph "Case 1"
        A ---> Dep0["prelude"]
        B ---> Dep0
      end
    ```
    -}
    Spec.it "Case 1: 'independent packages' builds" \{ spago } -> do
      writeWorkspaceOnlySpagoYamlFile
      void $ setupPackageWithDeps { packageName: "package-a", hasTest: true, deps: [] }
      void $ setupPackageWithDeps { packageName: "package-b", hasTest: true, deps: [] }
      void $ setupPackageWithDeps { packageName: "package-c", hasTest: true, deps: [] }
      spago [ "build" ] >>= shouldBeSuccess

    {-
    ```mermaid
    flowchart TD
      subgraph "Case 2"
        A2 ---> prelude2
        A2 ---> Shared2
        B2 ---> prelude2
        B2 ---> Shared2
        Shared2 ---> prelude2
      end
    ```
    -}
    Spec.it "Case 2: 'shared dependencies packages' builds" \{ spago } -> do
      writeWorkspaceOnlySpagoYamlFile
      shared <- setupPackageWithDeps { packageName: "package-shared", hasTest: false, deps: [] }
      void $ setupPackageWithDeps { packageName: "package-a", hasTest: true, deps: [ shared ] }
      void $ setupPackageWithDeps { packageName: "package-b", hasTest: true, deps: [ shared ] }
      spago [ "build" ] >>= shouldBeSuccess

    {-
    ```mermaid
    flowchart TD
      subgraph "Case 3"
        A3 ---> prelude3
        A3 ---> B3
        A3 ---> C3
        B3 ---> prelude3
        B3 ---> C3
        C3 ---> prelude3
      end
    ```
    -}
    Spec.it "Case 3: 'dependencies: A&B -> C; A -> B' builds" \{ spago, fixture } -> do
      writeWorkspaceOnlySpagoYamlFile
      pkgC <- setupPackageWithDeps { packageName: "package-c", hasTest: false, deps: [] }
      pkgB <- setupPackageWithDeps { packageName: "package-b", hasTest: true, deps: [ pkgC ] }
      void $ setupPackageWithDeps { packageName: "package-a", hasTest: true, deps: [ pkgC, pkgB ] }
      spago [ "build" ] >>= shouldBeSuccess
      checkFixture "spago.lock" (fixture "polyrepo.lock")

  {-
  ```mermaid
  flowchart TD
    subgraph "Case 4 (duplicate module)"
      A4 ---> prelude4
      B4 ---> prelude4
      C4 ---> prelude4
    end
  ```
  -}
  Spec.it "declaring 2+ modules with the same name across 2+ packages fails to build" \{ spago } -> do
    writeWorkspaceOnlySpagoYamlFile
    let
      sameModuleName = "SameModuleName"
      setupPackage packageName samePkgName = do
        void $ setupDir
          { packageName: packageName
          , spagoYaml: mkPackageOnlyConfig { packageName, srcDependencies: [ "prelude" ] } []
          , srcMain: mkModuleContent
              { modName: if samePkgName then sameModuleName else mkSrcModuleName packageName
              , imports: []
              , body:
                  [ ""
                  , "packageName :: String"
                  , "packageName = \"" <> packageName <> "\""
                  ]
              }
          , testMain: Nothing
          }
    setupPackage "package-a" true
    setupPackage "package-b" true
    setupPackage "package-c" false
    let
      hasExpectedModules stdErr = do
        let exp = "Module " <> sameModuleName <> " has been defined multiple times"

        unless (String.contains (Pattern exp) stdErr) do
          Assert.fail $ "STDERR did not contain text:\n" <> exp <> "\n\nStderr was:\n" <> stdErr
    spago [ "build" ] >>= check { stdout: mempty, stderr: hasExpectedModules, result: isLeft }

  {-
  ```mermaid
  flowchart TD
    subgraph "Case 1.1"
      A ---> Dep0["prelude"]
      B ---> Dep1["either-from-git"]
    end
  ```
  -}
  Spec.it "#1161 regression: 'subpackage with disjoint git dependency' builds" \{ spago } -> do
    -- Condition 1: there must be a root package
    let
      conf = Init.defaultConfig
        { name: mkPackageName "root"
        , withWorkspace: Just
            { setVersion: Just $ unsafeFromRight $ Version.parse "20.0.1"
            }
        , testModuleName: "Test.Main"
        }
    -- Condition 2: one of the dependencies of the subpackage must be fetched from git.
    -- This is a problem only when the git dependency is not a dependency of the root package.
    FS.writeYamlFile Config.configCodec "spago.yaml"
      ( conf
          { workspace = conf.workspace # map
              ( _
                  { extra_packages = Just $ Map.fromFoldable
                      -- This commit is for the `v6.1.0` release
                      [ Tuple (mkPackageName "either") $ Config.ExtraRemotePackage $ Config.RemoteGitPackage
                          { git: "https://github.com/purescript/purescript-either.git"
                          , ref: "af655a04ed2fd694b6688af39ee20d7907ad0763"
                          , subdir: Nothing
                          , dependencies: Just $ mkDependencies [ "control", "invariant", "maybe", "prelude" ]
                          }
                      ]
                  }
              )
          }
      )
    -- Condition 3: the workspace needs to contain a subpackage that is using the git dependency
    void $ setupPackageWithDeps { packageName: "package-b", hasTest: true, deps: [ { dep: "either", alias: "EITHER", import: "import Data.Either" } ] }
    -- Lastly, this broke only when building the root package
    spago [ "build", "-p", "root" ] >>= shouldBeSuccess
    -- Or getting its graph
    spago [ "uninstall", "-p", "root", "console", "effect", "prelude" ] >>= shouldBeSuccess
    spago [ "build", "-p", "root", "--pedantic-packages" ] >>= shouldBeSuccess

  Spec.describe "warning censoring and error-promotion" do
    let
      setupPackageWithUnusedNameWarning packageName deps strict censorShadowedName includeTestCode = do
        let
          mkMainFile isSrc = mkModuleContent
            { modName: (if isSrc then mkSrcModuleName else mkTestModuleName) packageName
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
          , spagoYaml: do
              let censorValue = Config.CensorSpecificWarnings $ pure $ Config.ByCode "UnusedName"
              mkPackageOnlyConfig
                { packageName, srcDependencies: [ "prelude" ] <> deps }
                $
                  [ if strict then configAddSrcStrict else identity
                  , if censorShadowedName then configAddSrcCensor censorValue else identity
                  ]
                <>
                  ( if not includeTestCode then []
                    else
                      [ configAddTestMain
                      , if strict then configAddTestStrict else identity
                      , if censorShadowedName then configAddTestCensor censorValue else identity
                      ]
                  )
          , srcMain: mkMainFile true
          , testMain: if includeTestCode then mkMainFile false else Nothing
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
        , spagoYaml: mkPackageOnlyConfig { packageName, srcDependencies: [ "prelude" ] } []
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
        , spagoYaml: mkPackageOnlyConfig
            { packageName, srcDependencies: [ "prelude", "control", "tuples" ] }
            [ if src then configAddSrcPedantic else identity
            , configAddTestMain
            , configAddTestDependencies [ "prelude", "control", "either" ]
            , if test then configAddTestPedantic else identity
            ]
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
                , "import Safe.Coerce as Coerce"
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
      mkTransitiveDepErr isSrc package = do
        let
          { pkg, mkModName, pkgModName } =
            if isSrc then { pkg: "newtype", mkModName: mkSrcModuleName, pkgModName: "Data.Newtype" }
            else { pkg: "safe-coerce", mkModName: mkTestModuleName, pkgModName: "Safe.Coerce" }
        Array.intercalate "\n"
          [ Array.fold
              [ toMsgPrefix isSrc
              , " for package '"
              , package
              , "' import the following transitive dependencies - please add them to the project dependencies, or remove the imports:"
              ]
          , "  " <> pkg
          , "    from `" <> mkModName package <> "`, which imports:"
          , "      " <> pkgModName
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

type SetupPackage = { packageName :: String, hasTest :: Boolean, deps :: Array { dep :: String, import :: String, alias :: String } }

setupPackageWithDeps :: SetupPackage -> Aff { dep :: String, import :: String, alias :: String }
setupPackageWithDeps { packageName, hasTest, deps } = do
  let
    packageAlias = packageToModuleName packageName
    packageNameValues
      | Array.null deps = "\"no deps\""
      | otherwise = Array.intercalate " <> " $ map (\r -> r.alias <> ".packageNameValue") deps
  setupDir
    { packageName: packageName
    , spagoYaml: mkPackageOnlyConfig
        { packageName, srcDependencies: [ "prelude" ] <> map _.dep deps }
        [ configAddTestMain
        , configAddTestDependencies [ "prelude", "console", "effect" ]
        ]
    , srcMain: mkModuleContent
        { modName: mkSrcModuleName packageName
        , imports: map _.import deps
        , body:
            [ ""
            , "libraryUsage :: String"
            , "libraryUsage = packageNameValue <> " <> packageNameValues
            , ""
            , "packageNameValue :: String"
            , "packageNameValue = \"package name \" <> \"" <> packageName <> "\""
            ]
        }
    , testMain:
        if not hasTest then Nothing
        else mkModuleContent
          { modName: mkTestModuleName packageName
          , imports:
              [ ""
              , "import Effect (Effect)"
              , "import Effect.Console (log)"
              , "import " <> mkSrcModuleName packageName <> " as " <> packageAlias
              ]
                <> (map _.import deps)
          , body:
              [ ""
              , "main :: Effect Unit"
              , "main = do"
              , "  log $ \"Test for \" <> " <> packageAlias <> ".packageNameValue <> " <> packageNameValues
              ]
          }
    }
  pure
    { dep: packageName
    , import: "import " <> mkSrcModuleName packageName <> " as " <> packageAlias
    , alias: packageAlias
    }

type SetupDir = { packageName :: String, spagoYaml :: Config.Config, srcMain :: Maybe String, testMain :: Maybe String }

setupDir :: SetupDir -> Aff Unit
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

type ModuleContent = { body :: Array String, imports :: Array String, modName :: String }

mkModuleContent :: ModuleContent -> Maybe String
mkModuleContent { modName, imports, body } = Just $ Array.intercalate "\n"
  $
    [ "module " <> modName <> " where"
    , ""
    , "import Prelude"
    ]
  <> imports
  <> body
