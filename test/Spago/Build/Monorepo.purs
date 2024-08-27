module Test.Spago.Build.Monorepo (spec) where

import Test.Prelude

import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String
import Node.Path as Path
import Spago.Cmd as Cmd
import Spago.FS as FS
import Test.Spec (SpecT)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Spec.Assertions.String as AssertString

spec :: SpecT Aff TestDirs Identity Unit
spec = Spec.describe "monorepo" do

  -- | Each test in this block describes the structure for each test via a Mermaid.js diagram.
  -- | If the diagram isn't clear enough, paste the diagram code into a GitHub
  -- | issue/PR and preview it to visualize it.
  -- | For a guide on how to write such a diagram, see https://mermaid.js.org/syntax/flowchart.html
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
    Spec.it "Case 1: 'independent packages' builds" \{ spago, fixture } -> do
      FS.copyTree { src: fixture "monorepo/case1-independent-packages", dst: "." }
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
    Spec.it "Case 2: 'shared dependencies packages' builds" \{ spago, fixture } -> do
      FS.copyTree { src: fixture "monorepo/case2-shared-dependencies1", dst: "." }
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
      FS.copyTree { src: fixture "monorepo/case3-shared-dependencies2", dst: "." }
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
  Spec.it "declaring 2+ modules with the same name across 2+ packages fails to build" \{ spago, fixture } -> do
    FS.copyTree { src: fixture "monorepo/unique-module-names", dst: "." }
    let
      sameModuleName = "SameModuleName"
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
  Spec.it "#1161 regression: 'subpackage with disjoint git dependency' builds" \{ spago, fixture } -> do
    -- This corner case happens only under very specific conditions:
    -- 1. there must be a root package
    -- 2. one of the dependencies of the subpackage must be fetched from git.
    --    This is a problem only when the git dependency is not a dependency of the root package.
    -- 3. the workspace needs to contain a subpackage that is using the git dependency
    FS.copyTree { src: fixture "monorepo/1161-regression", dst: "." }
    -- Lastly, this broke only when building the root package
    spago [ "build", "-p", "root" ] >>= shouldBeSuccess
    -- Or getting its graph
    spago [ "uninstall", "-p", "root", "console", "effect", "prelude" ] >>= shouldBeSuccess
    spago [ "build", "-p", "root", "--pedantic-packages" ] >>= shouldBeSuccess

  Spec.it "ignore nested workspaces" \{ spago, fixture } -> do
    FS.copyTree { src: fixture "monorepo/ignore-nested-workspaces", dst: "." }
    spago [ "build" ] >>= shouldBeSuccess
    spago [ "build" ] >>= shouldBeSuccessErr (fixture "monorepo/ignore-nested-workspaces/expected-stderr.txt")

  Spec.it "it's possible to reference local packages when using the solver" \{ spago, fixture } -> do
    FS.copyTree { src: fixture "monorepo/local-packages-work-with-solver", dst: "." }
    spago [ "build" ] >>= shouldBeSuccess
    spago [ "build" ] >>= shouldBeSuccessErr (fixture "monorepo/local-packages-work-with-solver/expected-stderr.txt")

  Spec.describe "warning censoring and error-promotion" do

    Spec.it "build succeeds when 'strict: true' because warnings were censored" \{ spago, fixture } -> do
      FS.copyTree { src: fixture "monorepo/strict-true-censored-warnings", dst: "." }
      let
        paths =
          [ escapePathInErrMsg [ "package-a", "src", "Main.purs:6:13" ]
          , escapePathInErrMsg [ "package-b", "src", "Main.purs:6:13" ]
          , escapePathInErrMsg [ "package-c", "src", "Main.purs:6:13" ]
          ]
        shouldNotHaveWarning = assertWarning paths false
      spago [ "build" ] >>= check { stdout: mempty, stderr: shouldNotHaveWarning, result: isRight }

    Spec.it "build fails when 'strict: true' and warnings were not censored" \{ spago, fixture } -> do
      FS.copyTree { src: fixture "monorepo/strict-true-uncensored-warnings", dst: "." }
      let
        errs =
          [ "[1/2 UnusedName] " <> escapePathInErrMsg [ "package-b", "src", "Main.purs:6:13" ]
          , "[2/2 UnusedName] " <> escapePathInErrMsg [ "package-b", "test", "Main.purs:6:13" ]
          ]
        hasUnusedWarningError = assertWarning errs true
      spago [ "build" ] >>= check { stdout: mempty, stderr: hasUnusedWarningError, result: isLeft }

  Spec.describe "passing --ensure-ranges flag..." do

    Spec.it "when root package exists adds ranges to the root package" \{ spago, fixture } -> do
      FS.copyTree { src: fixture "monorepo/ensure-ranges-root-package", dst: "." }
      spago [ "build", "--ensure-ranges" ] >>= shouldBeSuccess
      spagoYaml <- FS.readTextFile "spago.yaml"
      spagoYaml `AssertString.shouldContain` "- prelude: \">=6.0.1 <7.0.0\""

    Spec.it "when root package does not exist fails to build" \{ spago, fixture } -> do
      -- Note: this needs to contain at least two subpackages, otherwise, spago will
      -- automatically select the only package available even if it's a non-root package.
      FS.copyTree { src: fixture "monorepo/ensure-ranges-no-root-package", dst: "." }

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
    {-
                                                          /-- tuples (unused dep by `src`)
    newtype (transitive dep) <-- control (direct dep) <--+
                                                          \-- either (unused dep by `test`)

    - src and test both import `Data.Newtype` (from `newtype` package) unnecessarily,
      thereby triggering the 'unused transitive dependency' warning
    - src imports `tuples`
    - test imports `either` because it inherit's `src`'s dependencies implicitly

    -}

    let
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

    Spec.it "when package config has 'pedantic_packages: true', build fails with expected errors" \{ spago, fixture } -> do
      FS.copyTree { src: fixture "monorepo/pedantic-config", dst: "." }

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

    Spec.it "passing --pedantic-packages overrides package and test configs" \{ spago, fixture } -> do
      FS.copyTree { src: fixture "monorepo/pedantic-flag", dst: "." }

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

    Spec.it "prevents cross-package imports between local packages" \{ spago, fixture } -> do
      FS.copyTree { src: fixture "monorepo/pedantic-cross-package-imports", dst: "." }

      spago [ "build" ] >>= shouldBeSuccess
      spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "monorepo/pedantic-cross-package-imports/expected-stderr.txt")

    Spec.it "#1208: clones a monorepo only once, even if multiple packages from it are needed" \{ spago, fixture } -> do
      FS.copyTree { src: fixture "monorepo/1208-no-double-cloning", dst: "." }

      spago [ "ls", "packages" ] >>=
        shouldBeSuccessErr (fixture "monorepo/1208-no-double-cloning/expected-stderr.txt")

      let
        checkRefs =
          [ "deku-core" /\ "98c67533cc8c399aa643b495d3c02bab963e5b80"
          , "deku-dom" /\ "6b7c392da7782fe0f2e34811e36b11e630e10b26"
          , "deku-css" /\ "4e68a5cec10c91aa3377ce69cc97c276936a1194"
          ]
      for_ checkRefs \(pkg /\ ref) -> do
        let path = Path.concat [ ".spago", "p", pkg, ref ]
        git path [ "rev-parse", "HEAD" ] >>= shouldEqualStr ref

      -- Replace spago.yaml with one that has one more dependency from the same
      -- monorepo, check that the monorepo doesn't need to be cloned again and
      -- can be used in offline mode.
      FS.unlink "spago.yaml"
      FS.copyFile { src: "spago-one-more-dep.yaml", dst: "spago.yaml" }
      spago [ "ls", "packages", "--offline" ] >>=
        shouldBeSuccessErr (fixture "monorepo/1208-no-double-cloning/expected-stderr-one-more-dep.txt")

      let dekuPath = Path.concat [ ".spago", "p", "deku", "276f48adde3d9354f61917f7e9ae2ae7b43df6b2" ]
      git dekuPath [ "rev-parse", "HEAD" ] >>= shouldEqualStr "276f48adde3d9354f61917f7e9ae2ae7b43df6b2"

  where
  git cwd args =
    Cmd.getStdout <$> Cmd.exec "git" args
      Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, cwd = Just cwd }

