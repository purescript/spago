module Test.Spago.InitSubpackage where

import Test.Prelude

import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RF
import Node.Path as Path
import Spago.FS as FS
import Test.Spec (SpecT)
import Test.Spec as Spec

spec :: SpecT Aff TestDirs Identity Unit
spec =
  Spec.describe "subpackage" do
    Spec.it "sets up a sub-project in a subdirectory" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "init", "--subpackage", "subdir" ] >>= shouldBeSuccess
      checkFixture "subdir/spago.yaml" (fixture "init/subpackage/subdir-spago.yaml")
      spago [ "init", "--subpackage", "subdir2" ] >>= shouldBeSuccess
      checkFixture "subdir2/spago.yaml" (fixture "init/subpackage/subdir2-spago.yaml")

    Spec.it "does not overwrite existing files" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.mkdirp "subdir/src"
      FS.writeTextFile (Path.concat [ "subdir", "src", "Main.purs" ]) "Something"
      spago [ "init", "--subpackage", "subdir" ]
        >>= shouldBeSuccessErr' (fixture "init/subpackage/existing-src-file.txt")
      fileContent <- FS.readTextFile (Path.concat [ "subdir", "src", "Main.purs" ])
      fileContent `shouldEqualStr` "Something"

    Spec.it "warns when --package-set or --use-solver flags are used" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess

      spago [ "init", "--package-set", "9.0.0", "--subpackage", "subdir" ]
        >>= shouldBeSuccessErr' (fixture "init/subpackage/package-set-solver-warning.txt")
      checkFixture "subdir/spago.yaml" (fixture "init/subpackage/subdir-spago.yaml")

      spago [ "init", "--use-solver", "--subpackage", "subdir" ]
        >>= shouldBeSuccessErr' (fixture "init/subpackage/package-set-solver-warning-existing-files.txt")
      checkFixture "subdir/spago.yaml" (fixture "init/subpackage/subdir-spago.yaml")

    Spec.it "does not allow both --name and --subpackage flags" \{ spago, fixture } -> do
      spago [ "init", "--name", "foo", "--subpackage", "bar" ]
        >>= shouldBeFailureErr' (fixture "init/subpackage/conflicting-flags.txt")

  where
  shouldBeSuccessErr' = checkOutputsWithPatch isRight
  shouldBeFailureErr' = checkOutputsWithPatch isLeft

  checkOutputsWithPatch result fixture =
    checkOutputs'
      { stdoutFile: Nothing
      , stderrFile: Just fixture
      , result
      , sanitize:
          String.trim
          >>> withForwardSlashes
          >>> Regex.replace versionsRegex "Found PureScript a.b.c, will use package set x.y.z"
      }

  versionsRegex = unsafeFromRight $
    Regex.regex "Found PureScript \\d+\\.\\d+\\.\\d+, will use package set \\d+\\.\\d+\\.\\d+" RF.noFlags
