module Test.Spago.Registry where

import Test.Prelude

import Data.Array as Array
import Data.String (Pattern(..))
import Data.String as String
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "registry" do

    Spec.it "list package sets" \{ spago, fixture } -> do
      result <- spago [ "registry", "package-sets" ]
      let
        updateStdout r = r
          { stdout = r.stdout
              -- Take the oldest lines of output - the list of package sets will grow all the time
              # String.split (Pattern "\n")
              # Array.take 200
              # String.joinWith "\n"
          }
      shouldBeSuccessOutput (fixture "registry-list-package-sets.txt") $ bimap updateStdout updateStdout result

    Spec.it "list only latest package sets for compiler" \{ spago, fixture } -> do
      result <- spago [ "registry", "package-sets", "--latest" ]
      let
        updateStdout r = r
          { stdout = r.stdout
              -- Take the oldest lines of output - the list of package sets will grow all the time
              # String.split (Pattern "\n")
              # Array.take 7
              # String.joinWith "\n"
          }
      shouldBeSuccessOutput (fixture "registry-list-package-sets-latest.txt") $ bimap updateStdout updateStdout result

    Spec.it "query package sets and package info with set associations" \{ spago, fixture } -> do
      -- Test: List packages in a specific package set (table output)
      do
        result <- spago [ "registry", "package-sets", "0.0.1" ]
        let
          updateStdout r = r
            { stdout = r.stdout
                # String.split (Pattern "\n")
                # Array.take 50
                # String.joinWith "\n"
            }
        shouldBeSuccessOutput (fixture "registry-package-set-0.0.1.txt") $ bimap updateStdout updateStdout result

      -- Test: List packages in a specific package set (JSON output)
      spago [ "registry", "package-sets", "0.0.1", "--json" ] >>= shouldBeSuccess

      -- Test: Error for non-existent package set version
      spago [ "registry", "package-sets", "999.999.999" ] >>= shouldBeFailure

      -- Test: Info command shows package sets for versions (text output)
      spago [ "registry", "info", "prelude" ] >>= shouldBeSuccess

      -- Test: Info command with JSON includes package sets
      spago [ "registry", "info", "prelude", "--json" ] >>= shouldBeSuccess
