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
        result' = result
          { stdout = result.stdout
              -- Take the oldest lines of output - the list of package sets will grow all the time
              # String.split (Pattern "\n")
              # Array.take 200
              # String.joinWith "\n"
          }
      shouldBeSuccessOutput (fixture "registry-list-package-sets.txt") result'

    Spec.it "list only latest package sets for compiler" \{ spago, fixture } -> do
      result <- spago [ "registry", "package-sets", "--latest" ]
      let
        result' = result
          { stdout = result.stdout
              -- Take the oldest lines of output - the list of package sets will grow all the time
              # String.split (Pattern "\n")
              # Array.take 7
              # String.joinWith "\n"
          }
      shouldBeSuccessOutput (fixture "registry-list-package-sets-latest.txt") result'
