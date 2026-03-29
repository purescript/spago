module Test.Spago.Sources where

import Test.Prelude

import Data.Array as Array
import Data.String as String
import Node.Platform as Platform
import Node.Process as Process
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.parallel $ Spec.around withTempDir do
  Spec.describe "sources" do

    Spec.it "contains project and subproject sources" \{ spago, fixture, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess

      -- contains both dependencies and project sources
      let
        fixt = case Process.platform of
          Just Platform.Win32 -> fixture "sources-output.win.txt"
          _ -> fixture "sources-output.txt"
      spago [ "sources" ] >>= checkOutputs'
        { stdoutFile: Just fixt
        , stderrFile: Nothing
        , result: isRight
        , sanitize: sortLines
        }

      -- contains subproject sources when selecting a subproject
      _ <- makeSubpackage testCwd { name: "subpackage", moduleName: "Subpackage" }
      let
        fixt2 = case Process.platform of
          Just Platform.Win32 -> fixture "sources-subproject-output.win.txt"
          _ -> fixture "sources-subproject-output.txt"
      spago [ "sources", "-p", "subpackage" ] >>= checkOutputs'
        { stdoutFile: Just fixt2
        , stderrFile: Nothing
        , result: isRight
        , sanitize: sortLines
        }
  where
  sortLines = String.trim >>> String.split (String.Pattern "\n") >>> Array.sort >>> String.joinWith "\n"
