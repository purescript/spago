module Test.Spago.Sources where

import Test.Prelude

import Node.Platform as Platform
import Node.Process as Process
import Spago.Command.Init as Init
import Spago.Core.Config as Config
import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "sources" do

    Spec.it "contains both dependencies and project sources" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "sources" ] >>= shouldBeSuccessOutput case Process.platform of
        Just Platform.Win32 -> fixture "sources-output.win.txt"
        _ -> fixture "sources-output.txt"

    Spec.it "contains subproject sources when selecting a subproject" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.mkdirp "subpackage/src"
      FS.mkdirp "subpackage/test"
      FS.writeTextFile "subpackage/src/Main.purs" (Init.srcMainTemplate "Subpackage.Main")
      FS.writeTextFile "subpackage/test/Main.purs" (Init.testMainTemplate "Subpackage.Test.Main")
      FS.writeYamlFile Config.configCodec "subpackage/spago.yaml"
        ( Init.defaultConfig
            (mkPackageName "subpackage")
            Nothing
            "Subpackage.Test.Main"
        )
      spago [ "sources", "-p", "subpackage" ] >>= shouldBeSuccessOutput case Process.platform of
        Just Platform.Win32 -> fixture "sources-subproject-output.win.txt"
        _ -> fixture "sources-subproject-output.txt"
