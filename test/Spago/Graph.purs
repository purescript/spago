module Test.Spago.Graph where

import Test.Prelude

import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "graph" do

    Spec.it "can output module and package graphs in various formats" \{ spago, fixture } -> do
      spago [ "init", "--name", "my-project", "--package-set", "41.2.0" ] >>= shouldBeSuccess
      spago [ "graph", "modules", "--json" ] >>= shouldBeSuccessOutput (fixture "graph-modules.json")
      spago [ "graph", "modules", "--dot" ] >>= shouldBeSuccessOutput (fixture "graph-modules.dot")
      spago [ "graph", "modules", "--topo" ] >>= shouldBeSuccessOutput (fixture "graph-modules-topo.txt")
      spago [ "graph", "packages", "--json" ] >>= shouldBeSuccessOutput (fixture "graph-packages.json")
      spago [ "graph", "packages", "--dot" ] >>= shouldBeSuccessOutput (fixture "graph-packages.dot")
      spago [ "graph", "packages", "--topo" ] >>= shouldBeSuccessOutput (fixture "graph-packages-topo.txt")

    Spec.it "#1281 correctly reports modules from extra-packages on the local file system" \{ spagoIn, fixture, testCwd } -> do
      FS.copyTree { src: fixture "1281-local-fs-extra-packages", dst: testCwd }
      spagoIn (testCwd </> "consumer") [ "graph", "modules" ] >>= shouldBeSuccessOutput (fixture "1281-local-fs-extra-packages/expected-stdout.txt")
