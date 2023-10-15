module Test.Spago.Graph where

import Test.Prelude

import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "graph" do

    Spec.it "can output the module graph in JSON" \{ spago, fixture } -> do
      spago [ "init", "--name", "my-project", "--package-set", "41.2.0" ] >>= shouldBeSuccess
      spago [ "graph", "modules", "--json" ] >>= shouldBeSuccessOutput (fixture "graph-modules.json")

    Spec.it "can output the module graph for graphviz" \{ spago, fixture } -> do
      spago [ "init", "--name", "my-project", "--package-set", "41.2.0" ] >>= shouldBeSuccess
      spago [ "graph", "modules", "--dot" ] >>= shouldBeSuccessOutput (fixture "graph-modules.dot")

    Spec.it "can topologically sort modules" \{ spago, fixture } -> do
      spago [ "init", "--name", "my-project", "--package-set", "41.2.0" ] >>= shouldBeSuccess
      spago [ "graph", "modules", "--topo" ] >>= shouldBeSuccessOutput (fixture "graph-modules-topo.txt")

    Spec.it "can output the package graph in JSON" \{ spago, fixture } -> do
      spago [ "init", "--name", "my-project", "--package-set", "41.2.0" ] >>= shouldBeSuccess
      spago [ "graph", "packages", "--json" ] >>= shouldBeSuccessOutput (fixture "graph-packages.json")

    Spec.it "can output the package graph for graphviz" \{ spago, fixture } -> do
      spago [ "init", "--name", "my-project", "--package-set", "41.2.0" ] >>= shouldBeSuccess
      spago [ "graph", "packages", "--dot" ] >>= shouldBeSuccessOutput (fixture "graph-packages.dot")

    Spec.it "can topologically sort packages" \{ spago, fixture } -> do
      spago [ "init", "--name", "my-project", "--package-set", "41.2.0" ] >>= shouldBeSuccess
      spago [ "graph", "packages", "--topo" ] >>= shouldBeSuccessOutput (fixture "graph-packages-topo.txt")
