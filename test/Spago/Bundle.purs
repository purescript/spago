module Test.Spago.Bundle where

import Test.Prelude

import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "bundle" do

    Spec.it "bundles into an app" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "bundle", "-v", "--bundle-type", "app", "--outfile", "bundle-app.js" ] >>= shouldBeSuccess
      checkFixture "bundle-app.js" (fixture "bundle-app.js")

    Spec.it "bundles into a module" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      -- We pass the `--no-build` flag to skip rebuilding (i.e. we are counting on the previous command
      -- to have built stuff for us)
      -- TODO: get "--no-build" to work
      spago [ "bundle", "--bundle-type=module", "--outfile", "bundle-module.js" ] >>= shouldBeSuccess
      checkFixture "bundle-module.js" (fixture "bundle-module.js")

    Spec.it "bundles an app with source map" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "bundle", "-v", "--outfile", "bundle-app.js", "--source-maps", "--bundle-type", "app" ] >>= shouldBeSuccess
      checkFixture "bundle-app.js" (fixture "bundle-app-map.js")
      checkFixture "bundle-app.js.map" (fixture "bundle-app-map.js.map")

    Spec.it "bundles a module with source map" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "bundle", "--bundle-type", "module", "--outfile", "bundle-module.js", "--source-maps" ] >>= shouldBeSuccess

      checkFixture "bundle-module.js" (fixture "bundle-module-map.js")
      checkFixture "bundle-module.js.map" (fixture "bundle-module-map.js.map")

