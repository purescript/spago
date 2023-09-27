module Test.Spago.Init where

import Test.Prelude

import Node.Path as Path
import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "init" $ do

    Spec.it "sets up a project" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess

    Spec.it "does not overwrite files when initing a project" \{ spago } -> do
      FS.mkdirp "src"
      FS.writeTextFile (Path.concat [ "src", "Main.purs" ]) "Something"
      spago [ "init" ] >>= shouldBeSuccess
      fileContent <- FS.readTextFile (Path.concat [ "src", "Main.purs" ])
      fileContent `Assert.shouldEqual` "Something"

    -- FIXME `-f` not implemented
    -- Spec.it "always succeeds when doing init with force" \{ spago } -> do
    --   spago [ "init" ] >>= shouldBeSuccess
    --   spago [ "init", "-f" ] >>= shouldBeSuccess

    Spec.it "should use user-specified tag if it exists instead of latest release" \({ spago, fixture } :: TestDirs) -> do
      spago [ "init", "--package-set", "9.0.0", "--name", "7368613235362d47665357393342584955783641314b70674c" ] >>= shouldBeSuccess
      checkFixture "spago.yaml" (fixture "older-package-set-tag.yaml")
