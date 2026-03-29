module Test.Spago.Repl where

import Test.Prelude

import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.parallel $ Spec.around withTempDir do
  Spec.describe "repl" do

    Spec.it "manages .purs-repl file correctly" \{ spago, spago', testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess

      -- init creates .purs-repl
      FS.readTextFile (testCwd </> ".purs-repl") >>= shouldEqual "import Prelude\n"

      -- writes .purs-repl if not there
      FS.unlink $ testCwd </> ".purs-repl"
      spago' (StdinWrite ":q") [ "repl" ] >>= shouldBeSuccess
      FS.readTextFile (testCwd </> ".purs-repl") >>= shouldEqual "import Prelude\n"

      -- does not overwrite .purs-repl if already there
      FS.writeTextFile (testCwd </> ".purs-repl") "import Data.Maybe\n"
      spago' (StdinWrite ":q") [ "repl" ] >>= shouldBeSuccess
      FS.readTextFile (testCwd </> ".purs-repl") >>= shouldEqual "import Data.Maybe\n"
