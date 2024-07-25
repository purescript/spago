module Test.Spago.Repl where

import Test.Prelude

import Spago.FS as FS
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "repl" do

    Spec.it "writes .purs-repl if not there" \{ spago, spago' } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.readTextFile ".purs-repl" >>= shouldEqual "import Prelude\n"

      FS.unlink ".purs-repl"
      spago' (StdinWrite ":q") [ "repl" ] >>= shouldBeSuccess
      FS.readTextFile ".purs-repl" >>= shouldEqual "import Prelude\n"

    Spec.it "does not write .purs-repl if already there" \{ spago, spago' } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.writeTextFile ".purs-repl" "import Data.Maybe\n"
      spago' (StdinWrite ":q") [ "repl" ] >>= shouldBeSuccess
      FS.readTextFile ".purs-repl" >>= shouldEqual "import Data.Maybe\n"
