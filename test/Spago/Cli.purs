module Test.Spago.Cli where

import Test.Prelude

import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "CLI command parsing" do
    Spec.it "#1146 on mistyped command option, shows help for the current command, not root help" \{ spago, fixture } -> do
      spago [ "build", "--bogus" ] >>= shouldBeFailureErr (fixture "1146-cli-help/build.txt")
      spago [ "registry", "search", "--bogus" ] >>= shouldBeFailureErr (fixture "1146-cli-help/registry-search.txt")

    Spec.it "#1146 on mistyped command or root option, shows root help" \{ spago, fixture } -> do
      spago [ "bogus" ] >>= shouldBeFailureErr (fixture "1146-cli-help/root-error-command.txt")
      spago [ "--bogus" ] >>= shouldBeFailureErr (fixture "1146-cli-help/root-error-option.txt")

    Spec.it "#1146 can show help and version" \{ spago, fixture } -> do
      spago [ "--help" ] >>= shouldBeSuccessOutput (fixture "1146-cli-help/root-help.txt")
      spago [ "--version" ] >>= shouldBeSuccess
