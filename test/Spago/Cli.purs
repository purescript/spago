module Test.Spago.Cli where

import Test.Prelude

import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RF
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "CLI command parsing" do
    Spec.it "#1146 on mistyped command option, shows help for the current command, not root help" \{ spago, fixture } -> do
      spago [ "build", "--bogus" ] >>= shouldBeFailureErr' (fixture "1146-cli-help/build.txt")
      spago [ "registry", "search", "--bogus" ] >>= shouldBeFailureErr' (fixture "1146-cli-help/registry-search.txt")

    Spec.it "#1146 on mistyped command or root option, shows root help" \{ spago, fixture } -> do
      spago [ "bogus" ] >>= shouldBeFailureErr' (fixture "1146-cli-help/root-error-command.txt")
      spago [ "--bogus" ] >>= shouldBeFailureErr' (fixture "1146-cli-help/root-error-option.txt")

    Spec.it "#1146 can show help and version" \{ spago, fixture } -> do
      spago [ "--help" ] >>= shouldBeSuccessOutput' (fixture "1146-cli-help/root-help.txt")
      spago [ "--version" ] >>= shouldBeSuccess

  where
  shouldBeSuccessOutput' fixture = checkOutputsWithPatch isRight (Just fixture) Nothing
  shouldBeFailureErr' fixture = checkOutputsWithPatch isLeft Nothing (Just fixture)

  checkOutputsWithPatch result stdout stderr =
    checkOutputs'
      { stdoutFile: stdout
      , stderrFile: stderr
      , result
      , sanitize:
          String.trim
          >>> Regex.replace progNameRegex "Usage: index.dev.js"
          >>> Regex.replace optionsLineRegex " $1"
      }

  -- On Windows progname has the full path like
  -- "Usage: C:\whatever\index.dev.js", but on Unix
  -- it's just "Usage: index.dev.js"
  progNameRegex = unsafeFromRight $ Regex.regex "Usage: .*index\\.dev\\.js" RF.noFlags

  -- This regex catches "hanging" lines that list possible options after a
  -- command, like this:
  --
  --     Usage: index.dev.js build [--option] [--another-option]
  --                               [--third-option] [--foo]
  --                               [-f|--force] [--help]
  --
  -- The second and third line in this example are aligned to whererever the
  -- command name happened to end, and this will be different between Unix and
  -- Windows, because on Unix the command is just the file name `index.dev.js`,
  -- but on Windows it includes the full path, and worse, it's going to be
  -- different path on differemt machines with different configurations.
  --
  -- So to work around this we collapse those "hanging" lines with options by
  -- replacing newline and subsequent wide whitespace with a single space, like:
  --
  --     Usage: index.dev.js build [--option] [--another-option] [--third-option] [--foo] [-f|--force] [--help]
  --
  optionsLineRegex = unsafeFromRight $ Regex.regex "\\n\\s+(\\(\\[-|\\[-|PACKAGE)" RF.global
