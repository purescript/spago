module Test.Spago.Build.Pedantic (spec) where

import Test.Prelude

import Data.Array as Array
import Data.Map as Map
import Spago.Core.Config (Dependencies(..), Config)
import Spago.FS as FS
import Spago.Paths as Paths
import Test.Spec (SpecT)
import Test.Spec as Spec

spec :: SpecT Aff TestDirs Identity Unit
spec =
  Spec.describe "pedantic packages" do

    Spec.describe "fails when imports from transitive dependencies" do

      -- Here we are importing the `Control.Alt` module, which is in the `control`
      -- package, which comes through `maybe` but we are not importing directly
      Spec.it "appear in the source package" \{ spago, fixture, testCwd } -> do
        setup testCwd spago
          ( defaultSetupConfig
              { installSourcePackages = [ "maybe" ]
              , main = Just
                  [ "import Prelude"
                  , "import Data.Maybe as Maybe"
                  , "import Effect as Effect"
                  , "import Effect.Console as Console"
                  , "import Control.Alt as Alt"
                  , "main = unit"
                  ]
              }
          )
        spago [ "build", "--pedantic-packages" ]
          >>= shouldBeFailureErr (fixture "pedantic/check-direct-import-transitive-dependency.txt")
        editSpagoYaml addPedanticFlagToSrc
        spago [ "build" ]
          >>= shouldBeFailureErr (fixture "pedantic/check-direct-import-transitive-dependency.txt")

      -- We are importing `Control.Alt` in the test package, which is in the `control`
      -- package, which comes through `maybe` but we are not importing directly, so we
      -- should get a transitivity warning about that
      Spec.it "appear in the test package" \{ spago, fixture, testCwd } -> do
        setup testCwd spago
          ( defaultSetupConfig
              { installTestPackages = [ "maybe" ]
              , main = Just
                  [ "import Prelude"
                  , "import Data.Maybe as Maybe"
                  , "import Effect as Effect"
                  , "import Effect.Console as Console"
                  , "main = unit"
                  ]
              , testMain = Just
                  [ "import Prelude"
                  , "import Data.Maybe as Maybe"
                  , "import Effect as Effect"
                  , "import Effect.Console as Console"
                  , "import Control.Alt as Alt"
                  , "main = unit"
                  ]
              }
          )
        -- Here we install `maybe` only in test, so there should be a transitive
        -- warning to install `maybe` in source, since we use it there and the flag
        -- will turn it on for both source and test
        spago [ "build", "--pedantic-packages" ]
          >>= shouldBeFailureErr (fixture "pedantic/check-direct-import-transitive-dependency-both.txt")
        editSpagoYaml addPedanticFlagToTest
        -- Otherwise we can just turn it on for the test, and it will complain only about `control`
        spago [ "build" ]
          >>= shouldBeFailureErr (fixture "pedantic/check-direct-import-transitive-dependency-test.txt")

    Spec.describe "fails with warnings about unused dependencies" do

      -- Here we install `effect` and `console` in the test package, and we don't use them
      -- in the source, so we should get an "unused" warning about them
      Spec.it "in a source package" \{ spago, fixture, testCwd } -> do
        setup testCwd spago
          ( defaultSetupConfig
              { installTestPackages = [ "effect", "console" ]
              , main = Just
                  [ "import Prelude"
                  , "main = unit"
                  ]
              }
          )
        spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "pedantic/check-unused-dependency.txt")
        editSpagoYaml addPedanticFlagToSrc
        spago [ "build" ] >>= shouldBeFailureErr (fixture "pedantic/check-unused-dependency.txt")

      -- Here we do not install `effect` and `console` in the test package, and we don't use them
      -- in the source, so we should get an "unused" warning about them for the source, and a prompt
      -- to install them in test
      Spec.it "in a source package, but they are used in test" \{ spago, fixture, testCwd } -> do
        setup testCwd spago
          ( defaultSetupConfig
              { main = Just
                  [ "import Prelude"
                  , "main = unit"
                  ]
              }
          )
        spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "pedantic/check-unused-dependency-in-source.txt")
        editSpagoYaml addPedanticFlagToSrc
        spago [ "build" ] >>= shouldBeFailureErr (fixture "pedantic/check-unused-dependency.txt")

      -- Complain about the unused `newtype` dependency in the test package
      Spec.it "in a test package" \{ spago, fixture, testCwd } -> do
        setup testCwd spago
          ( defaultSetupConfig
              { installTestPackages = [ "newtype" ]
              , testMain = Just
                  [ "import Prelude"
                  , "main = unit"
                  ]
              }
          )
        -- first just add the flag
        spago [ "build", "--pedantic-packages" ]
          >>= shouldBeFailureErr (fixture "pedantic/check-unused-test-dependency.txt")
        -- then prove that it also works when using it from the config
        editSpagoYaml addPedanticFlagToTest
        spago [ "build" ]
          >>= shouldBeFailureErr (fixture "pedantic/check-unused-test-dependency.txt")

      -- `console` and `effect` are going to be unused for both source and test packages
      Spec.it "in both the source and test packages" \{ spago, fixture, testCwd } -> do
        setup testCwd spago
          ( defaultSetupConfig
              { installSourcePackages = [ "prelude", "effect", "console" ]
              , installTestPackages = [ "prelude", "effect", "console" ]
              , main = Just
                  [ "import Prelude"
                  , "main = unit"
                  ]
              , testMain = Just
                  [ "import Prelude"
                  , "main = unit"
                  ]
              }
          )
        spago [ "build", "--pedantic-packages" ]
          >>= shouldBeFailureErr (fixture "pedantic/check-unused-source-and-test-dependency.txt")
        editSpagoYaml (addPedanticFlagToSrc >>> addPedanticFlagToTest)
        spago [ "build" ]
          >>= shouldBeFailureErr (fixture "pedantic/check-unused-source-and-test-dependency.txt")

    -- The source package adds `control` and `either`, but:
    -- * `either` is unused
    -- * `control` is used, and that's fine
    -- * `newtype` is used but not declare, so we get a "transitive" warning
    -- The test package adds `tuples`, but:
    -- * `tuples` is unused
    -- * `newtype` is transitively imported, but we already warned about that in the source
    --   so consider that fixed
    -- * `either` is transitively imported, and it's going to be removed from the source
    --   dependencies, so we get a "transitive" warning to install it in test
    Spec.it "fails to build and reports deduplicated src and test unused/transitive dependencies" \{ spago, fixture, testCwd } -> do
      setup testCwd spago
        ( defaultSetupConfig
            { installSourcePackages = [ "prelude", "control", "either" ]
            , installTestPackages = [ "tuples" ]
            , main = Just
                [ "import Prelude"
                , "import Data.Newtype as Newtype"
                , "import Control.Alt as Alt"
                , ""
                , "foo :: Int"
                , "foo = 1"
                ]
            , testMain = Just
                [ "import Prelude"
                , "import Data.Newtype (class Newtype)"
                , "import Data.Either (Either(..))"
                , ""
                , "newtype Bar = Bar Int"
                , "derive instance Newtype Bar _"
                , ""
                , "foo :: Either Bar Int"
                , "foo = Right 1"
                ]
            }
        )
      spago [ "build", "--pedantic-packages" ] >>= shouldBeFailureErr (fixture "pedantic/check-pedantic-packages.txt")
      editSpagoYaml (addPedanticFlagToSrc >>> addPedanticFlagToTest)
      spago [ "build" ] >>= shouldBeFailureErr (fixture "pedantic/check-pedantic-packages.txt")

    -- A dependency on `console` will include `effect` as a transitive dependency.
    -- So, if we don't have `effect` as a direct dependency, we'll get a pedantic error
    -- where the fix is to install that missing package.
    -- Following those instructions shouldn't cause an error.
    Spec.it "following installation instructions does not fail with an unrelated pedantic error" \{ spago, fixture, testCwd } -> do
      FS.copyTree { src: fixture "pedantic/follow-instructions", dst: testCwd }
      spago [ "uninstall", "effect" ] >>= shouldBeSuccess
      -- Get rid of "Compiling..." messages
      spago [ "build" ] >>= shouldBeSuccess
      editSpagoYaml (addPedanticFlagToSrc)
      spago [ "build" ] >>= shouldBeFailureErr (fixture "pedantic/pedantic-instructions-initial-failure.txt")
      spago [ "install", "-p", "follow-instructions", "effect" ] >>= shouldBeSuccessErr (fixture "pedantic/pedantic-instructions-installation-result.txt")

    -- Regression test for https://github.com/purescript/spago/pull/1222
    let gitignores = [".spago", "/.spago", ".spago/**"]
    for_ gitignores \gitignore ->
      Spec.it
        (".gitignore does not affect discovery of transitive deps (" <> gitignore <> ")") \{ spago, fixture, testCwd } -> do
          FS.copyTree { src: fixture "pedantic/follow-instructions", dst: testCwd }
          FS.writeTextFile (testCwd </> ".gitignore") gitignore
          spago [ "uninstall", "effect" ] >>= shouldBeSuccess
          -- Get rid of "Compiling..." messages
          spago [ "build" ] >>= shouldBeSuccess
          editSpagoYaml (addPedanticFlagToSrc)
          spago [ "build" ] >>= shouldBeFailureErr (fixture "pedantic/pedantic-instructions-initial-failure.txt")
          spago [ "install", "-p", "follow-instructions", "effect" ] >>= shouldBeSuccessErr (fixture "pedantic/pedantic-instructions-installation-result.txt")

    Spec.it "#1281 treats extra-packages on the local file system as used" \{ spago, fixture, testCwd } -> do
      FS.copyTree { src: fixture "pedantic/1281-local-fs-extra-packages", dst: testCwd }

      Paths.chdir $ testCwd </> "packagea"
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "build", "--pedantic-packages" ]
        >>= shouldBeSuccessErr (fixture "pedantic/1281-local-fs-extra-packages/expected-stderr-used.txt")

      Paths.chdir $ testCwd </> "packagec"
      spago [ "build" ] >>= shouldBeSuccess
      spago [ "build", "--pedantic-packages" ]
        >>= shouldBeFailureErr (fixture "pedantic/1281-local-fs-extra-packages/expected-stderr-unused.txt")

addPedanticFlagToSrc :: Config -> Config
addPedanticFlagToSrc config = config
  { package = config.package <#> \r -> r
      { build = Just
          { pedanticPackages: Just true
          , strict: Nothing
          , censorProjectWarnings: Nothing
          }
      }
  }

addPedanticFlagToTest :: Config -> Config
addPedanticFlagToTest config = config
  { package = config.package <#> \r -> r
      { test = Just
          { main: "Test.Main"
          , pedanticPackages: Just true
          , strict: Nothing
          , censorTestWarnings: Nothing
          , dependencies: maybe (Dependencies Map.empty) _.dependencies r.test
          , execArgs: r.test >>= _.execArgs
          }
      }
  }

type SetupConfig =
  { installSourcePackages :: Array String
  , installTestPackages :: Array String
  , main :: Maybe (Array String)
  , testMain :: Maybe (Array String)
  }

defaultSetupConfig :: SetupConfig
defaultSetupConfig =
  { installSourcePackages: []
  , installTestPackages: []
  , main: Nothing
  , testMain: Nothing
  }

setup :: RootPath -> (Array String -> Aff (Either ExecResult ExecResult)) -> SetupConfig -> Aff Unit
setup root spago config = do
  spago [ "init", "--name", "pedantic" ] >>= shouldBeSuccess
  unless (Array.null config.installSourcePackages) do
    spago ([ "install" ] <> config.installSourcePackages) >>= shouldBeSuccess
  unless (Array.null config.installTestPackages) do
    spago ([ "install", "--test-deps" ] <> config.installTestPackages) >>= shouldBeSuccess
  for_ config.main \main ->
    FS.writeTextFile (root </> "src" </> "Main.purs") $ writeMain main
  for_ config.testMain \testMain ->
    FS.writeTextFile (root </> "test" </> "Test" </> "Main.purs") $ writeTestMain testMain
  -- get rid of "Compiling ..." messages and other compiler warnings
  spago [ "build" ] >>= shouldBeSuccess
