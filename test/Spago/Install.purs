module Test.Spago.Install where

import Test.Prelude

import Data.Array as Array
import Data.Map as Map
import Effect.Now as Now
import Node.FS.Aff as FSA
import Node.Path as Path
import Registry.Version as Version
import Spago.Command.Init as Init
import Spago.Core.Config as Config
import Spago.FS as FS
import Spago.Log (LogVerbosity(..))
import Spago.Purs as Purs
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Spec.Assertions as Assertions

spec :: Spec Unit
spec = Spec.around withTempDir do
  Spec.describe "install" do

    Spec.it "warns that the config was not changed when trying to install a package already present in project dependencies" \{ spago, fixture } -> do
      spago [ "init", "--name", "7368613235362d50744f44764f717435586c685938735a5154" ] >>= shouldBeSuccess
      spago [ "install" ] >>= shouldBeSuccess
      spago [ "fetch", "effect" ] >>= shouldBeSuccessErr (fixture "spago-install-existing-dep-stderr.txt")

    -- TODO: spago will currently crash if you try to have a purescript prefix. I guess we should fix this to at least not crash?
    -- Spec.it "Spago should strip 'purescript-' prefix and give warning if package without prefix is present in package set" \{ spago, fixture } -> do
    --   spago [ "init" ] >>= shouldBeSuccess
    --   spago [ "install", "safe-coerce" ] >>= shouldBeSuccess
    --   spago [ "install", "purescript-newtype" ] >>= shouldBeSuccessErr (fixture "spago-install-purescript-prefix-stderr.txt")
    --   -- dep added without "purescript-" prefix
    --   checkFixture "spago.yaml" (fixture "spago-strips-purescript.yaml")

    Spec.it "adds dependencies to the config file" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa", "--package-set", "29.3.0" ] >>= shouldBeSuccess
      spago [ "install", "foreign" ] >>= shouldBeSuccess
      checkFixture "spago.yaml" (fixture "spago-install-success.yaml")

    Spec.it "adds test dependencies to the config file" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa", "--package-set", "29.3.0" ] >>= shouldBeSuccess
      spago [ "install", "--test-deps", "foreign" ] >>= shouldBeSuccess
      checkFixture "spago.yaml" (fixture "spago-install-test-deps-success.yaml")

    Spec.it "adds test dependencies to the config file when the test section does not exist" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa", "--package-set", "29.3.0" ] >>= shouldBeSuccess
      let spagoYaml = "spago.yaml"
      FSA.unlink spagoYaml
      FS.copyFile
        { src: fixture "no-test-section.yaml"
        , dst: spagoYaml
        }
      spago [ "install", "--test-deps", "foreign" ] >>= shouldBeSuccess
      checkFixture spagoYaml (fixture "spago-install-test-deps-success.yaml")

    Spec.it "can't add dependencies that are not in the package set" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaaa", "--package-set", "29.3.0" ] >>= shouldBeSuccess
      spago [ "install", "foo", "bar" ] >>= shouldBeFailureErr (fixture "missing-dependencies.txt")
      checkFixture "spago.yaml" (fixture "spago-install-failure.yaml")

    Spec.it "does not allow circular dependencies" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let
        conf = Init.defaultConfig
          { name: mkPackageName "bbb"
          , withWorkspace: Just
              { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1"
              }
          , testModuleName: "Test.Main"
          }
      FS.writeYamlFile Config.configCodec "spago.yaml"
        ( conf
            { workspace = conf.workspace # map
                ( _
                    { extra_packages = Just $ Map.fromFoldable
                        [ Tuple (mkPackageName "a") $ Config.ExtraRemotePackage $ Config.RemoteGitPackage
                            { git: "https://github.com/purescript/spago.git"
                            , ref: "master"
                            , subdir: Nothing
                            , dependencies: Just $ mkDependencies [ "b" ]
                            }
                        , Tuple (mkPackageName "b") $ Config.ExtraRemotePackage $ Config.RemoteGitPackage
                            { git: "https://github.com/purescript/spago.git"
                            , ref: "master"
                            , subdir: Nothing
                            , dependencies: Just $ mkDependencies [ "a" ]
                            }
                        ]
                    }
                )
            }
        )
      spago [ "install", "a", "b" ] >>= shouldBeFailureErr (fixture "circular-dependencies.txt")

    Spec.it "installs a package in the set from a commit hash" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      writeConfigWithEither
      spago [ "install", "either" ] >>= shouldBeSuccess

    Spec.it "can't install (uncached) dependencies if offline" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      writeConfigWithEither
      spago [ "install", "--offline", "either" ] >>= shouldBeFailureErr (fixture "offline.txt")

    Spec.it "installs a package version by branch name with / in it" \{ spago, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let
        conf = Init.defaultConfig
          { name: mkPackageName "ddd"
          , withWorkspace: Just
              { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1"
              }
          , testModuleName: "Test.Main"
          }
      FS.writeYamlFile Config.configCodec "spago.yaml"
        ( conf
            { workspace = conf.workspace # map
                ( _
                    { extra_packages = Just $ Map.fromFoldable
                        [ Tuple (mkPackageName "nonexistent-package") $ Config.ExtraRemotePackage $ Config.RemoteGitPackage
                            { git: "https://github.com/spacchetti/purescript-metadata.git"
                            , ref: "spago-test/branch-with-slash"
                            , subdir: Nothing
                            , dependencies: Just $ mkDependencies [ "prelude" ]
                            }
                        ]
                    }
                )
            }
        )
      spago [ "install", "nonexistent-package" ] >>= shouldBeSuccess
      let slashyPath = Path.concat [ testCwd, ".spago", "packages", "nonexistent-package", "spago-test%2fbranch-with-slash" ]
      unlessM (FS.exists slashyPath) do
        Assertions.fail $ "Expected path to exist: " <> slashyPath
      kids <- FSA.readdir slashyPath
      when (Array.length kids == 0) do
        Assertions.fail $ "Expected path exists but contains nothing: " <> slashyPath

    Spec.it "installs a package not in the set from a commit hash" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let
        conf = Init.defaultConfig
          { name: mkPackageName "eee"
          , withWorkspace: Just
              { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1"
              }
          , testModuleName: "Test.Main"
          }
      FS.writeYamlFile Config.configCodec "spago.yaml"
        ( conf
            { workspace = conf.workspace # map
                ( _
                    { extra_packages = Just $ Map.fromFoldable
                        [ Tuple (mkPackageName "spago") $ Config.ExtraRemotePackage $ Config.RemoteGitPackage
                            { git: "https://github.com/purescript/spago.git"
                            , ref: "cbdbbf8f8771a7e43f04b18cdefffbcb0f03a990"
                            , subdir: Nothing
                            , dependencies: Just $ mkDependencies [ "prelude" ]
                            }
                        ]
                    }
                )
            }
        )
      spago [ "install", "spago" ] >>= shouldBeSuccess

    Spec.it "can't install a package from a not-existing commit hash" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let
        conf = Init.defaultConfig
          { name: mkPackageName "eee"
          , withWorkspace: Just
              { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1"
              }
          , testModuleName: "Test.Main"
          }
      FS.writeYamlFile Config.configCodec "spago.yaml"
        ( conf
            { workspace = conf.workspace # map
                ( _
                    { extra_packages = Just $ Map.fromFoldable
                        [ Tuple (mkPackageName "either") $ Config.ExtraRemotePackage $ Config.RemoteGitPackage
                            { git: "https://github.com/purescript/spago.git"
                            , ref: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                            , subdir: Nothing
                            , dependencies: Just $ mkDependencies [ "prelude" ]
                            }
                        ]
                    }
                )
            }
        )
      spago [ "install", "spago" ] >>= shouldBeFailure

    Spec.it "can update dependencies in a sub-package" \{ spago, fixture } -> do
      spago [ "init" ] >>= shouldBeSuccess
      FS.mkdirp "subpackage/src"
      FS.mkdirp "subpackage/test"
      FS.writeTextFile "subpackage/src/Main.purs" (Init.srcMainTemplate "Subpackage.Main")
      FS.writeTextFile "subpackage/test/Main.purs" (Init.testMainTemplate "Subpackage.Test.Main")
      FS.writeYamlFile Config.configCodec "subpackage/spago.yaml"
        ( Init.defaultConfig
            { name: mkPackageName "subpackage"
            , withWorkspace: Nothing
            , testModuleName: "Subpackage.Test.Main"
            }
        )
      spago [ "install", "-p", "subpackage", "either" ] >>= shouldBeSuccess
      checkFixture "subpackage/spago.yaml" (fixture "spago-subpackage-install-success.yaml")

    Spec.it "can build with a newer (but still compatible) compiler than the one in the package set" \{ spago } -> do
      spago [ "init", "--package-set", "10.0.0" ] >>= shouldBeSuccess
      startingTime <- liftEffect $ Now.now
      purs <- runSpago { logOptions: { color: false, verbosity: LogQuiet, startingTime } } Purs.getPurs
      -- The package set 10.0.0 has purescript 0.15.4, so we check that we have a newer version
      case purs.version > mkVersion "0.15.4" of
        true -> pure unit
        false -> Assert.fail $ "Expected purs version to be newer than 0.15.4, but it was " <> Version.print purs.version
      spago [ "install" ] >>= shouldBeSuccess

    Spec.it "can refresh the lockfile, and uninstall restores it" \{ spago, fixture } -> do
      spago [ "init", "--name", "aaa", "--package-set", "33.0.0" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      -- Check that we have written the lockfile
      checkFixture "spago.lock" (fixture "spago.lock")
      spago [ "install", "maybe" ] >>= shouldBeSuccess
      -- Check that the new lockfile includes maybe
      checkFixture "spago.lock" (fixture "spago-with-maybe.lock")
      spago [ "uninstall", "maybe" ] >>= shouldBeSuccess
      -- Check that the lockfile is back to the original
      checkFixture "spago.lock" (fixture "spago.lock")

writeConfigWithEither :: Aff Unit
writeConfigWithEither = do
  -- The commit for `either` is for the `v6.1.0` release
  let
    conf = Init.defaultConfig
      { name: mkPackageName "eee"
      , withWorkspace: Just
          { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1"
          }
      , testModuleName: "Test.Main"
      }
  FS.writeYamlFile Config.configCodec "spago.yaml"
    ( conf
        { workspace = conf.workspace # map
            ( _
                { extra_packages = Just $ Map.fromFoldable
                    [ Tuple (mkPackageName "either") $ Config.ExtraRemotePackage $ Config.RemoteGitPackage
                        { git: "https://github.com/purescript/purescript-either.git"
                        , ref: "af655a04ed2fd694b6688af39ee20d7907ad0763"
                        , subdir: Nothing
                        , dependencies: Just $ mkDependencies [ "control", "invariant", "maybe", "prelude" ]
                        }
                    ]
                }
            )
        }
    )
