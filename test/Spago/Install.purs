module Test.Spago.Install
  ( spec
  , forceResetSpec
  ) where

import Test.Prelude

import Data.Array as Array
import Data.Map as Map
import Effect.Now as Now
import Registry.Version as Version
import Spago.Command.Init as Init
import Spago.Cmd as Cmd
import Spago.Core.Config (Dependencies(..), Config)
import Spago.Core.Config as Config
import Spago.FS as FS
import Spago.Git as Git
import Spago.Log (LogVerbosity(..))
import Spago.Path as Path
import Spago.Paths as Paths
import Spago.Purs as Purs
import Test.Spec (Spec)
import Test.Spec as Spec
import Test.Spec.Assertions as Assert
import Test.Spec.Assertions as Assertions
import Test.Spec.Assertions.String (shouldContain)

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

    Spec.it "adds dependencies to the config file" \{ spago, fixture, testCwd } -> do
      spago [ "init", "--name", "aaa", "--package-set", "29.3.0" ] >>= shouldBeSuccess
      spago [ "install", "foreign" ] >>= shouldBeSuccess
      checkFixture (testCwd </> "spago.yaml") (fixture "spago-install-success.yaml")

    Spec.it "adds test dependencies to the config file" \{ spago, fixture, testCwd } -> do
      spago [ "init", "--name", "aaa", "--package-set", "29.3.0" ] >>= shouldBeSuccess
      spago [ "install", "--test-deps", "foreign" ] >>= shouldBeSuccess
      checkFixture (testCwd </> "spago.yaml") (fixture "spago-install-test-deps-success.yaml")

    Spec.it "adds test dependencies to the config file when the test section does not exist" \{ spago, fixture, testCwd } -> do
      spago [ "init", "--name", "aaa", "--package-set", "29.3.0" ] >>= shouldBeSuccess
      let spagoYaml = testCwd </> "spago.yaml"
      FS.unlink spagoYaml
      FS.copyFile
        { src: fixture "no-test-section.yaml"
        , dst: spagoYaml
        }
      spago [ "install", "--test-deps", "foreign" ] >>= shouldBeSuccess
      checkFixture spagoYaml (fixture "spago-install-test-deps-success.yaml")

    Spec.it "can't add dependencies that are not in the package set" \{ spago, fixture, testCwd } -> do
      spago [ "init", "--name", "aaaa", "--package-set", "29.3.0" ] >>= shouldBeSuccess
      spago [ "install", "foo-foo-foo", "bar-bar-bar", "effcet", "arrys" ] >>= shouldBeFailureErr (fixture "missing-dependencies.txt")
      checkFixture (testCwd </> "spago.yaml") (fixture "spago-install-failure.yaml")

    Spec.it "warns when specified dependency versions do not exist" \{ spago, fixture, testCwd } -> do
      spago [ "init", "--package-set", "29.3.0" ] >>= shouldBeSuccess

      FS.writeYamlFile Config.configCodec (testCwd </> "spago.yaml")
        $ insertConfigDependencies
            ( Init.defaultConfig
                { name: mkPackageName "aaa"
                , withWorkspace: Just { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1" }
                , testModuleName: "Test.Main"
                }
            )
            ( Dependencies $ Map.fromFoldable
                [ Tuple (mkPackageName "prelude") (Just $ mkRange ">=6.0.0 <7.0.0")
                , Tuple (mkPackageName "lists") (Just $ mkRange ">=1000.0.0 <1000.0.1")
                ]
            )
            ( Dependencies $ Map.fromFoldable
                [ Tuple (mkPackageName "spec") (Just $ mkRange ">=7.0.0 <8.0.0")
                , Tuple (mkPackageName "maybe") (Just $ mkRange ">=1000.0.0 <1000.0.1")
                ]
            )

      warning <- FS.readTextFileSync $ fixture "missing-versions.txt"
      outputs <- spago [ "install" ]
      either _.stderr _.stderr outputs `shouldContain` warning

    Spec.it "does not allow circular dependencies" \{ spago, fixture, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let
        conf = Init.defaultConfig
          { name: mkPackageName "bbb"
          , withWorkspace: Just
              { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1"
              }
          , testModuleName: "Test.Main"
          }
      FS.writeYamlFile Config.configCodec (testCwd </> "spago.yaml")
        ( conf
            { workspace = conf.workspace # map
                ( _
                    { extraPackages = Just $ Map.fromFoldable
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

    Spec.it "installs a package in the set from a commit hash" \{ spago, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      writeConfigWithEither testCwd
      spago [ "install", "either" ] >>= shouldBeSuccess

    Spec.it "can't install (uncached) dependencies if offline" \{ spago, fixture, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      writeConfigWithEither testCwd
      spago [ "install", "--offline", "either" ] >>= shouldBeFailureErr (fixture "offline.txt")

    Spec.it "forces registry refresh when using --refresh flag" \{ spago } -> do
      spago [ "init" ] >>= shouldBeSuccess
      -- The --refresh flag should force a registry refresh regardless of cache age
      result <- spago [ "install", "--refresh" ]
      shouldBeSuccess result
      either _.stderr _.stderr result `shouldContain` "Refreshing the Registry Index..."

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
      FS.writeYamlFile Config.configCodec (testCwd </> "spago.yaml")
        ( conf
            { workspace = conf.workspace # map
                ( _
                    { extraPackages = Just $ Map.fromFoldable
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
      let slashyPath = testCwd </> Paths.localCachePackagesPath </> "nonexistent-package" </> "spago-test%2fbranch-with-slash"
      unlessM (FS.exists slashyPath) do
        Assertions.fail $ "Expected path to exist: " <> Path.quote slashyPath
      kids <- FS.ls slashyPath
      when (Array.length kids == 0) do
        Assertions.fail $ "Expected path exists but contains nothing: " <> Path.quote slashyPath

    Spec.it "installs a package not in the set from a commit hash" \{ spago, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let
        conf = Init.defaultConfig
          { name: mkPackageName "eee"
          , withWorkspace: Just
              { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1"
              }
          , testModuleName: "Test.Main"
          }
      FS.writeYamlFile Config.configCodec (testCwd </> "spago.yaml")
        ( conf
            { workspace = conf.workspace # map
                ( _
                    { extraPackages = Just $ Map.fromFoldable
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

    Spec.it "can't install a package from a not-existing commit hash" \{ spago, testCwd } -> do
      spago [ "init" ] >>= shouldBeSuccess
      let
        conf = Init.defaultConfig
          { name: mkPackageName "eee"
          , withWorkspace: Just
              { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1"
              }
          , testModuleName: "Test.Main"
          }
      FS.writeYamlFile Config.configCodec (testCwd </> "spago.yaml")
        ( conf
            { workspace = conf.workspace # map
                ( _
                    { extraPackages = Just $ Map.fromFoldable
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

    Spec.it "can update dependencies in a sub-package" \{ spago, fixture, testCwd } -> do
      let subpackage = testCwd </> "subpackage"
      spago [ "init" ] >>= shouldBeSuccess
      FS.mkdirp (subpackage </> "src")
      FS.mkdirp (subpackage </> "test")
      FS.writeTextFile (subpackage </> "src" </> "Main.purs") (Init.srcMainTemplate "Subpackage.Main")
      FS.writeTextFile (subpackage </> "test" </> "Main.purs") (Init.testMainTemplate "Subpackage.Test.Main")
      FS.writeYamlFile Config.configCodec (subpackage </> "spago.yaml")
        ( Init.defaultConfig
            { name: mkPackageName "subpackage"
            , withWorkspace: Nothing
            , testModuleName: "Subpackage.Test.Main"
            }
        )
      spago [ "install", "-p", "subpackage", "either" ] >>= shouldBeSuccess
      checkFixture (subpackage </> "spago.yaml") (fixture "spago-subpackage-install-success.yaml")

    Spec.it "can build with a newer (but still compatible) compiler than the one in the package set" \{ spago } -> do
      spago [ "init", "--package-set", "10.0.0" ] >>= shouldBeSuccess
      startingTime <- liftEffect $ Now.now
      purs <- runSpago { logOptions: { color: false, verbosity: LogQuiet, startingTime } } Purs.getPurs
      -- The package set 10.0.0 has purescript 0.15.4, so we check that we have a newer version
      case purs.version > mkVersion "0.15.4" of
        true -> pure unit
        false -> Assert.fail $ "Expected purs version to be newer than 0.15.4, but it was " <> Version.print purs.version
      spago [ "install" ] >>= shouldBeSuccess

    Spec.it "can refresh the lockfile, and uninstall restores it" \{ spago, fixture, testCwd } -> do
      spago [ "init", "--name", "aaa", "--package-set", "33.0.0" ] >>= shouldBeSuccess
      spago [ "build" ] >>= shouldBeSuccess
      -- Check that we have written the lockfile
      checkFixture (testCwd </> "spago.lock") (fixture "spago.lock")
      spago [ "install", "maybe" ] >>= shouldBeSuccess
      -- Check that the new lockfile includes maybe
      checkFixture (testCwd </> "spago.lock") (fixture "spago-with-maybe.lock")
      spago [ "uninstall", "maybe" ] >>= shouldBeSuccess
      -- Check that the lockfile is back to the original
      checkFixture (testCwd </> "spago.lock") (fixture "spago.lock")

    Spec.it "adds version ranges automatically for solver-based projects" \{ spago, fixture, testCwd } -> do
      spago [ "init", "--name", "aaa", "--use-solver" ] >>= shouldBeSuccess
      spago [ "install", "either" ] >>= shouldBeSuccess
      checkFixture (testCwd </> "spago.yaml") (fixture "spago-install-solver-ranges.yaml")


insertConfigDependencies :: Config -> Dependencies -> Dependencies -> Config
insertConfigDependencies config core test =
  ( config
      { package = config.package # map
          ( \package' -> package'
              { dependencies = core
              , test = package'.test # map ((_ { dependencies = test }))
              }
          )
      }
  )

writeConfigWithEither :: RootPath -> Aff Unit
writeConfigWithEither root = do
  -- The commit for `either` is for the `v6.1.0` release
  let
    conf = Init.defaultConfig
      { name: mkPackageName "eee"
      , withWorkspace: Just
          { setVersion: Just $ unsafeFromRight $ Version.parse "0.0.1"
          }
      , testModuleName: "Test.Main"
      }
  FS.writeYamlFile Config.configCodec (root </> "spago.yaml")
    ( conf
        { workspace = conf.workspace # map
            ( _
                { extraPackages = Just $ Map.fromFoldable
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

-- | Test that fetchRepo handles git history rewrites (squash) gracefully
-- This verifies that git pull --rebase works even when the remote history is squashed,
-- as long as the content is the same. This is important for the registry repo.
forceResetSpec :: Spec Unit
forceResetSpec = Spec.around withTempDir do
  Spec.describe "git fetchRepo" do
    Spec.it "handles history rewrite (squash) gracefully" \{ testCwd } -> do
      -- Setup logging
      startingTime <- liftEffect $ Now.now
      let logOptions = { color: false, verbosity: LogQuiet, startingTime }

      -- Get git command
      gitCmd <- runSpago { logOptions } Git.getGit

      -- Build GitEnv
      let gitEnv = { git: gitCmd, logOptions, offline: Online }

      -- 1. Create a bare "origin" repo
      let originRepo = testCwd </> "origin"
      FS.mkdirp originRepo
      git' (Just $ Path.toGlobal originRepo) [ "init", "--bare" ]

      -- 2. Clone, make commits, push
      let workRepo = testCwd </> "work"
      let gitInWorkRepo = git' (Just $ Path.toGlobal workRepo)
      git [ "clone", Path.toRaw originRepo, Path.toRaw workRepo ]
      gitInWorkRepo [ "config", "user.name", "test" ]
      gitInWorkRepo [ "config", "user.email", "test@test.com" ]
      gitInWorkRepo [ "checkout", "-b", "main" ]
      FS.writeTextFile (workRepo </> "file1.txt") "content1"
      gitInWorkRepo [ "add", "." ]
      gitInWorkRepo [ "commit", "-m", "first" ]
      FS.writeTextFile (workRepo </> "file2.txt") "content2"
      gitInWorkRepo [ "add", "." ]
      gitInWorkRepo [ "commit", "-m", "second" ]
      gitInWorkRepo [ "push", "-u", "origin", "main" ]

      -- 3. Clone again (simulating spago's cached registry)
      let cachedRepo = testCwd </> "cached"
      git [ "clone", Path.toRaw originRepo, Path.toRaw cachedRepo ]

      -- 4. Add another commit to origin that modifies an existing file
      -- Now cached is behind and has different content
      FS.writeTextFile (workRepo </> "file2.txt") "content2-updated"
      gitInWorkRepo [ "add", "." ]
      gitInWorkRepo [ "commit", "-m", "third" ]
      gitInWorkRepo [ "push", "origin", "main" ]

      -- 5. Squash all history and force push (simulating registry squash)
      -- Now origin has: first -> second -> third (with updated file2), squashed to single commit
      -- But cached only has: first -> second (with original file2)
      gitInWorkRepo [ "reset", "--soft", "HEAD~2" ]
      gitInWorkRepo [ "commit", "--amend", "-m", "squashed all history" ]
      gitInWorkRepo [ "push", "--force", "origin", "main" ]

      -- 6. fetchRepo should succeed despite the history rewrite and different content
      -- because git pull --rebase can handle rebasing onto a squashed history
      result <- runSpago gitEnv $ Git.fetchRepo
        { git: Path.toRaw originRepo, ref: "main" }
        cachedRepo
      case result of
        Left err -> Assert.fail $ "Expected fetchRepo to succeed after history squash, but got: " <> show err
        Right _ -> pure unit

      -- 7. Verify the content is updated correctly
      content1 <- FS.readTextFile (cachedRepo </> "file1.txt")
      content1 `shouldEqualStr` "content1"
      content2 <- FS.readTextFile (cachedRepo </> "file2.txt")
      content2 `shouldEqualStr` "content2-updated"

git :: Array String -> Aff Unit
git = git' Nothing

git' :: Maybe GlobalPath -> Array String -> Aff Unit
git' cwd args =
  Cmd.exec (Path.global "git") args
    (Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, pipeStdin = StdinNewPipe, cwd = cwd })
    >>= shouldBeSuccess
