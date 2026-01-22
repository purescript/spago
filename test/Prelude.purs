module Test.Prelude
  ( module Test.Prelude
  , module X
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Node.FS.Aff as FS.Aff
import Node.Library.Execa (ExecaResult)
import Node.Platform as Platform
import Node.Process as Process
import Record (merge)
import Registry.PackageName as PackageName
import Registry.Range as Range
import Registry.Version as Version
import Spago.Cmd (ExecResult, StdinConfig(..))
import Spago.Cmd (ExecResult, StdinConfig(..)) as X
import Spago.Cmd as Cmd
import Spago.Command.Init as Init
import Spago.Core.Config (Dependencies(..), Config)
import Spago.Core.Config as Config
import Spago.FS as FS
import Spago.Path (toRaw)
import Spago.Path as Path
import Spago.Paths as Paths
import Spago.Prelude as X
import Test.Spec.Assertions (fail)
import Test.Spec.Assertions as Assert

type FixturePath = GlobalPath

type TestDirs =
  { spago :: Array String -> Aff (Either ExecResult ExecResult)
  , spago' :: StdinConfig -> Array String -> Aff (Either ExecResult ExecResult)
  , fixture :: RawFilePath -> FixturePath
  , oldCwd :: GlobalPath
  , testCwd :: RootPath
  }

withTempDir :: (TestDirs -> Aff Unit) -> Aff Unit
withTempDir = Aff.bracket createTempDir cleanupTempDir
  where
  createTempDir = do
    oldCwd <- Paths.cwd
    temp' <- mkTemp' (Just "spago-test-")
    FS.mkdirp temp'
    Paths.chdir temp'
    temp <- Path.mkRoot =<< Paths.cwd
    isDebug <- liftEffect $ map isJust $ Process.lookupEnv "SPAGO_TEST_DEBUG"
    when isDebug do
      log $ "Running test in " <> Path.quote temp
    let
      fixturesPath = oldCwd </> "test-fixtures"

      fixture path = fixturesPath </> path

      spago' :: StdinConfig -> Array String -> Aff (Either ExecResult ExecResult)
      spago' stdin args =
        Cmd.exec
          (Path.global "node")
          ([ Path.toRaw $ oldCwd </> "bin" </> "index.dev.js" ] <> args)
          $ Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, pipeStdin = stdin }

      spago = spago' StdinNewPipe

    pure
      { spago'
      , spago
      , oldCwd
      , testCwd: temp
      , fixture
      }

  cleanupTempDir { oldCwd } = do
    Paths.chdir oldCwd

rmRf :: ∀ m path. MonadAff m => IsPath path => path -> m Unit
rmRf dir = liftAff $ FS.Aff.rm' (toRaw dir) { force: true, recursive: true, maxRetries: 5, retryDelay: 1000 }

shouldEqual
  :: forall m t
   . MonadThrow Error m
  => Show t
  => Eq t
  => t
  -> t
  -> m Unit
shouldEqual v1 v2 =
  when (v1 /= v2) do
    fail $ show v1 <> "\n\n  ≠\n\n  " <> show v2

shouldEqualStr
  :: forall m
   . MonadThrow Error m
  => String
  -> String
  -> m Unit
shouldEqualStr v1 v2 =
  let
    renderNonPrinting =
      String.replaceAll (String.Pattern "\r") (String.Replacement "␍")
      >>> String.replaceAll (String.Pattern "\t") (String.Replacement "␉-->")
  in
  when (v1 /= v2) do
    fail $ Array.intercalate "\n"
      [ ""
      , "===== (Actual)"
      , renderNonPrinting v1
      , "====="
      , "  ≠"
      , "===== (Expected)"
      , renderNonPrinting v2
      , "====="
      , ""
      ]

checkFixture :: ∀ path. IsPath path => path -> FixturePath -> Aff Unit
checkFixture filepath fixturePath = checkFixture' filepath fixturePath identity (shouldEqualStr `on` String.trim)

checkFixture' :: ∀ path. IsPath path => path -> FixturePath -> (String -> String) -> (String -> String -> Aff Unit) -> Aff Unit
checkFixture' filepath fixturePath normalizeForFixture assertEqual = do
  filecontent <- FS.readTextFile filepath
  overwriteSpecFile <- liftEffect $ map isJust $ Process.lookupEnv "SPAGO_TEST_ACCEPT"
  if overwriteSpecFile then do
    Console.log $ "Overwriting fixture at path: " <> Path.quote fixturePath
    let parentDir = Path.dirname fixturePath
    unlessM (FS.exists parentDir) $ FS.mkdirp parentDir
    FS.writeTextFile fixturePath (String.trim (normalizeForFixture filecontent) <> "\n")
  else do
    expected <- FS.readTextFile fixturePath
    filecontent `assertEqual` expected

plusDependencies :: Array String -> Config -> Config
plusDependencies deps config = config
  { package = config.package <#> \p -> p { dependencies = p.dependencies <> (Dependencies $ Map.fromFoldable $ map mkDep deps) }
  }
  where
  mkDep p = Tuple (unsafeFromRight $ PackageName.parse p) Nothing

check
  :: { stdout :: String -> Aff Unit
     , stderr :: String -> Aff Unit
     , result :: Either ExecResult ExecResult -> Boolean
     }
  -> Either ExecResult ExecResult
  -> Aff Unit
check checkers execResult = do
  let
    stdout = String.trim $ Cmd.getStdout execResult
    stderr = String.trim $ Cmd.getStderr execResult

  printStdoutStderr <- liftEffect $ map isJust $ Process.lookupEnv "SPAGO_TEST_DEBUG"

  when printStdoutStderr do
    log $ "STDOUT:\n" <> prettyPrint stdout
    log $ "STDERR:\n" <> prettyPrint stderr
  execResult `Assert.shouldSatisfy` checkers.result
  checkers.stdout stdout
  checkers.stderr stderr
  where
  prettyPrint =
    String.replaceAll (Pattern "\\n") (Replacement "\n")
      <<< String.replaceAll (Pattern "\\\"") (Replacement "\"")

checkOutputsStr
  :: { stdoutStr :: Maybe String
     , stderrStr :: Maybe String
     , result :: Either ExecResult ExecResult -> Boolean
     }
  -> Either ExecResult ExecResult
  -> Aff Unit
checkOutputsStr checkers =
  check
    { stdout: maybe mempty (\exp act -> act `shouldEqualStr` exp) checkers.stdoutStr
    , stderr: maybe mempty (\exp act -> act `shouldEqualStr` exp) checkers.stderrStr
    , result: checkers.result
    }

checkOutputs
  :: { stdoutFile :: Maybe FixturePath
     , stderrFile :: Maybe FixturePath
     , result :: (Either ExecResult ExecResult) -> Boolean
     }
  -> Either ExecResult ExecResult
  -> Aff Unit
checkOutputs args = checkOutputs' $ args `merge` { sanitize: String.trim }

checkOutputs'
  :: { stdoutFile :: Maybe FixturePath
     , stderrFile :: Maybe FixturePath
     , result :: (Either ExecResult ExecResult) -> Boolean
     , sanitize :: String -> String
     }
  -> Either ExecResult ExecResult
  -> Aff Unit
checkOutputs' checkers execResult = do
  let
    checkOrOverwrite = case _ of
      Nothing -> mempty
      Just fixtureFileExpected -> \actual' -> do
        let actual = checkers.sanitize actual'
        overwriteSpecFile <- liftEffect $ map isJust $ Process.lookupEnv "SPAGO_TEST_ACCEPT"
        if overwriteSpecFile then do
          Console.log $ "Overwriting fixture at path: " <> Path.quote fixtureFileExpected
          let parentDir = Path.dirname fixtureFileExpected
          unlessM (FS.exists parentDir) $ FS.mkdirp parentDir
          FS.writeTextFile fixtureFileExpected (actual <> "\n")
        else do
          expected <- checkers.sanitize <$> FS.readTextFile fixtureFileExpected
          actual `shouldEqualStr` expected
  check
    { stdout: checkOrOverwrite checkers.stdoutFile
    , stderr: checkOrOverwrite checkers.stderrFile
    , result: checkers.result
    }
    execResult

shouldBeSuccess :: Either ExecaResult ExecaResult -> Aff Unit
shouldBeSuccess = checkOutputs { stdoutFile: Nothing, stderrFile: Nothing, result: isRight }

shouldBeSuccessOutput :: FixturePath -> Either ExecaResult ExecaResult -> Aff Unit
shouldBeSuccessOutput outFixture = checkOutputs { stdoutFile: Just outFixture, stderrFile: Nothing, result: isRight }

shouldBeSuccessErr :: FixturePath -> Either ExecaResult ExecaResult -> Aff Unit
shouldBeSuccessErr errFixture = checkOutputs { stdoutFile: Nothing, stderrFile: Just errFixture, result: isRight }

shouldBeSuccessOutputWithErr :: FixturePath -> FixturePath -> Either ExecaResult ExecaResult -> Aff Unit
shouldBeSuccessOutputWithErr outFixture errFixture = checkOutputs { stdoutFile: Just outFixture, stderrFile: Just errFixture, result: isRight }

shouldBeFailure :: Either ExecaResult ExecaResult -> Aff Unit
shouldBeFailure = checkOutputs { stdoutFile: Nothing, stderrFile: Nothing, result: isLeft }

shouldBeFailureOutput :: FixturePath -> Either ExecaResult ExecaResult -> Aff Unit
shouldBeFailureOutput outFixture = checkOutputs { stdoutFile: Just outFixture, stderrFile: Nothing, result: isLeft }

shouldBeFailureErr :: FixturePath -> Either ExecaResult ExecaResult -> Aff Unit
shouldBeFailureErr errFixture = checkOutputs { stdoutFile: Nothing, stderrFile: Just errFixture, result: isLeft }

shouldBeFailureOutputWithErr :: FixturePath -> FixturePath -> Either ExecaResult ExecaResult -> Aff Unit
shouldBeFailureOutputWithErr outFixture errFixture = checkOutputs { stdoutFile: Just outFixture, stderrFile: Just errFixture, result: isLeft }

mkPackageName :: String -> PackageName
mkPackageName = unsafeFromRight <<< PackageName.parse

mkVersion :: String -> Version
mkVersion = unsafeFromRight <<< Version.parse

mkRange :: String -> Range
mkRange = unsafeFromRight <<< Range.parse

writeMain :: Array String -> String
writeMain rest = writePursFile { moduleName: "Main", rest }

writeTestMain :: Array String -> String
writeTestMain rest = writePursFile { moduleName: "Test.Main", rest }

writePursFile :: { moduleName :: String, rest :: Array String } -> String
writePursFile { moduleName, rest } =
  Array.intercalate "\n" $ Array.cons modNameLine $ rest
  where
  modNameLine = "module " <> moduleName <> " where"

editSpagoYaml :: (Config -> Config) -> Aff Unit
editSpagoYaml = editSpagoYaml' $ Path.global "spago.yaml"

editSpagoYaml' :: ∀ path. IsPath path => path -> (Config -> Config) -> Aff Unit
editSpagoYaml' configPath f = do
  content <- liftAff $ FS.readYamlDocFile Config.configCodec configPath
  case content of
    Left err ->
      Assert.fail $ "Failed to decode spago.yaml file at path " <> Path.quote configPath <> "\n" <> err
    Right { yaml: config } ->
      liftAff $ FS.writeYamlFile Config.configCodec configPath $ f config

mkDependencies :: Array String -> Config.Dependencies
mkDependencies = Config.Dependencies <<< Map.fromFoldable <<< map (flip Tuple Nothing <<< mkPackageName)

-- | packageToModuleName "package-name" = "PACKAGE.NAME"
packageToModuleName ∷ String → String
packageToModuleName packageName =
  String.toUpper (String.replaceAll (Pattern "-") (Replacement ".") packageName)

mkSrcModuleName ∷ String → String
mkSrcModuleName packageName = "Src." <> packageToModuleName packageName

mkTestModuleName ∷ String → String
mkTestModuleName packageName = "Test." <> packageToModuleName packageName

-- See `config*` functions for transforms below this binding
mkPackageOnlyConfig
  :: { packageName :: String, srcDependencies :: Array String }
  -> Array (Tuple String Init.DefaultConfigPackageOptions -> Tuple String Init.DefaultConfigPackageOptions)
  -> Config.Config
mkPackageOnlyConfig initialOptions transforms = do
  Init.defaultConfig' $ Init.PackageOnly $ configurePackageSection initialOptions transforms

mkPackageAndWorkspaceConfig
  :: { package :: { packageName :: String, srcDependencies :: Array String }
     , workspace :: { setVersion :: Maybe Version }
     }
  -> Array (Tuple String Init.DefaultConfigPackageOptions -> Tuple String Init.DefaultConfigPackageOptions)
  -> Config.Config
mkPackageAndWorkspaceConfig initOptions transforms = do
  Init.defaultConfig' $ Init.PackageAndWorkspace (configurePackageSection initOptions.package transforms) initOptions.workspace

configurePackageSection
  :: { packageName :: String, srcDependencies :: Array String }
  -> Array (Tuple String Init.DefaultConfigPackageOptions -> Tuple String Init.DefaultConfigPackageOptions)
  -> Init.DefaultConfigPackageOptions
configurePackageSection initialOptions = snd <<< Array.foldl (\c f -> f c)
  ( Tuple initialOptions.packageName $
      { name: mkPackageName initialOptions.packageName
      , dependencies: initialOptions.srcDependencies
      , build: Nothing
      , test: Nothing
      }
  )

configAddSrcStrict :: Tuple String Init.DefaultConfigPackageOptions -> Tuple String Init.DefaultConfigPackageOptions
configAddSrcStrict = map \r -> r
  { build = Just
      { strict: Just true
      , censorProjectWarnings: r.build >>= _.censorProjectWarnings
      , pedanticPackages: r.build >>= _.pedanticPackages
      }
  }

configAddSrcPedantic :: Tuple String Init.DefaultConfigPackageOptions -> Tuple String Init.DefaultConfigPackageOptions
configAddSrcPedantic = map \r -> r
  { build = Just
      { strict: r.build >>= _.strict
      , censorProjectWarnings: r.build >>= _.censorProjectWarnings
      , pedanticPackages: Just true
      }
  }

configAddSrcCensor :: Config.CensorBuildWarnings -> Tuple String Init.DefaultConfigPackageOptions -> Tuple String Init.DefaultConfigPackageOptions
configAddSrcCensor censors = map \r -> r
  { build = Just
      { strict: r.build >>= _.strict
      , censorProjectWarnings: Just censors
      , pedanticPackages: r.build >>= _.pedanticPackages
      }
  }

configAddTestMain :: Tuple String Init.DefaultConfigPackageOptions -> Tuple String Init.DefaultConfigPackageOptions
configAddTestMain (Tuple packageName r) = Tuple packageName $ r
  { test = Just
      { moduleMain: mkTestModuleName packageName
      , strict: r.test >>= _.strict
      , censorTestWarnings: r.test >>= _.censorTestWarnings
      , pedanticPackages: r.test >>= _.pedanticPackages
      , dependencies: r.test >>= _.dependencies
      }
  }

configAddTestStrict :: Tuple String Init.DefaultConfigPackageOptions -> Tuple String Init.DefaultConfigPackageOptions
configAddTestStrict (Tuple packageName r) = Tuple packageName $ r
  { test = Just
      { moduleMain: mkTestModuleName packageName
      , strict: Just true
      , censorTestWarnings: r.test >>= _.censorTestWarnings
      , pedanticPackages: r.test >>= _.pedanticPackages
      , dependencies: r.test >>= _.dependencies
      }
  }

configAddTestPedantic :: Tuple String Init.DefaultConfigPackageOptions -> Tuple String Init.DefaultConfigPackageOptions
configAddTestPedantic (Tuple packageName r) = Tuple packageName $ r
  { test = Just
      { moduleMain: mkTestModuleName packageName
      , strict: r.test >>= _.strict
      , censorTestWarnings: r.test >>= _.censorTestWarnings
      , pedanticPackages: Just true
      , dependencies: r.test >>= _.dependencies
      }
  }

configAddTestCensor :: Config.CensorBuildWarnings -> Tuple String Init.DefaultConfigPackageOptions -> Tuple String Init.DefaultConfigPackageOptions
configAddTestCensor censors (Tuple packageName r) = Tuple packageName $ r
  { test = Just
      { moduleMain: mkTestModuleName packageName
      , strict: r.test >>= _.strict
      , censorTestWarnings: Just censors
      , pedanticPackages: r.test >>= _.pedanticPackages
      , dependencies: r.test >>= _.dependencies
      }
  }

configAddTestDependencies :: Array String -> Tuple String Init.DefaultConfigPackageOptions -> Tuple String Init.DefaultConfigPackageOptions
configAddTestDependencies deps (Tuple packageName r) = Tuple packageName $ r
  { test = Just
      { moduleMain: mkTestModuleName packageName
      , strict: r.test >>= _.strict
      , censorTestWarnings: r.test >>= _.censorTestWarnings
      , pedanticPackages: r.test >>= _.pedanticPackages
      , dependencies: Just $ maybe (mkDependencies deps) (append (mkDependencies deps)) $ r.test >>= _.dependencies
      }
  }

escapePathInErrMsg :: Array String -> String
escapePathInErrMsg = case Process.platform of
  Just Platform.Win32 -> Array.intercalate "\\"
  _ -> Array.intercalate "/"

assertWarning :: forall m. MonadThrow Error m => Array String -> Boolean -> String -> m Unit
assertWarning paths shouldHave stdErr = do
  when (not $ Array.all (\exp -> shouldHave == (String.contains (Pattern exp) stdErr)) paths) do
    Assert.fail
      $ "STDERR "
      <> (if shouldHave then "did not contain" else "contained")
      <> " one or more texts:\n"
      <> show paths
      <> "\n\nStderr was:\n"
      <> stdErr

-- | Run a git command in the current directory
git :: Array String -> Aff Unit
git = git' Nothing

-- | Run a git command in a specific directory
git' :: Maybe GlobalPath -> Array String -> Aff Unit
git' cwd args =
  Cmd.exec (Path.global "git") args
    (Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false, pipeStdin = StdinNewPipe, cwd = cwd })
    >>= shouldBeSuccess

-- | Create a git repo with a spago.yaml in the test's temp directory.
-- | Cleanup is automatic when the test's temp directory is removed.
mkGitRepo
  :: RootPath
  -> { name :: String, deps :: Array String }
  -> Aff LocalPath
mkGitRepo testCwd { name, deps } = do
  let repo = testCwd </> ("lib-" <> name)
  FS.mkdirp repo
  let depsYaml = if Array.null deps then "[]" else "\n" <> foldMap (\d -> "    - " <> d <> "\n") deps
  FS.writeTextFile (repo </> "spago.yaml") $
    "package:\n  name: " <> name <> "\n  dependencies: " <> depsYaml <> "\nworkspace:\n  packageSet:\n    registry: 0.0.1\n"
  FS.mkdirp (repo </> "src")
  FS.writeTextFile (repo </> "src/Main.purs") $ "module " <> String.toUpper name <> ".Main where\n"
  git' (Just $ Path.toGlobal repo) [ "init", "-b", "main" ]
  git' (Just $ Path.toGlobal repo) [ "config", "user.name", "test" ]
  git' (Just $ Path.toGlobal repo) [ "config", "user.email", "test@test.com" ]
  git' (Just $ Path.toGlobal repo) [ "add", "." ]
  git' (Just $ Path.toGlobal repo) [ "commit", "-m", "initial" ]
  pure repo

