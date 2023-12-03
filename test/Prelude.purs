module Test.Prelude
  ( module X
  , module Test.Prelude
  ) where

import Spago.Prelude

import Data.Array as Array
import Data.Map as Map
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Effect.Class.Console as Console
import Node.Path as Path
import Node.Process as Process
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Cmd (ExecResult, StdinConfig(..)) as X
import Spago.Cmd (ExecResult, StdinConfig(..))
import Spago.Cmd as Cmd
import Spago.Command.Init as Init
import Spago.Core.Config (Dependencies(..), Config)
import Spago.Core.Config as Config
import Spago.FS as FS
import Spago.Prelude as X
import Test.Spec.Assertions (fail)
import Test.Spec.Assertions as Assert

type TestDirs =
  { spago :: Array String -> Aff ExecResult
  , spago' :: StdinConfig -> Array String -> Aff ExecResult
  , fixture :: FilePath -> FilePath
  , oldCwd :: FilePath
  , testCwd :: FilePath
  }

withTempDir :: (TestDirs -> Aff Unit) -> Aff Unit
withTempDir = Aff.bracket createTempDir cleanupTempDir
  where
  createTempDir = do
    oldCwd <- liftEffect $ Process.cwd
    temp <- mkTemp' $ Just "spago-test-"
    FS.mkdirp temp
    liftEffect $ Process.chdir temp
    isDebug <- liftEffect $ map isJust $ Process.lookupEnv "SPAGO_TEST_DEBUG"
    when isDebug do
      log $ "Running test in " <> temp
    let
      fixturesPath = oldCwd <> Path.sep <> "test-fixtures"

      fixture path = Path.concat [ fixturesPath, path ]

      spago' :: StdinConfig -> Array String -> Aff ExecResult
      spago' stdin args =
        Cmd.exec
          "node"
          ([ Path.concat [ oldCwd, "bin", "index.dev.js" ] ] <> args)
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
    liftEffect $ Process.chdir oldCwd

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
  when (v1 /= v2) do
    fail $ "\n=====\n" <> v1 <> "\n=====\n  ≠\n=====\n  " <> show v2 <> "\n=====\n"

checkFixture :: String -> String -> Aff Unit
checkFixture filepath fixturePath = do
  filecontent <- FS.readTextFile filepath
  fixturecontent <- FS.readTextFile fixturePath
  filecontent `shouldEqualStr` fixturecontent

plusDependencies :: Array String -> Config -> Config
plusDependencies deps config = config
  { package = config.package <#> \p -> p { dependencies = p.dependencies <> (Dependencies $ Map.fromFoldable $ map mkDep deps) }
  }
  where
  mkDep p = Tuple (unsafeFromRight $ PackageName.parse p) Nothing

check
  :: { stdout :: String -> Aff Unit
     , stderr :: String -> Aff Unit
     , result :: ExecResult -> Boolean
     }
  -> ExecResult
  -> Aff Unit
check checkers execResult = do
  let
    stdout = String.trim $ execResult.stdout
    stderr = String.trim $ execResult.stderr

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
     , result :: ExecResult -> Boolean
     }
  -> ExecResult
  -> Aff Unit
checkOutputsStr checkers =
  check
    { stdout: maybe mempty (\exp act -> act `Assert.shouldEqual` exp) checkers.stdoutStr
    , stderr: maybe mempty (\exp act -> act `Assert.shouldEqual` exp) checkers.stderrStr
    , result: checkers.result
    }

checkOutputs
  :: { stdoutFile :: Maybe FilePath
     , stderrFile :: Maybe FilePath
     , result :: ExecResult -> Boolean
     }
  -> ExecResult
  -> Aff Unit
checkOutputs checkers execResult = do
  let
    checkOrOverwrite = case _ of
      Nothing -> mempty
      Just fixtureFileExpected -> \actual -> do
        overwriteSpecFile <- liftEffect $ map isJust $ Process.lookupEnv "SPAGO_TEST_ACCEPT"
        if overwriteSpecFile then do
          Console.log $ "Overwriting fixture at path: " <> fixtureFileExpected
          FS.writeTextFile fixtureFileExpected actual
        else do
          expected <- String.trim <$> FS.readTextFile fixtureFileExpected
          actual `Assert.shouldEqual` expected
  check
    { stdout: checkOrOverwrite checkers.stdoutFile
    , stderr: checkOrOverwrite checkers.stderrFile
    , result: checkers.result
    }
    execResult

shouldBeSuccess :: ExecResult -> Aff Unit
shouldBeSuccess = checkOutputs { stdoutFile: Nothing, stderrFile: Nothing, result: Cmd.isSuccess }

shouldBeSuccessOutput :: FilePath -> ExecResult -> Aff Unit
shouldBeSuccessOutput outFixture = checkOutputs { stdoutFile: Just outFixture, stderrFile: Nothing, result: Cmd.isSuccess }

shouldBeSuccessErr :: FilePath -> ExecResult -> Aff Unit
shouldBeSuccessErr errFixture = checkOutputs { stdoutFile: Nothing, stderrFile: Just errFixture, result: Cmd.isSuccess }

shouldBeSuccessOutputWithErr :: FilePath -> FilePath -> ExecResult -> Aff Unit
shouldBeSuccessOutputWithErr outFixture errFixture = checkOutputs { stdoutFile: Just outFixture, stderrFile: Just errFixture, result: Cmd.isSuccess }

shouldBeFailure :: ExecResult -> Aff Unit
shouldBeFailure = checkOutputs { stdoutFile: Nothing, stderrFile: Nothing, result: Cmd.isFailure }

shouldBeFailureOutput :: FilePath -> ExecResult -> Aff Unit
shouldBeFailureOutput outFixture = checkOutputs { stdoutFile: Just outFixture, stderrFile: Nothing, result: Cmd.isFailure }

shouldBeFailureErr :: FilePath -> ExecResult -> Aff Unit
shouldBeFailureErr errFixture = checkOutputs { stdoutFile: Nothing, stderrFile: Just errFixture, result: Cmd.isFailure }

shouldBeFailureOutputWithErr :: FilePath -> FilePath -> ExecResult -> Aff Unit
shouldBeFailureOutputWithErr outFixture errFixture = checkOutputs { stdoutFile: Just outFixture, stderrFile: Just errFixture, result: Cmd.isFailure }

mkPackageName :: String -> PackageName
mkPackageName = unsafeFromRight <<< PackageName.parse

mkVersion :: String -> Version
mkVersion = unsafeFromRight <<< Version.parse

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
editSpagoYaml = editSpagoYaml' "spago.yaml"

editSpagoYaml' :: FilePath -> (Config -> Config) -> Aff Unit
editSpagoYaml' configPath f = do
  content <- liftAff $ FS.readYamlDocFile Config.configCodec configPath
  case content of
    Left err ->
      Assert.fail $ "Failed to decode spago.yaml file at path " <> configPath <> "\n" <> err
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
