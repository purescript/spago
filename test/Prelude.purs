module Test.Prelude
  ( module X
  , module Test.Prelude
  ) where

import Spago.Prelude

import Data.Map as Map
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Node.Path as Path
import Node.Process as Process
import Registry.PackageName as PackageName
import Registry.Version as Version
import Spago.Cmd (ExecError, ExecResult) as X
import Spago.Cmd (ExecError, ExecResult, StdinConfig(..))
import Spago.Cmd as Cmd
import Spago.Core.Config (Dependencies(..), Config)
import Spago.FS as FS
import Spago.Prelude as X
import Test.Spec.Assertions (fail)
import Test.Spec.Assertions as Assert

type TestDirs =
  { spago :: Array String -> Aff (Either ExecError ExecResult)
  , spago' :: StdinConfig -> Array String -> Aff (Either ExecError ExecResult)
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
    log $ "Running test in " <> temp
    let
      fixturesPath = oldCwd <> Path.sep <> "test-fixtures"

      fixture path = Path.concat [ fixturesPath, path ]

      spago' :: StdinConfig -> Array String -> Aff (Either ExecError ExecResult)
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
    fail $ show v1 <> "\n\n≠\n\n" <> show v2

checkFixture :: String -> String -> Aff Unit
checkFixture filepath fixturePath = do
  filecontent <- FS.readTextFile filepath
  fixturecontent <- FS.readTextFile fixturePath
  filecontent `shouldEqual` fixturecontent

plusDependencies :: Array String -> Config -> Config
plusDependencies deps config = config
  { package = config.package <#> \p -> p { dependencies = p.dependencies <> (Dependencies $ Map.fromFoldable $ map mkDep deps) }
  }
  where
  mkDep p = Tuple (unsafeFromRight $ PackageName.parse p) Nothing

check
  :: { stdout :: String -> Aff Unit
     , stderr :: String -> Aff Unit
     , result :: Either ExecError ExecResult -> Boolean
     }
  -> Either ExecError ExecResult
  -> Aff Unit
check checkers execResult = do
  let
    stdout = String.trim $ case execResult of
      Left err -> err.stdout
      Right res -> res.stdout
    stderr = String.trim $ case execResult of
      Left err -> err.stderr
      Right res -> res.stderr

  when true do
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
     , result :: Either ExecError ExecResult -> Boolean
     }
  -> Either ExecError ExecResult
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
     , result :: Either ExecError ExecResult -> Boolean
     }
  -> Either ExecError ExecResult
  -> Aff Unit
checkOutputs checkers execResult = do
  maybeOutStr <- for checkers.stdoutFile \expectedOutFixture -> do
    String.trim <$> FS.readTextFile expectedOutFixture
  maybeErrStr <- for checkers.stderrFile \expectedErrFixture -> do
    String.trim <$> FS.readTextFile expectedErrFixture
  checkOutputsStr
    { stdoutStr: maybeOutStr
    , stderrStr: maybeErrStr
    , result: checkers.result
    }
    execResult

shouldBeSuccess :: Either ExecError ExecResult -> Aff Unit
shouldBeSuccess = checkOutputs { stdoutFile: Nothing, stderrFile: Nothing, result: isRight }

shouldBeSuccessOutput :: FilePath -> Either ExecError ExecResult -> Aff Unit
shouldBeSuccessOutput outFixture = checkOutputs { stdoutFile: Just outFixture, stderrFile: Nothing, result: isRight }

shouldBeSuccessErr :: FilePath -> Either ExecError ExecResult -> Aff Unit
shouldBeSuccessErr errFixture = checkOutputs { stdoutFile: Nothing, stderrFile: Just errFixture, result: isRight }

shouldBeSuccessOutputWithErr :: FilePath -> FilePath -> Either ExecError ExecResult -> Aff Unit
shouldBeSuccessOutputWithErr outFixture errFixture = checkOutputs { stdoutFile: Just outFixture, stderrFile: Just errFixture, result: isRight }

shouldBeFailure :: Either ExecError ExecResult -> Aff Unit
shouldBeFailure = checkOutputs { stdoutFile: Nothing, stderrFile: Nothing, result: isLeft }

shouldBeFailureOutput :: FilePath -> Either ExecError ExecResult -> Aff Unit
shouldBeFailureOutput outFixture = checkOutputs { stdoutFile: Just outFixture, stderrFile: Nothing, result: isLeft }

shouldBeFailureErr :: FilePath -> Either ExecError ExecResult -> Aff Unit
shouldBeFailureErr errFixture = checkOutputs { stdoutFile: Nothing, stderrFile: Just errFixture, result: isLeft }

shouldBeFailureOutputWithErr :: FilePath -> FilePath -> Either ExecError ExecResult -> Aff Unit
shouldBeFailureOutputWithErr outFixture errFixture = checkOutputs { stdoutFile: Just outFixture, stderrFile: Just errFixture, result: isLeft }

mkPackageName :: String -> PackageName
mkPackageName = unsafeFromRight <<< PackageName.parse

mkVersion :: String -> Version
mkVersion = unsafeFromRight <<< Version.parse
