module Test.Prelude
  ( module X
  , module Test.Prelude
  ) where

import Spago.Prelude

import Data.Map as Map
import Data.String as String
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Node.Path as Path
import Node.Process as Process
import Registry.PackageName as PackageName
import Spago.Cmd (ExecError, ExecResult) as X
import Spago.Cmd (ExecError, ExecResult, StdinConfig(..))
import Spago.Cmd as Cmd
import Spago.Core.Config (Dependencies(..), Config)
import Spago.FS as FS
import Spago.Prelude as X
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
      fixturesPath = oldCwd <> "/test-fixtures"

      fixture path = Path.concat [ fixturesPath, path ]

      spago' :: StdinConfig -> Array String -> Aff (Either ExecError ExecResult)
      spago' stdin args =
        Cmd.exec
          (Path.concat [ oldCwd, "bin", "index.dev.js" ])
          args
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

checkFixture :: String -> String -> Aff Unit
checkFixture filepath fixturePath = do
  filecontent <- FS.readTextFile filepath
  fixturecontent <- FS.readTextFile fixturePath
  filecontent `Assert.shouldEqual` fixturecontent

plusDependencies :: Array String -> Config -> Config
plusDependencies deps config = config
  { package = config.package <#> \p -> p { dependencies = p.dependencies <> (Dependencies $ Map.fromFoldable $ map mkDep deps) }
  }
  where
  mkDep p = Tuple (unsafeFromRight $ PackageName.parse p) Nothing

checkResultAndOutputsStr :: Maybe String -> Maybe String -> (Either ExecError ExecResult -> Boolean) -> Either ExecError ExecResult -> Aff _
checkResultAndOutputsStr maybeOutStr maybeErrStr resultFn execResult = do
  execResult `Assert.shouldSatisfy` resultFn
  let
    stdout = String.trim $ case execResult of
      Left err -> err.stdout
      Right res -> res.stdout
    stderr = String.trim $ case execResult of
      Left err -> err.stderr
      Right res -> res.stderr
  for_ maybeOutStr \expectedOut -> do
    stdout `Assert.shouldEqual` expectedOut
  for_ maybeErrStr \expectedErr -> do
    stderr `Assert.shouldEqual` expectedErr

checkResultAndOutputs :: Maybe FilePath -> Maybe FilePath -> (Either ExecError ExecResult -> Boolean) -> Either ExecError ExecResult -> Aff _
checkResultAndOutputs maybeOutFixture maybeErrFixture resultFn execResult = do
  maybeOutStr <- for maybeOutFixture \expectedOutFixture -> do
    String.trim <$> FS.readTextFile expectedOutFixture
  maybeErrStr <- for maybeErrFixture \expectedErrFixture -> do
    String.trim <$> FS.readTextFile expectedErrFixture
  checkResultAndOutputsStr maybeOutStr maybeErrStr resultFn execResult

shouldBeSuccess :: Either ExecError ExecResult -> Aff Unit
shouldBeSuccess = checkResultAndOutputs Nothing Nothing isRight

shouldBeSuccessOutput :: FilePath -> Either ExecError ExecResult -> Aff Unit
shouldBeSuccessOutput outFixture = checkResultAndOutputs (Just outFixture) Nothing isRight

shouldBeSuccessErr :: FilePath -> Either ExecError ExecResult -> Aff Unit
shouldBeSuccessErr errFixture = checkResultAndOutputs Nothing (Just errFixture) isRight

shouldBeSuccessOutputWithErr :: FilePath -> FilePath -> Either ExecError ExecResult -> Aff Unit
shouldBeSuccessOutputWithErr outFixture errFixture = checkResultAndOutputs (Just outFixture) (Just errFixture) isRight

shouldBeFailure :: Either ExecError ExecResult -> Aff Unit
shouldBeFailure = checkResultAndOutputs Nothing Nothing isLeft

shouldBeFailureOutput :: FilePath -> Either ExecError ExecResult -> Aff Unit
shouldBeFailureOutput outFixture = checkResultAndOutputs (Just outFixture) Nothing isLeft

shouldBeFailureErr :: FilePath -> Either ExecError ExecResult -> Aff Unit
shouldBeFailureErr errFixture = checkResultAndOutputs Nothing (Just errFixture) isLeft

shouldBeFailureOutputWithErr :: FilePath -> FilePath -> Either ExecError ExecResult -> Aff Unit
shouldBeFailureOutputWithErr outFixture errFixture = checkResultAndOutputs (Just outFixture) (Just errFixture) isLeft

mkPackageName :: String -> PackageName
mkPackageName = unsafeFromRight <<< PackageName.parse
