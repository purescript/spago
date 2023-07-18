module Test.Prelude
  ( module X
  , module Test.Prelude
  ) where

import Spago.Prelude

import Data.String as String
import Effect.Aff as Aff
import Node.Path as Path
import Node.Process as Process
import Spago.Cmd (ExecError, ExecResult)
import Spago.Cmd (ExecError, ExecResult) as X
import Spago.Cmd as Cmd
import Spago.FS as FS
import Spago.Prelude as Either
import Spago.Prelude as X
import Test.Spec.Assertions as Assert

type TestDirs =
  { spago :: Array String -> Aff (Either ExecError ExecResult)
  , oldCwd :: FilePath
  , fixture :: FilePath -> FilePath
  }

withTempDir :: (TestDirs -> Aff Unit) -> Aff Unit
withTempDir = Aff.bracket createTempDir cleanupTempDir
  where
  createTempDir = do
    oldCwd <- liftEffect $ Process.cwd
    temp <- mkTemp' $ Just "spago-test-"
    FS.mkdirp temp
    liftEffect $ Process.chdir temp
    -- log $ "Running test in " <> temp
    let
      fixturesPath = oldCwd <> "/test/fixtures"

      fixture path = Path.concat [ fixturesPath, path ]

      spago :: Array String -> Aff (Either ExecError ExecResult)
      spago args =
        Cmd.exec
          (Path.concat [ oldCwd, "bin", "index.dev.js" ])
          args
          $ Cmd.defaultExecOptions { pipeStdout = false, pipeStderr = false }

    pure
      { spago
      , oldCwd
      , fixture
      }

  cleanupTempDir { oldCwd } = do
    liftEffect $ Process.chdir oldCwd

checkFixture :: String -> String -> Aff Unit
checkFixture filepath fixturePath = do
  filecontent <- FS.readTextFile filepath
  fixturecontent <- FS.readTextFile fixturePath
  filecontent `Assert.shouldEqual` fixturecontent

shouldBeSuccess :: Either ExecError ExecResult -> Aff _
shouldBeSuccess execResult = execResult `Assert.shouldSatisfy` Either.isRight

shouldBeFailure :: Either ExecError ExecResult -> Aff _
shouldBeFailure execResult = execResult `Assert.shouldSatisfy` Either.isLeft

shouldBeSuccessStderr :: String -> Either ExecError ExecResult -> Aff _
shouldBeSuccessStderr expectedFixture execResult = do
  execResult `Assert.shouldSatisfy` Either.isRight
  let
    stderr = case execResult of
      Left err -> err.stderr
      Right res -> res.stderr
  expected <- String.trim <$> FS.readTextFile expectedFixture
  stderr `Assert.shouldEqual` expected

shouldBeFailureStderr :: String -> Either ExecError ExecResult -> Aff _
shouldBeFailureStderr expectedFixture execResult = do
  execResult `Assert.shouldSatisfy` Either.isLeft
  let
    stderr = case execResult of
      Left err -> err.stderr
      Right res -> res.stderr
  expected <- String.trim <$> FS.readTextFile expectedFixture
  stderr `Assert.shouldEqual` expected

shouldBeSuccessOutput :: String -> Either ExecError ExecResult -> Aff _
shouldBeSuccessOutput expectedFixture execResult = do
  execResult `Assert.shouldSatisfy` Either.isRight
  let
    stdout = case execResult of
      Left err -> err.stdout
      Right res -> res.stdout
  expected <- String.trim <$> FS.readTextFile expectedFixture
  stdout `Assert.shouldEqual` expected
