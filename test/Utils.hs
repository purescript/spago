module Utils
  ( checkFixture
  , readFixture
  , rmtree
  , runFor
  , shouldBeFailure
  , shouldBeFailureOutput
  , shouldBeSuccess
  , shouldBeSuccessOutput
  , spago
  , withCwd
  ) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception  as Exception
import           Prelude            hiding (FilePath)
import           System.Directory   (removePathForcibly)
import qualified System.Process     as Process
import           Test.Hspec         (HasCallStack, shouldBe, shouldSatisfy)
import           Turtle             (ExitCode (..), FilePath, Text, cd, empty, encodeString,
                                     procStrictWithErr, pwd, readTextFile)

withCwd :: FilePath -> IO () -> IO ()
withCwd dir cmd = do
  oldDir <- pwd
  Exception.bracket (cd dir) (const $ cd oldDir) (const cmd)

spago :: [Text] -> IO (ExitCode, Text, Text)
spago args =
  procStrictWithErr "spago" args empty

runFor :: Int -> String -> [String] -> IO ()
runFor us cmd args = do
  p <- Process.spawnProcess cmd args
  Concurrent.threadDelay us
  Process.terminateProcess p

shouldBeSuccess :: HasCallStack => (ExitCode, Text, Text) -> IO ()
shouldBeSuccess result@(_code, _stdout, _stderr) = do
  -- print $ "STDOUT: " <> _stdout
  -- print $ "STDERR: " <> _stderr
  result `shouldSatisfy` (\(code, _, _) -> code == ExitSuccess)

shouldBeSuccessOutput :: HasCallStack => FilePath -> (ExitCode, Text, Text) -> IO ()
shouldBeSuccessOutput expected result = do
  expectedContent <- readFixture expected
  result `shouldSatisfy` (\(code, stdout, _stderr) -> code == ExitSuccess && stdout == expectedContent)

shouldBeFailure :: HasCallStack => (ExitCode, Text, Text) -> IO ()
shouldBeFailure result@(_code, _stdout, _stderr) = do
  -- print $ "STDOUT: " <> _stdout
  -- print $ "STDERR: " <> _stderr
  result `shouldSatisfy` (\(code, _, _) -> code == ExitFailure 1)

shouldBeFailureOutput :: HasCallStack => FilePath -> (ExitCode, Text, Text) -> IO ()
shouldBeFailureOutput expected result = do
  expectedContent <- readFixture expected
  result `shouldSatisfy` (\(code, _stdout, stderr) -> code == ExitFailure 1 && stderr == expectedContent)

readFixture :: FilePath -> IO Text
readFixture path =
  readTextFile $ "../fixtures/" <> path

checkFixture :: HasCallStack => FilePath -> IO ()
checkFixture path = do
  actual <- readTextFile path
  expected <- readFixture path
  actual `shouldBe` expected

rmtree :: FilePath -> IO ()
rmtree = removePathForcibly . encodeString
