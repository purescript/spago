module Utils
  ( checkFixture
  , checkFileHasInfix
  , readFixture
  , getHighestTag
  , git
  , rmtree
  , runFor
  , shouldBeFailure
  , shouldBeFailureInfix
  , shouldBeFailureOutput
  , shouldBeSuccess
  , shouldBeSuccessInfix
  , shouldBeSuccessOutput
  , shouldBeEmptySuccess
  , spago
  , withCwd
  ) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception  as Exception
import qualified Data.Text          as Text
import           Prelude            hiding (FilePath)
import           System.Directory   (removePathForcibly)
import qualified System.Process     as Process
import           Test.Hspec         (HasCallStack, shouldBe, shouldSatisfy)
import           Turtle             (ExitCode (..), FilePath, Text, cd, empty,
                                     encodeString, inproc, limit,
                                     procStrictWithErr, pwd, readTextFile,
                                     strict)

withCwd :: FilePath -> IO () -> IO ()
withCwd dir cmd = do
  oldDir <- pwd
  Exception.bracket (cd dir) (const $ cd oldDir) (const cmd)

spago :: [Text] -> IO (ExitCode, Text, Text)
spago args =
  procStrictWithErr "spago" args empty

git :: [Text] -> IO (ExitCode, Text, Text)
git args =
  procStrictWithErr "git" args empty

runFor :: Int -> String -> [String] -> IO ()
runFor us cmd args = do
  p <- Process.spawnProcess cmd args
  Concurrent.threadDelay us
  Process.terminateProcess p

shouldBeSuccess :: HasCallStack => (ExitCode, Text, Text) -> IO ()
shouldBeSuccess result@(_code, _stdout, _stderr) = do
  print $ "STDOUT: " <> _stdout
  print $ "STDERR: " <> _stderr
  result `shouldSatisfy` (\(code, _, _) -> code == ExitSuccess)

shouldBeSuccessOutput :: HasCallStack => FilePath -> (ExitCode, Text, Text) -> IO ()
shouldBeSuccessOutput expected result = do
  expectedContent <- readFixture expected
  result `shouldSatisfy` (\(code, stdout, _stderr) -> code == ExitSuccess && stdout == expectedContent)

shouldBeSuccessInfix :: HasCallStack => Text -> (ExitCode, Text, Text) -> IO ()
shouldBeSuccessInfix expected result =
  result `shouldSatisfy` (\(code, stdout, _stderr) -> code == ExitSuccess && Text.isInfixOf expected stdout)

shouldBeEmptySuccess :: HasCallStack => (ExitCode, Text, Text) -> IO ()
shouldBeEmptySuccess result = do
  result `shouldBe` (ExitSuccess, "", "")

shouldBeFailure :: HasCallStack => (ExitCode, Text, Text) -> IO ()
shouldBeFailure result@(_code, _stdout, _stderr) = do
  -- print $ "STDOUT: " <> _stdout
  -- print $ "STDERR: " <> _stderr
  result `shouldSatisfy` (\(code, _, _) -> code == ExitFailure 1)

shouldBeFailureOutput :: HasCallStack => FilePath -> (ExitCode, Text, Text) -> IO ()
shouldBeFailureOutput expected result = do
  expectedContent <- readFixture expected
  result `shouldSatisfy` (\(code, _stdout, stderr) -> code == ExitFailure 1 && stderr == expectedContent)

shouldBeFailureInfix :: HasCallStack => Text -> (ExitCode, Text, Text) -> IO ()
shouldBeFailureInfix expected result = do
  result `shouldSatisfy` (\(code, _stdout, stderr) -> code == ExitFailure 1 && Text.isInfixOf expected stderr)

readFixture :: FilePath -> IO Text
readFixture path =
  readTextFile $ "../fixtures/" <> path

checkFixture :: HasCallStack => FilePath -> IO ()
checkFixture path = do
  actual <- readTextFile path
  expected <- readFixture path
  actual `shouldBe` expected

checkFileHasInfix :: HasCallStack => FilePath -> Text -> IO ()
checkFileHasInfix path needle = do
  actual <- readTextFile path
  actual `shouldSatisfy` Text.isInfixOf needle

rmtree :: FilePath -> IO ()
rmtree = removePathForcibly . encodeString

getHighestTag :: IO (Maybe Text)
getHighestTag = do
  tag <- strict $ limit 1 $ inproc "git" ["tag", "--list", "--sort=-version:refname", "v*"] empty
  pure $ case Text.strip tag of
    ""   -> Nothing
    tag' -> Just tag'
