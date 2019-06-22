module Utils
  ( checkFixture
  , getHighestTag
  , git
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
import qualified Data.Text          as Text
import           Prelude            hiding (FilePath)
import           System.Directory   (removePathForcibly)
import qualified System.Process     as Process
import           Test.Hspec         (shouldBe)
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

shouldBeSuccess :: (ExitCode, Text, Text) -> IO ()
shouldBeSuccess (code, _stdout, _stderr) = do
  -- print $ "STDOUT: " <> _stdout
  -- print $ "STDERR: " <> _stderr
  code `shouldBe` ExitSuccess

shouldBeSuccessOutput :: FilePath -> (ExitCode, Text, Text) -> IO ()
shouldBeSuccessOutput expected (code, out, _) = do
  expectedContent <- readFixture expected
  (code, out) `shouldBe` (ExitSuccess, expectedContent)

shouldBeFailure :: (ExitCode, Text, Text) -> IO ()
shouldBeFailure (code, _stdout, _stderr) = do
  -- print $ "STDOUT: " <> _stdout
  -- print $ "STDERR: " <> _stderr
  code `shouldBe` ExitFailure 1

shouldBeFailureOutput :: FilePath -> (ExitCode, Text, Text) -> IO ()
shouldBeFailureOutput expected (code, _, out) = do
  expectedContent <- readFixture expected
  (code, out) `shouldBe` (ExitFailure 1, expectedContent)

readFixture :: FilePath -> IO Text
readFixture path =
  readTextFile $ "../fixtures/" <> path

checkFixture :: FilePath -> IO ()
checkFixture path = do
  actual <- readTextFile path
  expected <- readFixture path
  actual `shouldBe` expected

rmtree :: FilePath -> IO ()
rmtree = removePathForcibly . encodeString

getHighestTag :: IO (Maybe Text)
getHighestTag = do
  tag <- strict $ limit 1 $ inproc "git" ["tag", "--list", "--sort=-version:refname", "v*"] empty
  pure $ case Text.strip tag of
    ""   -> Nothing
    tag' -> Just tag'
