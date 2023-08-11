module Test.Spec.Runner.Event where

import Prelude

import Test.Spec (Tree)
import Test.Spec.Result (Result)
import Test.Spec.Tree (Path)

type Name = String
type NumberOfTests = Int

data Execution = Parallel | Sequential
instance showExecution :: Show Execution where
  show = case _ of
    Parallel -> "Parallel"
    Sequential -> "Sequential"

data Event
  = Start NumberOfTests
  | Suite Execution Path Name
  | SuiteEnd Path
  | Test Execution Path Name
  | TestEnd Path Name Result
  | Pending Path Name
  | End (Array (Tree Void Result))

instance showEvent :: Show Event where
  show = case _ of
    Start n -> "Start " <> show n
    Suite e path name -> "Suite " <> show e <> show path <> ": " <> name
    SuiteEnd path -> "SuiteEnd " <> show path
    Test e path name -> "Test " <> show e <> show path <> " " <> name
    TestEnd path name res -> "TestEnd " <> show path <> " " <> name <> ": " <> show res
    Pending path name -> "Pending " <> show path <> " " <> name
    End results -> "End " <> show results
