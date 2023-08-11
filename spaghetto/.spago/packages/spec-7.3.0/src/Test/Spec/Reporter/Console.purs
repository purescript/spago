module Test.Spec.Reporter.Console (consoleReporter) where

import Prelude

import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer (class MonadWriter)
import Data.Foldable (for_, intercalate)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Effect.Exception as Error
import Test.Spec.Console (tellLn)
import Test.Spec.Reporter.Base (RunningItem(..), defaultReporter, defaultUpdate)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event as Event
import Test.Spec.Style (styled)
import Test.Spec.Style as Style
import Test.Spec.Summary (Summary(..))
import Test.Spec.Summary as Summary
import Test.Spec.Tree (Path, Tree, parentSuite, parentSuiteName)

type State = { runningItems :: Map Path RunningItem, lastPrintedSuitePath :: Maybe Path}

initialState :: State
initialState = { runningItems: Map.empty, lastPrintedSuitePath: Nothing }

consoleReporter :: Reporter
consoleReporter = defaultReporter initialState $ defaultUpdate
  { getRunningItems: _.runningItems
  , putRunningItems: flip _{runningItems = _}
  , printFinishedItem: \path -> case _ of
      RunningTest name (Just res) -> print path $ PrintTest name res
      RunningPending name -> print path $ PrintPending name
      _ -> pure unit
  , update: case _ of
      Event.TestEnd path name res -> do
        {runningItems} <- get
        when (isNothing $ Map.lookup path runningItems) do
          print path $ PrintTest name res
      Event.Pending path name -> do
        {runningItems} <- get
        when (Map.isEmpty runningItems) do
          print path $ PrintPending name
      Event.End results -> printSummary results
      _ -> pure unit
  }

printSummary :: forall m. MonadWriter String m => Array (Tree Void Result) -> m Unit
printSummary = Summary.summarize >>> \(Count {passed, failed, pending}) -> do
  tellLn ""
  tellLn $ styled Style.bold "Summary"
  printPassedFailed passed failed
  printPending pending
  tellLn ""
  where
    printPassedFailed :: Int -> Int -> m Unit
    printPassedFailed p f = do
      let total = p + f
          testStr = pluralize "test" total
          amount = show p <> "/" <> (show total) <> " " <> testStr <> " passed"
          color = if f > 0 then Style.red else Style.dim
      tellLn $ styled color amount

    printPending :: Int -> m Unit
    printPending p
      | p > 0     = tellLn $ styled Style.yellow $ show p <> " " <> pluralize "test" p <> " pending"
      | otherwise = pure unit

    pluralize :: String -> Int -> String
    pluralize s 1 = s
    pluralize s _ = s <> "s"

data PrintAction
  = PrintTest String Result
  | PrintPending String

derive instance printActionGeneric :: Generic PrintAction _
instance printActionShow :: Show PrintAction where show = genericShow

print
  :: forall s m
   . MonadState { lastPrintedSuitePath :: Maybe Path | s} m
  => MonadWriter String m
  => Path
  -> PrintAction
  -> m Unit
print path a = do
  for_ (parentSuite path) \suite -> do
    s <- get
    case s.lastPrintedSuitePath of
      Just p | p == suite.path -> pure unit
      _ -> do
        tellLn $ styled (Style.bold <> Style.magenta)
          $ intercalate " » " (parentSuiteName suite.path <> [suite.name])
        put s{lastPrintedSuitePath = Just suite.path}
  case a of
    PrintTest name (Success _ _) -> do
      tellLn $ "  " <> styled Style.green "✓︎ " <> styled Style.dim name
    PrintTest name (Failure err) -> do
      tellLn $ "  " <> styled Style.red ("✗ " <> name <> ":")
      tellLn $ ""
      tellLn $ "  " <> styled Style.red (Error.message err)
    PrintPending name -> do
      tellLn $ "  " <> styled Style.cyan ("~ " <> name)
