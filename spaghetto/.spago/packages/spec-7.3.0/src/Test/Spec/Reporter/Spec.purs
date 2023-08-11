module Test.Spec.Reporter.Spec (specReporter) where

import Prelude

import Control.Monad.State (class MonadState, get, modify)
import Control.Monad.Writer (class MonadWriter)
import Data.Array (length)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Time.Duration (Milliseconds(..))
import Test.Spec.Console (tellLn)
import Test.Spec.Reporter.Base (RunningItem(..), defaultReporter, defaultSummary, defaultUpdate)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event (Execution(..))
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed as Speed
import Test.Spec.Style (styled)
import Test.Spec.Style as Style
import Test.Spec.Tree (Path)

type State = { runningItems :: Map Path RunningItem, numFailures :: Int }

initialState :: State
initialState = { runningItems: Map.empty, numFailures: 0}

specReporter :: Reporter
specReporter = defaultReporter initialState $ defaultUpdate
  { getRunningItems: _.runningItems
  , putRunningItems: flip _{runningItems = _}
  , printFinishedItem: \path -> case _ of
      RunningTest name (Just res) -> print path $ PrintTest name res
      RunningPending name -> print path $ PrintPending name
      RunningSuite name true -> print path $ PrintSuite name
      _ -> pure unit
  , update: case _ of
      Event.Suite Sequential path name -> do
        print path $ PrintSuite name
      Event.TestEnd path name res -> do
        {runningItems} <- get
        when (isNothing $ Map.lookup path runningItems) do
          print path $ PrintTest name res
      Event.Pending path name -> do
        {runningItems} <- get
        when (Map.isEmpty runningItems) do
          print path $ PrintPending name
      Event.End results -> defaultSummary results
      _ -> pure unit
  }

data PrintAction
  = PrintSuite String
  | PrintTest String Result
  | PrintPending String

derive instance printActionGeneric :: Generic PrintAction _
instance printActionShow :: Show PrintAction where show = genericShow

print
  :: forall s m
   . MonadState { numFailures :: Int | s } m
  => MonadWriter String m
  => Path
  -> PrintAction
  -> m Unit
print path = case _ of
  PrintSuite name -> do
    tellLn $ indent path <> name
  PrintTest name (Success speed (Milliseconds ms)) -> do
    let
      speedDetails = case speed of
        Speed.Fast -> ""
        _ -> styled (Speed.toStyle speed) $ " (" <> show (Int.round ms) <> "ms)"
    tellLn $ (indent path) <> styled Style.green "✓︎ " <> styled Style.dim name <> speedDetails
  PrintTest name (Failure _) -> do
    {numFailures} <- modify \s -> s{numFailures = s.numFailures +1}
    tellLn $ (indent path) <> styled Style.red (show numFailures <> ") " <> name)
  PrintPending name -> do
    tellLn $ (indent path) <> (styled Style.cyan $ "- " <> name)
  where
    indent = length >>> Style.indent

