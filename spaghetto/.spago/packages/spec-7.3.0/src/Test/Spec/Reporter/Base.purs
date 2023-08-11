module Test.Spec.Reporter.Base
  ( defaultSummary
  , defaultReporter
  , defaultUpdate
  , RunningItem(..)
  ) where

import Prelude

import Control.Monad.State (StateT, evalStateT, execStateT, get, gets, put)
import Control.Monad.State as State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (class MonadWriter, Writer, runWriter)
import Data.Either (Either(..))
import Data.Foldable (all, for_, intercalate, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.List (List(..), (:), reverse)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Class (liftEffect)
import Effect.Exception as Error
import Pipes (await, yield)
import Pipes.Core (Pipe)
import Test.Spec (Tree)
import Test.Spec as S
import Test.Spec.Console (tellLn)
import Test.Spec.Console as Console
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event (Event)
import Test.Spec.Runner.Event as Event
import Test.Spec.Style (styled)
import Test.Spec.Style as Style
import Test.Spec.Summary (Summary(..))
import Test.Spec.Summary as Summary
import Test.Spec.Tree (Path)


defaultSummary :: forall m
   . MonadWriter String m
  => Array (Tree Void Result)
  -> m Unit
defaultSummary xs = do
  case Summary.summarize xs of
    (Count {passed, failed, pending}) -> do
      when (passed  > 0) $ tellLn $ styled Style.green $ show passed  <> " passing"
      when (pending > 0) $ tellLn $ styled Style.cyan $ show pending <> " pending"
      when (failed  > 0) $ tellLn $ styled Style.red $ show failed  <> " failed"
  tellLn ""
  printFailures xs

printFailures
  :: forall m
   . MonadWriter String m
  => Array (Tree Void Result)
  -> m Unit
printFailures xs' = evalStateT (go xs') {i: 0, crumbs: Nil}
  where
    go :: Array (Tree Void Result) -> StateT { i :: Int, crumbs :: List String } m Unit
    go = traverse_ case _ of
      S.Node (Left n) xs -> do
        {crumbs} <- State.get
        State.modify_ _{crumbs = n : crumbs}
        go xs
        State.modify_ _{crumbs = crumbs}
      S.Node (Right v) _ -> absurd v
      S.Leaf n (Just (Failure err)) -> do
        {i, crumbs} <- State.modify \s -> s{i = s.i +1}
        let label = intercalate " " (reverse $ n:crumbs)
        tellLn $ show i <> ") " <> label
        tellLn $ styled Style.red $ Style.indent 2 <> Error.message err
      S.Leaf _ _ -> pure unit

-- | Monadic left scan with state.
-- | TODO: Is this already included in purescript-pipes somehow, or should be?
scanWithStateM
  :: forall a x m r
   . Monad m
  => (x -> a -> m x) -> m x -> Pipe a a m r
scanWithStateM step begin = do
  x <- lift begin
  go x
  where
    go x = do
        a  <- await
        yield a
        x' <- lift (step x a)
        go $ x'

-- | A default reporter implementation that can be used as a base to build
-- | other reporters on top of.
defaultReporter
  :: forall s
   . s
  -> (Event -> StateT s (Writer String) Unit)
  -> Reporter
defaultReporter initialState onEvent = pure initialState # scanWithStateM \s e ->
  let Tuple res log = runWriter $ execStateT (onEvent e) s
  in liftEffect $ Console.write log $> res


data RunningItem
  = RunningTest String (Maybe Result)
  | RunningPending String
  | RunningSuite String Boolean

derive instance runningItemGeneric :: Generic RunningItem _
instance runningItemShow :: Show RunningItem where show = genericShow

defaultUpdate
  :: forall s
  . { getRunningItems :: s -> Map Path RunningItem
    , putRunningItems :: Map Path RunningItem -> s -> s
    , printFinishedItem :: Path -> RunningItem -> StateT s (Writer String) Unit
    , update :: Event -> StateT s (Writer String) Unit
    }
  -> (Event -> StateT s (Writer String) Unit)
defaultUpdate opts e = do
  baseUpdate e
  opts.update e
  where
    baseUpdate = case _ of
      Event.Suite Event.Sequential _ _ ->
        pure unit
      Event.Suite Event.Parallel path name -> do
        modifyRunningItems $ Map.insert path $ RunningSuite name false
      Event.SuiteEnd path -> do
        modifyRunningItems $ flip Map.update path case _ of
          RunningSuite n _ -> Just $ RunningSuite n true
          _ -> Nothing
      Event.Test Event.Sequential _ _ -> do
        pure unit
      Event.Test Event.Parallel path name -> do
        modifyRunningItems $ Map.insert path $ RunningTest name Nothing
      Event.TestEnd path _ res -> do
        runningItem <- gets opts.getRunningItems
        case Map.lookup path runningItem of
          Just (RunningTest n _) ->
            modifyRunningItems $ Map.insert path $ RunningTest n $ Just res
          _ ->
            pure unit
      Event.Pending path name -> do
        runningItem <- gets opts.getRunningItems
        unless (Map.isEmpty runningItem) do
          modifyRunningItems $ Map.insert path $ RunningPending name
      Event.End _ -> pure unit
      Event.Start _ -> pure unit
    modifyRunningItems f = do
      s <- get
      let
        nextRunningItems = f $ opts.getRunningItems s
        allFinished = all runningItemIsFinished nextRunningItems
      put $ opts.putRunningItems (if allFinished then Map.empty else nextRunningItems) s

      when allFinished do
        for_ (asArray $ Map.toUnfoldable nextRunningItems) $ uncurry opts.printFinishedItem
      where
        asArray = identity :: Array ~> Array
        runningItemIsFinished = case _ of
          RunningPending _ -> true
          RunningTest _ res -> isJust res
          RunningSuite _ finished -> finished
