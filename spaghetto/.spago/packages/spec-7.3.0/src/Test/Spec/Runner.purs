module Test.Spec.Runner
  ( run
  , runSpecT
  , runSpec
  , runSpec'
  , defaultConfig
  , Config
  , TestEvents
  , Reporter
  ) where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parTraverse, parallel, sequential)
import Data.Array (groupBy, mapWithIndex)
import Data.Array.NonEmpty as NEA
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Function (on)
import Data.Identity (Identity(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (class Traversable, for)
import Effect (Effect)
import Effect.Aff (Aff, attempt, delay, forkAff, joinFiber, makeAff, throwError, try)
import Effect.Aff.AVar as AV
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (now)
import Pipes ((>->), yield)
import Pipes.Core (Pipe, Producer, (//>))
import Pipes.Core (runEffectRec) as P
import Prim.TypeError (class Warn, Text)
import Test.Spec (Item(..), Spec, SpecT, SpecTree, Tree(..), collect)
import Test.Spec.Console as Console
import Test.Spec.Result (Result(..))
import Test.Spec.Runner.Event (Event, Execution(..))
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed (speedOf)
import Test.Spec.Style (styled)
import Test.Spec.Style as Style
import Test.Spec.Summary (successful)
import Test.Spec.Tree (Path, PathItem(..), countTests, isAllParallelizable)

foreign import exit :: Int -> Effect Unit

type Config =
  { slow :: Milliseconds
  , timeout :: Maybe Milliseconds
  , exit :: Boolean
  }

defaultConfig :: Config
defaultConfig =
  { slow: Milliseconds 75.0
  , timeout: Just $ Milliseconds 2000.0
  , exit: true
  }

makeTimeout
  :: Milliseconds
  -> Aff Unit
makeTimeout ms@(Milliseconds ms') = do
  delay ms
  makeAff \cb -> mempty <$ do
    cb <<< Left $ error $ "test timed out after " <> show (Int.round ms') <> "ms"

timeout
  :: Milliseconds
  -> Aff Unit
  -> Aff Unit
timeout time t = do
  sequential (parallel (try (makeTimeout time)) <|> parallel (try t))
    >>= either throwError pure

type TestWithPath r = {test :: SpecTree Aff Unit, path :: Path | r}

-- Run the given spec as `Producer` in the underlying `Aff` monad.
-- This producer has two responsibilities:
--      1) emit events for key moments in the runner's lifecycle
--      2) collect the tst output into an array of results
-- This allows downstream consumers to report about the tests even before the
-- prodocer has completed and still benefit from the array of results the way
-- the runner sees it.
_run
  :: forall m
   . Functor m
  => Config
  -> SpecT Aff Unit m Unit
  -> m TestEvents
_run config = collect >>> map \tests -> do
  yield (Event.Start (countTests tests))
  let indexer index test = {test, path: [PathItem {name: Nothing, index}]}
  r <- loop $ mapWithIndex indexer tests
  yield (Event.End r)
  pure r
  where
    loop :: Array (TestWithPath ()) -> TestEvents
    loop tests =
      let
        noteWithIsAllParallelizable = map \{test,path} -> { isParallelizable: isAllParallelizable test, test, path}
        groupByIsParallelizable = groupBy (\a b -> a.isParallelizable && b.isParallelizable)
      in join <$> for (groupByIsParallelizable $ noteWithIsAllParallelizable tests) \g ->
        join <$> if (NEA.head g).isParallelizable
          then mergeProducers (runGroup <$> (NEA.toArray g))
          else for (NEA.toArray g) runGroup

    runGroup :: TestWithPath (isParallelizable :: Boolean) -> TestEvents
    runGroup {test, path, isParallelizable} = case test of
      (Leaf name (Just (Item item))) -> do
        yield $ Event.Test (if isParallelizable then Parallel else Sequential) path name
        let example = item.example \a -> a unit
        start <- lift $ liftEffect now
        e <- lift $ attempt case config.timeout of
          Just t -> timeout t example
          _      -> example
        end <- liftEffect now
        let duration = Milliseconds $ on (-) (unInstant >>> un Milliseconds) end start
        let res = either Failure (const $ Success (speedOf config.slow duration) duration) e
        yield $ Event.TestEnd path name res
        pure [ Leaf name $ Just res ]
      (Leaf name Nothing) -> do
        yield $ Event.Pending path name
        pure [ Leaf name Nothing ]
      (Node (Right cleanup) xs) -> do
        let indexer index x = {test:x, path: path <> [PathItem {name: Nothing, index}]}
        loop (mapWithIndex indexer xs) <* lift (cleanup unit)
      (Node (Left name) xs) -> do
        yield $ Event.Suite (if isParallelizable then Parallel else Sequential) path name
        let indexer index x = {test:x, path: path <> [PathItem {name: Just name, index}]}
        res <- loop (mapWithIndex indexer xs)
        yield $ Event.SuiteEnd path
        pure [ Node (Left name) res ]

-- https://github.com/felixSchl/purescript-pipes/issues/16
mergeProducers :: forall t o a. Traversable t => t (Producer o Aff a) -> Producer o Aff (t a)
mergeProducers ps = do
  var <- lift AV.empty

  fib <- lift $ forkAff do
    let consumer i = lift (AV.put i var) *> pure unit
    x <- parTraverse (\p -> P.runEffectRec $ p //> consumer) ps
    AV.kill (error "finished") var
    pure x

  let
    loop = do
      res <- lift $ try (AV.take var)
      case res of
        Left _ -> lift $ joinFiber fib
        Right e -> do
          yield e
          loop
  loop

type TestEvents = Producer Event Aff (Array (Tree Void Result))

type Reporter = Pipe Event Event Aff (Array (Tree Void Result))

-- | Run the spec with `config`, returning the results, which
-- | are also reported using specified Reporters, if any.
-- | If configured as such, `exit` the program upon completion
-- | with appropriate exit code.
runSpecT
  :: forall m
   . Functor m
  => Config
  -> Array Reporter
  -> SpecT Aff Unit m Unit
  -> m (Aff (Array (Tree Void Result)))
runSpecT config reporters spec = _run config spec <#> \runner -> do
  let
    drain = const (pure unit)
    events = foldl (>->) runner reporters
    reportedEvents = P.runEffectRec $ events //> drain
  if config.exit
    then try reportedEvents >>= case _ of
      Left err -> do
        liftEffect $ Console.write $ styled Style.red (show err <> "\n")
        liftEffect $ exit 1
        throwError err
      Right results -> liftEffect do
        let code = if successful results then 0 else 1
        exit code
        pure results
    else reportedEvents

-- | Run the spec with the default config
run
  :: Warn (Text "`Test.Spec.Runner.run` is Deprecated use runSpec instead")
  => Array Reporter
  -> Spec Unit
  -> Aff Unit
run = runSpec' defaultConfig

-- | Run the spec with the default config
runSpec
  :: Array Reporter
  -> Spec Unit
  -> Aff Unit
runSpec = runSpec' defaultConfig

runSpec'
  :: Config
  -> Array Reporter
  -> Spec Unit
  -> Aff Unit
runSpec' config reporters spec = void $ un Identity $ runSpecT config reporters spec
