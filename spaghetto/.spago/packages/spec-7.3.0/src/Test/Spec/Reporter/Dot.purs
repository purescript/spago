module Test.Spec.Reporter.Dot (dotReporter) where

import Prelude

import Control.Monad.State (modify)
import Control.Monad.Writer (tell)
import Test.Spec.Style (styled)
import Test.Spec.Style as Style
import Test.Spec.Console (tellLn)
import Test.Spec.Reporter.Base (defaultReporter)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed as Speed

type DotReporterConfig = { width :: Int }

dotReporter :: DotReporterConfig -> Reporter
dotReporter { width } = defaultReporter (-1) case _ of
  Event.TestEnd _ _ (Success speed _) -> wrap $ styled (Speed.toStyle speed) "."
  Event.TestEnd _ _ (Failure _) -> wrap $ styled Style.red "!"
  Event.Pending _ _ -> wrap $ styled Style.dim ","
  Event.End _ -> tellLn ""
  _ -> pure unit
  where
    wrap action = do
      n <- modify (_ + 1)
      when (n `mod` width == 0) do
        tellLn ""
      tell action
