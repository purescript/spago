module Data.Time.Gen
  ( genTime
  , module Data.Time.Component.Gen
  ) where

import Prelude
import Control.Monad.Gen (class MonadGen)
import Data.Time (Time(..))
import Data.Time.Component.Gen (genHour, genMillisecond, genMinute, genSecond)

-- | Generates a random `Time` between 00:00:00 and 23:59:59, inclusive.
genTime :: forall m. MonadGen m => m Time
genTime = Time <$> genHour <*> genMinute <*> genSecond <*> genMillisecond
