module Spago.DryRun
  ( DryRun(..)
  , showHelp
  ) where

import Spago.Prelude


-- | Whether to actually perform side-effects
data DryRun = DryRun | NoDryRun deriving Eq


showHelp :: Spago m => DryRun -> m ()
showHelp dryRun = do
  when (dryRun == DryRun) $ do
    echo "This is a dry run. Side effects will not be performed. Pass --no-dry-run to change this."
