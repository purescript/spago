module Spago.DryRun
  ( DryRun(..)
  , DryAction(..)
  , runDryActions
  ) where

import Spago.Prelude


-- | Whether to actually perform side-effects
data DryRun = DryRun | NoDryRun

-- | Wrapper for Spago actions that can be dry run
data DryAction m
  = DryAction
    { dryMessage :: Text
    , dryAction  :: m ()
    }

runDryActions :: Spago m => DryRun -> NonEmpty (DryAction m) -> m () -> m ()
runDryActions DryRun dryActions _finalAction = do
  echo "\nWARNING: this is a dry run, so these side effects were not performed:"
  for_ dryActions $ \DryAction{..} -> echo $ "* " <> dryMessage
  echo "\nUse the `--no-dry-run` flag to run them"
runDryActions NoDryRun dryActions finalAction = do
  for_ dryActions $ \DryAction{..} -> do
    echo $ "** Running action: " <> dryMessage
    dryAction
  finalAction
