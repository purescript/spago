module Spago.DryRun
  ( DryRun(..)
  , DryAction(..)
  , runDryActions
  ) where

import Spago.Prelude


-- | Whether to actually perform side-effects
data DryRun = DryRun | NoDryRun

-- | Wrapper for Spago actions that can be dry run
data DryAction
  = DryAction
    { dryMessage :: Text
    , dryAction  :: Spago ()
    }

runDryActions :: DryRun -> NonEmpty DryAction -> Spago ()
runDryActions DryRun dryActions = do
  logWarn "this is a dry run, so these side effects were not performed:"
  for_ dryActions $ \DryAction{..} -> output $ "* " <> dryMessage
  output "\nUse the `--no-dry-run` flag to run them"
runDryActions NoDryRun dryActions = do
  for_ dryActions $ \DryAction{..} -> do
    output $ "** Running action: " <> dryMessage
    dryAction
