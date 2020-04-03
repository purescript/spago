module Spago.DryRun
  ( DryRun(..)
  , DryAction(..)
  , runDryActions
  ) where

import Spago.Prelude


-- | Whether to actually perform side-effects
data DryRun = DryRun | NoDryRun

-- | Wrapper for Spago actions that can be dry run
data DryAction env
  = DryAction
    { dryMessage :: Text
    , dryAction  :: HasLogFunc env => RIO env ()
    }

runDryActions :: HasLogFunc env => DryRun -> NonEmpty (DryAction env) -> RIO env ()
runDryActions DryRun dryActions = do
  logWarn "this is a dry run, so these side effects were not performed:"
  for_ dryActions $ \DryAction{..} -> logWarn $ "* " <> display dryMessage
  logWarn "\nUse the `--no-dry-run` flag to run them"
runDryActions NoDryRun dryActions = do
  for_ dryActions $ \DryAction{..} -> do
    logInfo $ "** Running action: " <> display dryMessage
    dryAction
