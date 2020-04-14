module Spago.Async
  ( module Spago.Async
  , Async.Async
  ) where

import Spago.Prelude

import qualified Control.Concurrent.Async.Pool         as Async


withTaskGroup :: (MonadIO m, MonadReader env m, HasLogFunc env, MonadUnliftIO m) => Int -> (Async.TaskGroup -> m b) -> m b
withTaskGroup n action = withRunInIO $ \run -> Async.withTaskGroup n (\taskGroup -> run $ action taskGroup)

async :: (MonadIO m, MonadReader env m, HasLogFunc env, MonadUnliftIO m) => Async.TaskGroup -> m a -> m (Async.Async a)
async taskGroup action = withRunInIO $ \run -> Async.async taskGroup (run action)

wait :: MonadIO m => Async.Async a -> m a
wait = liftIO . Async.wait

cancel :: MonadIO m => Async.Async a -> m ()
cancel = liftIO . Async.cancel

waitCatch :: MonadIO m => Async.Async a -> m (Either SomeException a)
waitCatch = liftIO . Async.waitCatch

mapTasks :: Traversable t => Async.TaskGroup -> t (RIO env a) -> RIO env (t a)
mapTasks taskGroup actions = withRunInIO $ \run -> Async.mapTasks taskGroup (run <$> actions)
