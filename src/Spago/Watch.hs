{-# LANGUAGE TupleSections #-}
module Spago.Watch (watch, globToParent, ClearScreen (..)) where

-- This code basically comes straight from
-- https://github.com/commercialhaskell/stack/blob/0740444175f41e6ea5ed236cd2c53681e4730003/src/Stack/FileWatch.hs

import           Spago.Prelude          hiding (FilePath)

import           Control.Concurrent.STM (check)
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           GHC.IO                 (FilePath)
import           GHC.IO.Exception
import           System.Console.ANSI    (clearScreen)
import qualified System.FilePath.Glob   as Glob
import qualified System.FSNotify        as Watch
import           System.IO              (getLine)
import qualified UnliftIO
import           UnliftIO.Async         (race_)
import           Data.Time.Clock        (NominalDiffTime, getCurrentTime, diffUTCTime)

-- Should we clear the screen on rebuild?
data ClearScreen = DoClear | NoClear
  deriving Eq

watch :: Spago m => Set.Set Glob.Pattern -> ClearScreen -> m () -> m ()
watch globs shouldClear action = do
  let config = Watch.defaultConfig
  fileWatchConf config shouldClear $ \getGlobs -> do
    getGlobs globs
    action


withManagerConf :: Spago m => Watch.WatchConfig -> (Watch.WatchManager -> m a) -> m a
withManagerConf conf = UnliftIO.bracket
  (liftIO $ Watch.startManagerConf conf)
  (liftIO . Watch.stopManager)


debounceTime :: NominalDiffTime
debounceTime = 0.1


-- | Run an action, watching for file changes
--
-- The action provided takes a callback that is used to set the files to be
-- watched. When any of those files are changed, we rerun the action again.
fileWatchConf
  :: Spago m
  => Watch.WatchConfig
  -> ClearScreen
  -> ((Set.Set Glob.Pattern -> m ()) -> m ())
  -> m ()
fileWatchConf watchConfig shouldClear inner = withManagerConf watchConfig $ \manager -> do
    allGlobs  <- liftIO $ newTVarIO Set.empty
    dirtyVar  <- liftIO $ newTVarIO True
    -- `lastEvent` is used for event debouncing.
    -- We don't use built-in debouncing because it does not work well with some
    -- text editors (#346).
    lastEvent <- liftIO $ do
      timeNow <- getCurrentTime
      newTVarIO (timeNow, "")
    watchVar  <- liftIO $ newTVarIO Map.empty

    let redisplay maybeMsg = do
          when (shouldClear == DoClear) $ liftIO clearScreen
          mapM_ echoStr maybeMsg

    let onChange event = do
          timeNow <- liftIO getCurrentTime

          rebuilding <- liftIO $ atomically $ do
            globs                <- readTVar allGlobs
            (lastTime, lastPath) <- readTVar lastEvent

            let shouldRebuild =
                  ( or ((\glob -> Glob.match glob $ Watch.eventPath event) <$> Set.toList globs)
                 && ( lastPath /= Watch.eventPath event
                   || diffUTCTime timeNow lastTime > debounceTime
                    )
                  )

            when shouldRebuild
              (writeTVar dirtyVar True)

            pure shouldRebuild

          when rebuilding $ do
            liftIO $ atomically $ writeTVar lastEvent (timeNow, Watch.eventPath event)
            redisplay $ Just $ "File changed, triggered a build: " <> show (Watch.eventPath event)

        setWatched :: Spago m => Set.Set Glob.Pattern -> m ()
        setWatched globs = do
          liftIO $ atomically $ writeTVar allGlobs globs
          watch0 <- liftIO $ readTVarIO watchVar
          let actions = Map.mergeWithKey
                keepListening
                stopListening
                startListening
                watch0
                newDirs
          watch1 <- liftIO $ forM (Map.toList actions) $ \(k, mmv) -> do
            mv' <- mmv
            return $
              case mv' of
                Nothing -> Map.empty
                Just v  -> Map.singleton k v
          liftIO $ atomically $ writeTVar watchVar $ Map.unions watch1
          where
            newDirs = Map.fromList $ map (, ())
                    $ Set.toList
                    $ Set.map globToParent globs

            keepListening _dir listen () = Just $ return $ Just listen

            stopListening = Map.map $ \f -> do
              () <- f `catch` \ioe ->
                -- Ignore invalid argument error - it can happen if
                -- the directory is removed.
                case ioe_type ioe of
                  InvalidArgument -> return ()
                  _               -> throwM ioe
              return Nothing

            startListening = Map.mapWithKey $ \dir () -> do
              listen <- Watch.watchTree manager dir (const True) $ onChange
              return $ Just listen

    let watchInput :: Spago m => m ()
        watchInput = do
          line <- liftIO $ getLine
          unless (line == "quit") $ liftIO $ do
            case line of
              "help" -> do
                echo ""
                echo "help: display this help"
                echo "quit: exit"
                echo "build: force a rebuild"
                echo "watched: display watched files"
              "build" -> do
                redisplay Nothing
                atomically $ writeTVar dirtyVar True
              "watched" -> do
                watch' <- readTVarIO allGlobs
                mapM_ echoStr (Glob.decompile <$> Set.toList watch')
              "" -> do
                redisplay Nothing
                atomically $ writeTVar dirtyVar True
              _ -> echoStr $ concat
                  [ "Unknown command: "
                  , show line
                  , ". Try 'help'"
                  ]

          watchInput

    race_ watchInput $ forever $ do
      liftIO $ atomically $ do
        dirty <- readTVar dirtyVar
        check dirty
        writeTVar dirtyVar False

      eres :: Either SomeException () <- try $ inner setWatched

      case eres of
        Left e -> echoStr $ show e
        _      -> echo "Success! Waiting for next file change."

      echo "Type help for available commands. Press enter to force a rebuild."


globToParent :: Glob.Pattern -> FilePath
globToParent glob = go base $ map Text.unpack pathComponents
  where
    path = Glob.decompile glob

    base = case isAbsolute path of
      True  -> [pathSeparator]
      False -> "."

    pathComponents = Text.split (== pathSeparator) $ Text.pack path

    go acc []           = acc
    go acc ("*":_rest)  = acc
    go acc ("**":_rest) = acc
    go acc [_file]      = acc
    go acc (h:rest)     = go (acc </> h) rest
