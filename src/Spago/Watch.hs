{-# LANGUAGE TupleSections #-}
module Spago.Watch (watch, globToParent) where

-- This code is derived from:
-- https://github.com/commercialhaskell/stack/blob/0740444175f41e6ea5ed236cd2c53681e4730003/src/Stack/FileWatch.hs

import           Spago.Prelude          hiding (FilePath)
import           Spago.Env

import           Control.Concurrent.STM (check)
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text.IO
import           Data.Time.Clock        (NominalDiffTime, diffUTCTime, getCurrentTime)
import           GHC.IO                 (FilePath)
import           GHC.IO.Exception
import           System.Console.ANSI    (clearScreen, setCursorPosition)
import           System.FilePath        (splitDirectories)
import qualified System.FilePath.Glob   as Glob
import qualified System.FSNotify        as Watch
import qualified System.IO.Utf8         as Utf8
import qualified System.Process       
import qualified UnliftIO
import qualified UnliftIO.Async         as Async
import qualified UnliftIO.IORef         as IORef
import qualified UnliftIO.Process       as Process

import qualified Spago.Git              as Git

watch
  :: (HasLogFunc env, HasGit env)
  => Set.Set Glob.Pattern -> ClearScreen -> AllowIgnored -> IORef (Maybe Process.ProcessHandle) -> RIO env ()
  -> RIO env ()
watch globs shouldClear allowIgnored handleRef action = do
  let config = Watch.defaultConfig { Watch.confDebounce = Watch.NoDebounce }
  fileWatchConf config shouldClear allowIgnored handleRef $ \getGlobs -> do
    getGlobs globs
    action


withManagerConf :: Watch.WatchConfig -> (Watch.WatchManager -> RIO env a) -> RIO env a
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
  :: forall env
  .  (HasLogFunc env, HasGit env)
  => Watch.WatchConfig
  -> ClearScreen
  -> AllowIgnored
  -> IORef (Maybe Process.ProcessHandle)
  -> ((Set.Set Glob.Pattern -> RIO env ()) -> RIO env ())
  -> RIO env ()
fileWatchConf watchConfig shouldClear allowIgnored handleRef inner = withManagerConf watchConfig $ \manager -> do
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
          when (shouldClear == DoClear) $ liftIO $ do
            clearScreen
            setCursorPosition 0 0
            hFlush stdout
          mapM_ logInfo maybeMsg

    let onChange event = do
          timeNow <- liftIO getCurrentTime
          let eventPath = Watch.eventPath event
              isPathIgnored =
                case allowIgnored of
                  NoAllowIgnored -> Git.isIgnored
                  DoAllowIgnored -> const (pure False)

          pathIgnored <- isPathIgnored $ Text.pack eventPath

          unless pathIgnored $ do
            rebuilding <- liftIO $ atomically $ do
              globs                <- readTVar allGlobs
              (lastTime, lastPath) <- readTVar lastEvent

              let matches glob = Glob.match glob eventPath
              -- We should rebuild if at least one of the globs matches the path,
              -- and the last event either has different path, or has happened
              -- more than `debounceTime` seconds ago.
              let shouldRebuild =
                     any matches globs
                   && ( lastPath /= eventPath
                     || diffUTCTime timeNow lastTime > debounceTime
                      )

              when shouldRebuild $ do
                writeTVar dirtyVar True
                writeTVar lastEvent (timeNow, eventPath)

              pure shouldRebuild

            when rebuilding $ do
              maybeHandle <- IORef.readIORef handleRef
              for_ maybeHandle $ \h -> do
                pid <- liftIO $ System.Process.getPid h
                logDebug $ "Terminating process: " <> displayShow pid
                Process.terminateProcess h
              redisplay $ Just $ "File changed, triggered a build: " <> displayShow eventPath

        setWatched :: Set.Set Glob.Pattern -> RIO env ()
        setWatched globs = do
          liftIO $ atomically $ writeTVar allGlobs globs
          watch0 <- liftIO $ readTVarIO watchVar
          env <- ask
          let startListening = Map.mapWithKey $ \dir () -> do
                listen <- Watch.watchTree manager dir (const True) (runRIO env . onChange)
                return $ Just listen
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

    let watchInput :: RIO env ()
        watchInput = do
          line <- Utf8.withHandle stdin (liftIO $ Text.toLower <$> Text.IO.hGetLine stdin)
          if line == "quit" then logInfo "Leaving watch mode."
          else do
            case line of
              "help" -> traverse_ logInfo
                          [ ""
                          , "help: display this help"
                          , "quit: exit"
                          , "build: force a rebuild"
                          , "watched: display watched files"
                          ]
              "build" -> do
                redisplay Nothing
                atomically $ writeTVar dirtyVar True
              "watched" -> do
                watch' <- readTVarIO allGlobs
                mapM_ (logInfo . displayShow) (Glob.decompile <$> Set.toList watch')
              "" -> do
                redisplay Nothing
                atomically $ writeTVar dirtyVar True
              _ -> logWarn $ displayShow $ concat
                     [ "Unknown command: "
                     , show line
                     , ". Try 'help'"
                     ]
            watchInput

    Async.race_ watchInput $ forever $ do
      liftIO $ atomically $ do
        dirty <- readTVar dirtyVar
        check dirty
        writeTVar dirtyVar False

      eres :: Either SomeException () <- try $ inner setWatched

      case eres of
        Left e -> logWarn $ display e
        _      -> logInfo "Success! Waiting for next file change."

      logInfo "Type help for available commands. Press enter to force a rebuild."


globToParent :: Glob.Pattern -> FilePath
globToParent glob = go pathHead pathRest
  where
    pathHead : pathRest = splitDirectories $ Glob.decompile glob

    go acc []           = acc
    go acc ("*":_rest)  = acc
    go acc ("**":_rest) = acc
    go acc [_file]      = acc
    go acc (h:rest)     = go (acc </> h) rest
