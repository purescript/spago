{-# LANGUAGE CPP, TupleSections #-}
module Spago.Watch (watch, globToParent, ClearScreen (..)) where

-- This code basically comes straight from
-- https://github.com/commercialhaskell/stack/blob/0740444175f41e6ea5ed236cd2c53681e4730003/src/Stack/FileWatch.hs

import           Spago.Prelude            hiding (FilePath, hFlush)

import           Control.Concurrent.STM   (check)
import           Control.Monad.Trans.Cont (ContT (..))
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import           Data.Text                (pack, toLower, unpack)
import           Data.Time.Clock          (NominalDiffTime, diffUTCTime, getCurrentTime)
import           GHC.IO                   (FilePath)
import           GHC.IO.Exception
import qualified GHC.IO.Handle            as Handle (BufferMode (..))
import           System.Console.ANSI      (hClearScreen, hSetCursorPosition)
import           System.FilePath          (splitDirectories)
import qualified System.FilePath.Glob     as Glob
import qualified System.FSNotify          as Watch
#ifdef mingw32_HOST_OS
import           System.IO                (getLine, hFlush, openFile, stdout)
#else
import           System.IO                (getLine, hFlush, openFile)
import           System.Posix.Terminal    (getControllingTerminalName)
#endif
import qualified UnliftIO                 hiding (hFlush)
import           UnliftIO.Async           (race_)

import           Spago.Messages           as Messages

-- Should we clear the screen on rebuild?
data ClearScreen = DoClear | NoClear
  deriving Eq

watch :: Set.Set Glob.Pattern -> ClearScreen -> Spago () -> Spago ()
watch globs shouldClear action = flip runContT return $ do
  let conf = Watch.defaultConfig { Watch.confDebounce = Watch.NoDebounce }
  manager <- ContT $ withManagerConf conf
  terminalHandle <- ContT $ withTerminalHandle
  useGlobUser <- ContT $ \k -> k $ \useGlobs -> useGlobs globs *> action
  lift $ fileWatchConf manager terminalHandle shouldClear useGlobUser


withManagerConf :: Watch.WatchConfig -> (Watch.WatchManager -> Spago a) -> Spago a
withManagerConf conf = UnliftIO.bracket
  (liftIO $ Watch.startManagerConf conf)
  (liftIO . Watch.stopManager)


#ifdef mingw32_HOST_OS
withTerminalHandle :: (Handle -> Spago a) -> Spago a
withTerminalHandle = UnliftIO.bracket (return stdout) (const $ return ())
#else
withTerminalHandle :: (Handle -> Spago a) -> Spago a
withTerminalHandle = UnliftIO.bracket terminalHandle release
  where
    terminalHandle :: Spago Handle
    terminalHandle =
      (tryIO $ liftIO getControllingTerminalName) >>= \case
        Right terminalFilePath -> liftIO $ do
          terminalHandle' <- openFile terminalFilePath WriteMode
          hSetBuffering terminalHandle' Handle.NoBuffering
          return terminalHandle'
        Left e@IOError{..} -> case (ioe_type, ioe_location) of
          (UnsupportedOperation, "getControllingTerminalName") -> do
            logWarn $ display $ Messages.noControllingTerminal $ pack $ show e
            return stdout
          _ ->
            throwIO e
    release :: Handle -> Spago ()
    release handle' = do
      when (handle' /= stdout) $ liftIO $ do
        hClose handle'
#endif


debounceTime :: NominalDiffTime
debounceTime = 0.1

-- | Run an action, watching for file changes
--
-- The action provided takes a callback that is used to set the files to be
-- watched. When any of those files are changed, we rerun the action again.
fileWatchConf
  :: Watch.WatchManager
  -> Handle
  -> ClearScreen
  -> ((Set.Set Glob.Pattern -> Spago ()) -> Spago ())
  -> Spago ()
fileWatchConf manager terminalHandle shouldClear inner = do
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
            hClearScreen terminalHandle
            hSetCursorPosition terminalHandle 0 0
            when (terminalHandle == stdout) $ do
              hFlush terminalHandle
          mapM_ logInfo maybeMsg

    let onChange event = do
          timeNow <- liftIO getCurrentTime

          rebuilding <- liftIO $ atomically $ do
            globs                <- readTVar allGlobs
            (lastTime, lastPath) <- readTVar lastEvent

            let matches glob = Glob.match glob $ Watch.eventPath event
            -- We should rebuild if at least one of the globs matches the path,
            -- and the last event either has different path, or has happened
            -- more than `debounceTime` seconds ago.
            let shouldRebuild =
                   any matches globs
                 && ( lastPath /= Watch.eventPath event
                   || diffUTCTime timeNow lastTime > debounceTime
                    )

            when shouldRebuild $ do
              writeTVar dirtyVar True
              writeTVar lastEvent (timeNow, Watch.eventPath event)

            pure shouldRebuild

          when rebuilding $ do
            redisplay $ Just $ "File changed, triggered a build: " <> displayShow (Watch.eventPath event)

        setWatched :: Set.Set Glob.Pattern -> Spago ()
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

    let watchInput :: Spago ()
        watchInput = do
          -- env <- ask
          line <- liftIO $ unpack . toLower . pack <$> getLine
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

    race_ watchInput $ forever $ do
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
