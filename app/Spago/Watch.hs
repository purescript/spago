{-# LANGUAGE TupleSections #-}
module Spago.Watch (watch, globToParent) where

-- This code basically comes straight from
-- https://github.com/commercialhaskell/stack/blob/0740444175f41e6ea5ed236cd2c53681e4730003/src/Stack/FileWatch.hs


import           Control.Concurrent.Async (race_)
import           Control.Concurrent.STM   (check)
import           Control.Exception        (SomeException, catch, throwIO, try)
import           Control.Monad            (forM, forever, unless, when)
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as Text
import           GHC.Conc                 (atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import           GHC.IO.Exception
import           System.FilePath          (isAbsolute, pathSeparator, (</>))
import qualified System.FilePath.Glob     as Glob
import qualified System.FSNotify          as Watch
import           System.IO                (getLine)

import           Spago.Turtle


watch :: Set.Set Glob.Pattern -> IO () -> IO ()
watch globs action = do
  fileWatchConf Watch.defaultConfig $ \getGlobs -> do
    getGlobs globs
    action


-- | Run an action, watching for file changes
--
-- The action provided takes a callback that is used to set the files to be
-- watched. When any of those files are changed, we rerun the action again.
fileWatchConf :: Watch.WatchConfig
              -> ((Set.Set Glob.Pattern -> IO ()) -> IO ())
              -> IO ()
fileWatchConf watchConfig inner = Watch.withManagerConf watchConfig $ \manager -> do
    allGlobs <- newTVarIO Set.empty
    dirtyVar <- newTVarIO True
    watchVar <- newTVarIO Map.empty

    let onChange event = do
          atomically $ do
            globs <- readTVar allGlobs
            when (or $ fmap (\glob -> Glob.match glob $ Watch.eventPath event) $ Set.toList globs)
              (writeTVar dirtyVar True)

        setWatched :: Set.Set Glob.Pattern -> IO ()
        setWatched globs = do
          atomically $ writeTVar allGlobs globs
          watch0 <- readTVarIO watchVar
          let actions = Map.mergeWithKey
                keepListening
                stopListening
                startListening
                watch0
                newDirs
          watch1 <- forM (Map.toList actions) $ \(k, mmv) -> do
            mv <- mmv
            return $
              case mv of
                Nothing -> Map.empty
                Just v  -> Map.singleton k v
          atomically $ writeTVar watchVar $ Map.unions watch1
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
                  _               -> throwIO ioe
              return Nothing

            startListening = Map.mapWithKey $ \dir () -> do
                -- let dir' = fromString $ toFilePath dir
                listen <- Watch.watchTree manager dir (const True) onChange
                return $ Just listen

    let watchInput = do
          line <- getLine
          unless (line == "quit") $ do
            case line of
              "help" -> do
                echo ""
                echo "help: display this help"
                echo "quit: exit"
                echo "build: force a rebuild"
                echo "watched: display watched files"
              "build" -> atomically $ writeTVar dirtyVar True
              "watched" -> do
                watch' <- readTVarIO allGlobs
                mapM_ echoStr (Glob.decompile <$> Set.toList watch')
              "" -> atomically $ writeTVar dirtyVar True
              _ -> echoStr $ concat
                  [ "Unknown command: "
                  , show line
                  , ". Try 'help'"
                  ]

          watchInput

    race_ watchInput $ forever $ do
      atomically $ do
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
