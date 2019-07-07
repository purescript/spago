module Spago.Prelude
  ( echo
  , echoStr
  , echoDebug
  , tshow
  , die
  , Dhall.Core.throws
  , hush
  , pathFromText
  , assertDirectory
  , GlobalOptions (..)
  , Spago
  , module X
  , Typeable
  , Proxy(..)
  , Text
  , NonEmpty (..)
  , Seq (..)
  , Map
  , Generic
  , Alternative
  , Pretty
  , FilePath
  , IOException
  , ExitCode (..)
  , (<|>)
  , (</>)
  , (^..)
  , transformMOf
  , testfile
  , testdir
  , mktree
  , mv
  , cptree
  , chmod
  , executable
  , readTextFile
  , writeTextFile
  , atomically
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  , isAbsolute
  , pathSeparator
  , headMay
  , for
  , try
  , makeAbsolute
  , hPutStrLn
  , many
  , empty
  , callCommand
  , shell
  , shellStrict
  , systemStrictWithErr
  , viewShell
  , repr
  , with
  , appendonly
  , async'
  , withTaskGroup'
  , Turtle.mktempdir
  , getModificationTime
  ) where


import qualified Control.Concurrent.Async.Pool as Async
import qualified Data.Text                     as Text
import qualified Dhall.Core
import qualified System.FilePath               as FilePath
import qualified System.IO
import qualified Turtle                        as Turtle
import qualified UnliftIO.Directory            as Directory

import           Control.Applicative           (Alternative, empty, many, (<|>))
import           Control.Lens                  ((^..))
import           Control.Lens.Combinators      (transformMOf)
import           Control.Monad                 as X
import           Control.Monad.Catch           as X hiding (try)
import           Control.Monad.Reader          as X
import           Data.Aeson                    as X
import           Data.Bool                     as X
import           Data.Either                   as X
import           Data.Foldable                 as X
import           Data.List.NonEmpty            (NonEmpty (..))
import           Data.Map                      (Map)
import           Data.Maybe                    as X
import           Data.Sequence                 (Seq (..))
import           Data.Text                     (Text)
import           Data.Text.Prettyprint.Doc     (Pretty)
import           Data.Traversable              (for)
import           Data.Typeable                 (Proxy (..), Typeable)
import           GHC.Conc                      (atomically, newTVarIO, readTVar, readTVarIO,
                                                writeTVar)
import           GHC.Generics                  (Generic)
import           Prelude                       as X hiding (FilePath)
import           Safe                          (headMay)
import           System.FilePath               (isAbsolute, pathSeparator, (</>))
import           System.IO                     (hPutStrLn)
import           Turtle                        (ExitCode (..), FilePath, appendonly, mktree, repr,
                                                shell, shellStrict, systemStrictWithErr, testdir,
                                                testfile, chmod, executable)
import           UnliftIO                      (MonadUnliftIO, withRunInIO)
import           UnliftIO.Directory            (getModificationTime, makeAbsolute)
import           UnliftIO.Exception            (IOException, try)
import           UnliftIO.Process              (callCommand)

-- | Generic Error that we throw on program exit.
--   We have it so that errors are displayed nicely to the user
--   (the default Turtle.die is not nice)
newtype SpagoError = SpagoError { _unError :: Text }
instance Exception SpagoError
instance Show SpagoError where
  show (SpagoError err) = Text.unpack err


data GlobalOptions = GlobalOptions
  { debug :: Bool
  }

type Spago m =
  ( MonadReader GlobalOptions m
  , MonadIO m
  , MonadUnliftIO m
  , MonadCatch m
  , Turtle.Alternative m
  , MonadMask m
  )

echo :: MonadIO m => Text -> m ()
echo = Turtle.printf (Turtle.s Turtle.% "\n")

echoStr :: MonadIO m => String -> m ()
echoStr = echo . Text.pack

tshow :: Show a => a -> Text
tshow = Text.pack . show

echoDebug :: Spago m => Text -> m ()
echoDebug str = do
  hasDebug <- asks debug
  Turtle.when hasDebug $ do
    echo str

die :: MonadThrow m => Text -> m a
die reason = throwM $ SpagoError reason


-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just


pathFromText :: Text -> Turtle.FilePath
pathFromText = Turtle.fromText


readTextFile :: MonadIO m => Turtle.FilePath -> m Text
readTextFile = liftIO . Turtle.readTextFile


writeTextFile :: MonadIO m => Turtle.FilePath -> Text -> m ()
writeTextFile path text = liftIO $ Turtle.writeTextFile path text


with :: MonadIO m => Turtle.Managed a -> (a -> IO r) -> m r
with r f = liftIO $ Turtle.with r f


viewShell :: (MonadIO m, Show a) => Turtle.Shell a -> m ()
viewShell = Turtle.view


mv :: MonadIO m => System.IO.FilePath -> System.IO.FilePath -> m ()
mv from to = Turtle.mv (Turtle.decodeString from) (Turtle.decodeString to)


cptree :: MonadIO m => System.IO.FilePath -> System.IO.FilePath -> m ()
cptree from to = Turtle.cptree (Turtle.decodeString from) (Turtle.decodeString to)


withTaskGroup' :: Spago m => Int -> (Async.TaskGroup -> m b) -> m b
withTaskGroup' n action = withRunInIO $ \run -> Async.withTaskGroup n (\taskGroup -> run $ action taskGroup)

async' :: Spago m => Async.TaskGroup -> m a -> m (Async.Async a)
async' taskGroup action = withRunInIO $ \run -> Async.async taskGroup (run action)


-- | Code from: https://github.com/dhall-lang/dhall-haskell/blob/d8f2787745bb9567a4542973f15e807323de4a1a/dhall/src/Dhall/Import.hs#L578
assertDirectory :: (MonadIO m, MonadThrow m) => FilePath.FilePath -> m ()
assertDirectory directory = do
  let private = transform Directory.emptyPermissions
        where
          transform =
            Directory.setOwnerReadable   True
            .   Directory.setOwnerWritable   True
            .   Directory.setOwnerSearchable True

  let accessible path =
        Directory.readable   path
        && Directory.writable   path
        && Directory.searchable path

  directoryExists <- Directory.doesDirectoryExist directory

  if directoryExists
    then do
      permissions <- Directory.getPermissions directory
      unless (accessible permissions) $ do
        die $ "Directory " <> tshow directory <> " is not accessible. " <> tshow permissions
    else do
      assertDirectory (FilePath.takeDirectory directory)

      Directory.createDirectory directory

      Directory.setPermissions directory private
