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
  , UsePsa(..)
  , Spago
  , module X
  , Typeable
  , Proxy(..)
  , Text
  , NonEmpty (..)
  , Seq (..)
  , IsString
  , Map
  , Set
  , Generic
  , Alternative
  , Pretty
  , FilePath
  , IOException
  , ExitCode (..)
  , Validation(..)
  , (<|>)
  , (</>)
  , (^..)
  , set
  , surroundQuote
  , transformMOf
  , testfile
  , testdir
  , mktree
  , mv
  , cptree
  , bimap
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
  , shouldRefreshFile
  , for
  , handleAny
  , try
  , tryIO
  , makeAbsolute
  , hPutStrLn
  , many
  , empty
  , callCommand
  , shell
  , shellStrict
  , shellStrictWithErr
  , systemStrictWithErr
  , Turtle.proc
  , Turtle.inproc
  , Turtle.strict
  , Turtle.procStrictWithErr
  , Turtle.procs
  , viewShell
  , repr
  , with
  , appendonly
  , async'
  , mapTasks'
  , withTaskGroup'
  , Turtle.mktempdir
  , getModificationTime
  , docsSearchVersion
  , githubTokenEnvVar
  , whenM
  , encodeUtf8
  , decodeUtf8
  , pretty
  ) where


import qualified Control.Concurrent.Async.Pool         as Async
import qualified Data.Text                             as Text
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as PrettyText
import qualified Dhall.Core
import qualified System.FilePath                       as FilePath
import qualified System.IO

import qualified Turtle                                as Turtle
import qualified UnliftIO.Directory                    as Directory

import           Control.Applicative                   (Alternative, empty, many, (<|>))
import           Control.Monad                         as X
import           Control.Monad.Catch                   as X hiding (try)
import           Control.Monad.Reader                  as X
import           Data.Aeson                            as X hiding (Result (..))
import           Data.Bifunctor                        (bimap)
import           Data.Bool                             as X
import           Data.Either                           as X
import           Data.Either.Validation                (Validation (..))
import           Data.Foldable                         as X
import           Data.List.NonEmpty                    (NonEmpty (..))
import           Data.Map                              (Map)
import           Data.Maybe                            as X
import           Data.Sequence                         (Seq (..))
import           Data.Set                              (Set)
import           Data.String                           (IsString)
import           Data.Text                             (Text)
import           Data.Text.Encoding                    (decodeUtf8, encodeUtf8)
import           Data.Text.Prettyprint.Doc             (Pretty)
import qualified Data.Time                             as Time
import           Data.Traversable                      (for)
import           Data.Typeable                         (Proxy (..), Typeable)
import           Dhall.Optics                          (transformMOf)
import           GHC.Generics                          (Generic)
import           Lens.Family                           (set, (^..))
import           Prelude                               as X hiding (FilePath)
import           Safe                                  (headMay)
import           System.FilePath                       (isAbsolute, pathSeparator, (</>))
import           System.IO                             (hPutStrLn)
import           Turtle                                (ExitCode (..), FilePath, appendonly, chmod,
                                                        executable, mktree, repr, shell,
                                                        shellStrict, shellStrictWithErr,
                                                        systemStrictWithErr, testdir)
import           UnliftIO                              (MonadUnliftIO, withRunInIO)
import           UnliftIO.Directory                    (getModificationTime, makeAbsolute)
import           UnliftIO.Exception                    (IOException, handleAny, try, tryIO)
import           UnliftIO.Process                      (callCommand)
import           UnliftIO.STM                          (atomically, newTVarIO, readTVar, readTVarIO,
                                                        writeTVar)


-- | Generic Error that we throw on program exit.
--   We have it so that errors are displayed nicely to the user
--   (the default Turtle.die is not nice)
newtype SpagoError = SpagoError { _unError :: Text }
instance Exception SpagoError
instance Show SpagoError where
  show (SpagoError err) = Text.unpack err


-- | Flag to disable the automatic use of `psa`
data UsePsa = UsePsa | NoPsa

data GlobalOptions = GlobalOptions
  { globalDebug      :: Bool
  , globalUsePsa     :: UsePsa
  , globalJobs       :: Int
  , globalConfigPath :: Text
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
  hasDebug <- asks globalDebug
  Turtle.when hasDebug $ do
    echo str

die :: MonadThrow m => Text -> m a
die reason = throwM $ SpagoError reason


-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

pathFromText :: Text -> Turtle.FilePath
pathFromText = Turtle.fromText

testfile :: MonadIO m => Text -> m Bool
testfile = Turtle.testfile . pathFromText

readTextFile :: MonadIO m => Turtle.FilePath -> m Text
readTextFile = liftIO . Turtle.readTextFile


writeTextFile :: MonadIO m => Text -> Text -> m ()
writeTextFile path text = liftIO $ Turtle.writeTextFile (Turtle.fromText path) text


with :: MonadIO m => Turtle.Managed a -> (a -> IO r) -> m r
with r f = liftIO $ Turtle.with r f


viewShell :: (MonadIO m, Show a) => Turtle.Shell a -> m ()
viewShell = Turtle.view


surroundQuote :: IsString t => Semigroup t => t -> t
surroundQuote y = "\"" <> y <> "\""


mv :: MonadIO m => System.IO.FilePath -> System.IO.FilePath -> m ()
mv from to = Turtle.mv (Turtle.decodeString from) (Turtle.decodeString to)


cptree :: MonadIO m => System.IO.FilePath -> System.IO.FilePath -> m ()
cptree from to = Turtle.cptree (Turtle.decodeString from) (Turtle.decodeString to)


-- | Like 'when', but where the test can be monadic
whenM :: Monad m => m Bool -> m () -> m ()
whenM b t = b >>= \case
  True  -> t
  False -> pure ()


withTaskGroup' :: Spago m => Int -> (Async.TaskGroup -> m b) -> m b
withTaskGroup' n action = withRunInIO $ \run -> Async.withTaskGroup n (\taskGroup -> run $ action taskGroup)

async' :: Spago m => Async.TaskGroup -> m a -> m (Async.Async a)
async' taskGroup action = withRunInIO $ \run -> Async.async taskGroup (run action)

mapTasks' :: (Spago m, Traversable t) => Async.TaskGroup -> t (m a) -> m (t a)
mapTasks' taskGroup actions = withRunInIO $ \run -> Async.mapTasks taskGroup (run <$> actions)

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


-- | Release tag for the `purescript-docs-search` app.
docsSearchVersion :: Text
docsSearchVersion = "v0.0.5"


githubTokenEnvVar :: IsString t => t
githubTokenEnvVar = "SPAGO_GITHUB_TOKEN"


-- | Check if the file is present and more recent than 1 day
shouldRefreshFile :: Spago m => FilePath.FilePath -> m Bool
shouldRefreshFile path = (tryIO $ liftIO $ do
  fileExists <- testfile $ Text.pack path
  lastModified <- getModificationTime path
  now <- Time.getCurrentTime
  -- Note: `NomiNalDiffTime` is 1 second
  let fileIsRecentEnough = Time.addUTCTime (24 * 60 * 60) lastModified >= now
  pure $ not (fileExists && fileIsRecentEnough))
  >>= \case
  Right v -> pure v
  Left err -> do
    echoDebug $ "Unable to read file " <> Text.pack path <> ". Error was: " <> tshow err
    pure True


-- | Prettyprint a `Pretty` expression
pretty :: Pretty.Pretty a => a -> Text
pretty = PrettyText.renderStrict
  . Pretty.layoutPretty Pretty.defaultLayoutOptions
  . Pretty.pretty
