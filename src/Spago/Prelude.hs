module Spago.Prelude
  ( die
  , Dhall.Core.throws
  , hush
  , pathFromText
  , assertDirectory
  , Env
  , HasEnv(..)
  , HasGlobalCache(..)
  , HasConfigPath(..)
  , HasJobs(..)
  , HasPsa(..)
  , UsePsa(..)
  , Spago
  , module X
  , Proxy(..)
  , NonEmpty (..)
  , Seq (..)
  , Pretty
  , FilePath
  , ExitCode (..)
  , Validation(..)
  , (</>)
  , (^..)
  , surroundQuote
  , transformMOf
  , testfile
  , testdir
  , mktree
  , mv
  , cptree
  , bimap
  , first
  , second
  , chmod
  , executable
  , readTextFile
  , writeTextFile
  , isAbsolute
  , pathSeparator
  , headMay
  , lastMay
  , shouldRefreshFile
  , makeAbsolute
  , hPutStrLn
  , empty
  , callCommand
  , shell
  , shellStrict
  , shellStrictWithErr
  , systemStrictWithErr
  , viewShell
  , repr
  , with
  , appendonly
  , async'
  , wait'
  , cancel'
  , waitCatch'
  , mapTasks'
  , withTaskGroup'
  , Turtle.mktempdir
  , getModificationTime
  , docsSearchVersion
  , githubTokenEnvVar
  , pretty
  , output
  , outputStr
  ) where


import qualified Control.Concurrent.Async.Pool         as Async
import qualified Data.Text                             as Text
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as PrettyText
import qualified Data.Time                             as Time
import           Dhall                                 (Text)
import qualified Dhall.Core
import qualified System.FilePath                       as FilePath
import qualified System.IO
import qualified Turtle
import qualified UnliftIO.Directory                    as Directory

import           Control.Applicative                   (empty)
import           Control.Monad                         as X
import           Control.Monad.Reader                  as X
import           Data.Aeson                            as X hiding (Result (..))
import           Data.Bifunctor                        (bimap, first, second)
import           Data.Bool                             as X
import           Data.Either                           as X
import           Data.Either.Validation                (Validation (..))
import           Data.Foldable                         as X
import           Data.List.NonEmpty                    (NonEmpty (..))
import           Data.Maybe                            as X
import           Data.Sequence                         (Seq (..))
import           Data.Text.Prettyprint.Doc             (Pretty)
import           Dhall.Optics                          (transformMOf)
import           Lens.Family                           ((^..))
import           RIO                                   as X hiding (FilePath, first, force, second)
import           RIO.Orphans                           as X
import           Safe                                  (headMay, lastMay)
import           System.FilePath                       (isAbsolute, pathSeparator, (</>))
import           System.IO                             (hPutStrLn)
import           Turtle                                (ExitCode (..), FilePath, appendonly, chmod,
                                                        executable, mktree, repr, shell,
                                                        shellStrict, shellStrictWithErr,
                                                        systemStrictWithErr, testdir)
import           UnliftIO.Directory                    (getModificationTime, makeAbsolute)
import           UnliftIO.Process                      (callCommand)

import           Spago.Env


-- | Generic Error that we throw on program exit.
--   We have it so that errors are displayed nicely to the user
newtype SpagoError = SpagoError { _unError :: Text }
instance Exception SpagoError
instance Show SpagoError where
  show (SpagoError err) = Text.unpack err


type Spago = RIO Env

output :: MonadIO m => Text -> m ()
output = Turtle.printf (Turtle.s Turtle.% "\n")

outputStr :: MonadIO m => String -> m ()
outputStr = output . Text.pack

die :: (MonadIO m, HasLogFunc env, MonadReader env m) => [Utf8Builder] -> m a
die reasons = do
  traverse_ logError reasons
  exitFailure

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
mv from to' = Turtle.mv (Turtle.decodeString from) (Turtle.decodeString to')


cptree :: MonadIO m => System.IO.FilePath -> System.IO.FilePath -> m ()
cptree from to' = Turtle.cptree (Turtle.decodeString from) (Turtle.decodeString to')


withTaskGroup' :: (MonadIO m, MonadReader env m, HasLogFunc env, MonadUnliftIO m) => Int -> (Async.TaskGroup -> m b) -> m b
withTaskGroup' n action = withRunInIO $ \run -> Async.withTaskGroup n (\taskGroup -> run $ action taskGroup)

async' :: (MonadIO m, MonadReader env m, HasLogFunc env, MonadUnliftIO m) => Async.TaskGroup -> m a -> m (Async.Async a)
async' taskGroup action = withRunInIO $ \run -> Async.async taskGroup (run action)

wait' :: MonadIO m => Async.Async a -> m a
wait' = liftIO . Async.wait

cancel' :: MonadIO m => Async.Async a -> m ()
cancel' = liftIO . Async.cancel

waitCatch' :: MonadIO m => Async.Async a -> m (Either SomeException a)
waitCatch' = liftIO . Async.waitCatch

mapTasks' :: Traversable t => Async.TaskGroup -> t (Spago a) -> Spago (t a)
mapTasks' taskGroup actions = withRunInIO $ \run -> Async.mapTasks taskGroup (run <$> actions)

-- | Code from: https://github.com/dhall-lang/dhall-haskell/blob/d8f2787745bb9567a4542973f15e807323de4a1a/dhall/src/Dhall/Import.hs#L578
assertDirectory :: (MonadIO m, MonadThrow m, HasLogFunc env, MonadReader env m) => FilePath.FilePath -> m ()
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
        die [ "Directory " <> displayShow directory <> " is not accessible. " <> displayShow permissions ]
    else do
      assertDirectory (FilePath.takeDirectory directory)

      Directory.createDirectory directory

      Directory.setPermissions directory private


-- | Release tag for the `purescript-docs-search` app.
docsSearchVersion :: Text
docsSearchVersion = "v0.0.7"


githubTokenEnvVar :: IsString t => t
githubTokenEnvVar = "SPAGO_GITHUB_TOKEN"


-- | Check if the file is present and more recent than 1 day
shouldRefreshFile :: FilePath.FilePath -> Spago Bool
shouldRefreshFile path = (tryIO $ liftIO $ do
  fileExists <- testfile $ Text.pack path
  lastModified <- getModificationTime path
  now <- Time.getCurrentTime
  let fileIsRecentEnough = Time.addUTCTime Time.nominalDay lastModified >= now
  pure $ not (fileExists && fileIsRecentEnough)) >>= \case
    Right v -> pure v
    Left err -> do
      logDebug $ "Unable to read file " <> displayShow path <> ". Error was: " <> display err
      pure True


-- | Prettyprint a `Pretty` expression
pretty :: Pretty.Pretty a => a -> Dhall.Text
pretty = PrettyText.renderStrict
  . Pretty.layoutPretty Pretty.defaultLayoutOptions
  . Pretty.pretty
