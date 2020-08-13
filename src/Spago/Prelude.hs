module Spago.Prelude
  ( 
  -- * Basic exports
    module X
  , Proxy(..)
  , NonEmpty (..)
  , Seq (..)
  , Validation(..)
  , bimap
  , first
  , second
  , headMay
  , lastMay
  , empty
  
  -- * Logging, errors, printing, etc
  , Pretty
  , pretty
  , output
  , outputStr
  , die
  , hush
  , surroundQuote

  -- * Lens
  , (</>)
  , (^..)
  , transformMOf

  -- * Files and directories
  , FilePath
  , pathFromText
  , testfile
  , testdir
  , mktree
  , mv
  , cptree
  , assertDirectory
  , chmod
  , executable
  , readTextFile
  , writeTextFile
  , isAbsolute
  , pathSeparator
  , shouldRefreshFile
  , makeAbsolute
  , getModificationTime
  , Turtle.mktempdir

  -- * Running commands
  , ExitCode (..)
  , callCommand
  , shell
  , shellStrict
  , shellStrictWithErr
  , systemStrictWithErr
  , viewShell
  , findExecutableOrDie
  , Directory.findExecutable

  -- * Other
  , Dhall.Core.throws
  , repr
  , with
  , appendonly
  , docsSearchVersion
  , githubTokenEnvVar
  ) where


import           Control.Monad.Catch                   (MonadMask)
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
import           Data.Text.IO.Utf8                     (readFile, writeFile)
import           Dhall.Optics                          (transformMOf)
import           Lens.Family                           ((^..))
import           RIO                                   as X hiding (FilePath, first, force, second)
import           RIO.Orphans                           as X
import           Safe                                  (headMay, lastMay)
import           System.FilePath                       (isAbsolute, pathSeparator, (</>))
import           Turtle                                (ExitCode (..), FilePath, appendonly, chmod,
                                                        executable, mktree, repr, shell,
                                                        shellStrict, shellStrictWithErr,
                                                        systemStrictWithErr, testdir)
import           UnliftIO.Directory                    (getModificationTime, makeAbsolute)
import           UnliftIO.Process                      (callCommand)


-- | Generic Error that we throw on program exit.
--   We have it so that errors are displayed nicely to the user
newtype SpagoError = SpagoError { _unError :: Text }
instance Exception SpagoError
instance Show SpagoError where
  show (SpagoError err) = Text.unpack err


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
readTextFile = readFile . Turtle.encodeString


writeTextFile :: (MonadIO m, MonadMask m) => Text -> Text -> m ()
writeTextFile path text = writeFile (Text.unpack path) text


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


-- | Code from: https://github.com/dhall-lang/dhall-haskell/blob/d8f2787745bb9567a4542973f15e807323de4a1a/dhall/src/Dhall/Import.hs#L578
assertDirectory :: (MonadIO m, HasLogFunc env, MonadReader env m) => FilePath.FilePath -> m ()
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
      logDebug $ "Directory " <> displayShow directory <> " does not exist, creating..."
      assertDirectory (FilePath.takeDirectory directory)

      Directory.createDirectory directory

      Directory.setPermissions directory private


-- | Release tag for the `purescript-docs-search` app.
docsSearchVersion :: Text
docsSearchVersion = "v0.0.10"


githubTokenEnvVar :: IsString t => t
githubTokenEnvVar = "SPAGO_GITHUB_TOKEN"


-- | Check if the file is present and more recent than 1 day
shouldRefreshFile :: HasLogFunc env => FilePath.FilePath -> RIO env Bool
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

-- | Return the full path of the executable we're trying to call,
--   or die trying
findExecutableOrDie :: HasLogFunc env => String -> RIO env Text
findExecutableOrDie cmd = do
  Directory.findExecutable cmd >>= \case
    Nothing -> die [ "Executable was not found in path: " <> displayShow cmd ]
    -- Note: we ignore the path and just return the input because the one we get
    -- here is absolute, and Windows doesn't seem to be able to deal with that.
    -- See: https://github.com/purescript/spago/issues/635
    Just _path -> pure $ Text.pack cmd
