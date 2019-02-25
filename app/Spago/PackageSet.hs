module Spago.PackageSet
  ( upgradeSpacchetti
  , checkPursIsUpToDate
  , makePackageSetFile
  , freeze
  , path
  , pathText
  , PackageSet
  , Package (..)
  , PackageName (..)
  , Repo (..)
  ) where

import           Control.Exception  (throwIO)
import           Data.Aeson
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map           (Map)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Versions      as Version
import qualified Dhall
import           Dhall.Binary       (defaultStandardVersion)
import qualified Dhall.Format       as Dhall.Format
import qualified Dhall.Freeze
import qualified Dhall.Import
import qualified Dhall.Parser       as Parser
import qualified Dhall.Pretty       as Dhall.Pretty
import qualified Dhall.TypeCheck
import           GHC.Generics       (Generic)
import qualified GitHub
import           Lens.Micro         ((^..))
import           Network.URI        (parseURI)
import qualified Turtle             as T hiding (die, echo)

import qualified Spago.Dhall        as Dhall
import           Spago.Messages     as Messages
import qualified Spago.Purs         as Purs
import qualified Spago.Templates    as Templates
import           Spago.Turtle


-- | Matches the packages definition of Spacchetti Package.dhall/psc-package
newtype PackageName = PackageName { packageName :: Text }
  deriving (Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Dhall.Interpret)

-- | A spacchetti package.
data Package = Package
  { dependencies :: ![PackageName] -- ^ list of dependency package names
  , repo         :: !Repo          -- ^ the remote git repository or the local path
  , version      :: !Text          -- ^ version string (also functions as a git ref)
  }
  deriving (Eq, Show, Generic)

instance ToJSON Package
instance FromJSON Package

type PackageSet = Map PackageName Package

-- | We consider a "Repo" a "box of source to include in the build"
--   This can have different nature:
data Repo
  = Local !Text    -- ^ A local path
  | Remote !Text   -- ^ The address of a remote git repository
  deriving (Eq, Show, Generic)

instance ToJSON Repo
instance FromJSON Repo

instance Dhall.Interpret Repo where
  autoWith _ = makeRepo <$> Dhall.strictText
    where
      -- We consider a "Remote" anything that `parseURI` thinks is a URI
      makeRepo repo = case parseURI $ Text.unpack repo of
        Just _uri -> Remote repo
        Nothing   -> Local repo


pathText :: Text
pathText = "packages.dhall"

path :: T.FilePath
path = T.fromText pathText


-- | Tries to create the `packages.dhall` file. Fails when the file already exists,
--   unless `--force` has been used.
makePackageSetFile :: Bool -> IO ()
makePackageSetFile force = do
  T.unless force $ do
    hasPackagesDhall <- T.testfile path
    T.when hasPackagesDhall $ die $ Messages.foundExistingProject pathText
  T.writeTextFile path Templates.packagesDhall
  Dhall.Format.format Dhall.Pretty.Unicode (Just $ Text.unpack pathText)


-- | Freeze the package-set imports so they can be cached
freeze :: IO ()
freeze = do
  echo Messages.freezePackageSet
  Dhall.Freeze.freeze (Just $ Text.unpack pathText) False defaultStandardVersion


data RawPackageSet = RawPackageSet
  { mkPackage :: !Dhall.Import
  , upstream  :: !Dhall.Import
  -- TODO: add additions and overrides if needed
  } deriving (Show, Generic)


data ReadOnly = ReadOnly | ReadAndWrite


-- | Takes a function that manipulates the Dhall AST of the PackageSet,
--   and tries to run it on the current packages.dhall
--   If it succeeds, it writes back to file the result returned.
--   Note: it will pass in the parsed AST, not the resolved one (so
--   e.g. imports will still be in the tree). If you need the resolved
--   one, use `ensureConfig`.
withPackageSetAST
  :: ReadOnly
  -> (RawPackageSet -> IO RawPackageSet)
  -> IO ()
withPackageSetAST readOnly transform = do
  -- get a workable configuration
  exists <- T.testfile path
  T.unless exists $ makePackageSetFile False
  packageSetText <- T.readTextFile path

  -- parse the config without resolving imports
  (header, expr) <- case Parser.exprAndHeaderFromText mempty packageSetText of
    Left err -> do
      echo $ Messages.failedToReadFile pathText
      throwIO err
    Right (comment, ast) -> case Dhall.denote ast of
      -- remove Note constructors, and match on the `let` shape
      Dhall.Let
        (Dhall.Binding "mkPackage" ann1 (Dhall.Embed mkPackageImport)
          :| (Dhall.Binding "upstream" ann2 (Dhall.Embed upstreamImport)):rest)
        body
        -> do
        -- run the transform on the package set
        RawPackageSet{..} <- transform $ RawPackageSet mkPackageImport upstreamImport
        -- rebuild it
        pure $ (,) comment
          $ Dhall.Let
              (Dhall.Binding "mkPackage" ann1 (Dhall.Embed mkPackage)
                :| (Dhall.Binding "upstream" ann2 (Dhall.Embed upstream)):rest)
              body

      e -> die
        $ Messages.failedToParseFile pathText
        $ Dhall.CannotParsePackageSet e

  -- After modifying the expression, we have to check if it still typechecks
  -- if it doesn't we don't write to file.
  -- We also don't write to file if we are supposed to only read.
  resolvedExpr <- Dhall.Import.load expr
  case (Dhall.TypeCheck.typeOf resolvedExpr, readOnly) of
    (Left err, _)     -> throwIO err
    (_, ReadOnly)     -> pure ()
    (_, ReadAndWrite) -> do
      echo "Done. Updating the local package-set file.."
      T.writeTextFile path
        $ Dhall.prettyWithHeader header expr <> "\n"


-- | Tries to upgrade the Spacchetti release of the local package set.
--   It will:
--   - try to read the latest tag from GitHub
--   - try to read the current package-set file
--   - try to replace the Spacchetti tag to which the package-set imports point to
--     (if they point to the Spacchetti repo. This can be eventually made GitHub generic)
--   - if all of this succeeds, it will regenerate the hashes and write to file
upgradeSpacchetti :: IO ()
upgradeSpacchetti = do
  (GitHub.executeRequest' $ GitHub.latestReleaseR "spacchetti" "spacchetti") >>= \case
    Left err -> die $ Messages.failedToReachGitHub err
    Right GitHub.Release{..} -> do
      echo ("Found the most recent tag for \"spacchetti\": " <> surroundQuote releaseTagName)
      withPackageSetAST ReadAndWrite $ \packagesRaw -> do
        maybePackages <- pure $ do
          newMkPackages <- upgradeImport releaseTagName $ mkPackage packagesRaw
          newUpstream   <- upgradeImport releaseTagName $ upstream packagesRaw
          pure $ RawPackageSet newMkPackages newUpstream
        case maybePackages of
          Left wrongImport ->
            throwIO $ (Dhall.ImportCannotBeUpdated wrongImport :: Dhall.ReadError Dhall.Import)
          -- If everything is fine, refreeze the imports
          Right RawPackageSet{..} -> do
            echo $ Messages.upgradingPackageSet releaseTagName
            frozenMkPackages <- Dhall.Freeze.freezeImport "." defaultStandardVersion mkPackage
            frozenUpstream   <- Dhall.Freeze.freezeImport "." defaultStandardVersion upstream
            pure $ RawPackageSet frozenMkPackages frozenUpstream
  where
    -- | Given an import and a new Spacchetti tag, upgrades the import to
    --   the tag and resets the hash
    upgradeImport :: Text -> Dhall.Import -> Either Dhall.Import Dhall.Import
    upgradeImport newTag Dhall.Import
      { importHashed = Dhall.ImportHashed
        { importType = Dhall.Remote Dhall.URL
          -- Check if we're dealing with the Spacchetti repo
          { authority = "raw.githubusercontent.com"
          , path = Dhall.File
            { directory = Dhall.Directory
              { components = [ "src", _currentTag, "spacchetti", "spacchetti" ]}
            , ..
            }
          , ..
          }
        , ..
        }
      , ..
      } =
      let components = [ "src", newTag, "spacchetti", "spacchetti" ]
          directory = Dhall.Directory{..}
          newPath = Dhall.File{..}
          authority = "raw.githubusercontent.com"
          importType = Dhall.Remote Dhall.URL { path = newPath, ..}
          newHash = Nothing -- Reset the hash here, as we'll refreeze
          importHashed = Dhall.ImportHashed { hash = newHash, ..}
      in Right Dhall.Import{..}
    upgradeImport _ imp = Left imp


-- | Given a Dhall.Import, extract the GitHub tag if the upstream is Spacchetti
getPackageSetTag :: Dhall.Import -> Maybe Text
getPackageSetTag Dhall.Import
  { importHashed = Dhall.ImportHashed
    { importType = Dhall.Remote Dhall.URL
      -- Check if we're dealing with the Spacchetti repo
      { authority = "raw.githubusercontent.com"
      , path = Dhall.File
        { directory = Dhall.Directory
          { components = [ "src", tag, "spacchetti", "spacchetti" ]}
        , ..
        }
      , ..
      }
    , ..
    }
  , ..
  } = Just tag
getPackageSetTag _ = Nothing


checkPursIsUpToDate :: IO ()
checkPursIsUpToDate = do
  withPackageSetAST ReadOnly $ \packageSet@RawPackageSet{..} -> do
    -- Let's talk backwards-compatibility.
    -- At some point we switched Spacchetti from tagging the PackageSet with
    -- something like '20180923'  to something like '0.12.2-20190209' instead
    -- (in order to support this check).
    -- Now, if people are still using the old tag, we should:
    -- - warn them to upgrade with `spacchetti-upgrade`
    -- - skip this check
    case fmap (Text.split (=='-')) (getPackageSetTag upstream) of
      Just [minPursVersion, _packageSetVersion] -> do
        maybeCompilerVersion <- Purs.version
        case (maybeCompilerVersion, hush $ Version.semver minPursVersion) of
          (Just compilerVersion, Just pursVersionFromPackageSet) -> do
            performCheck compilerVersion pursVersionFromPackageSet
          _ -> echo "WARNING: unable to parse versions, skipping check.."
      _ -> echo Messages.packageSetVersionWarning

    -- We have to return a RawPackageSet, unmodified.
    -- TODO: refactor so we don't have to return it
    pure packageSet

    where
      -- | The check is successful only when the installed compiler is "slightly"
      --   greater (or equal of course) to the minimum version. E.g. fine cases are:
      --   - current is 0.12.2 and package-set is on 0.12.1
      --   - current is 1.4.3 and package-set is on 1.3.4
      --   Not fine cases are e.g.:
      --   - current is 0.1.2 and package-set is 0.2.3
      --   - current is 1.2.3 and package-set is 1.3.4
      --   - current is 1.2.3 and package-set is 0.2.3
      performCheck :: Version.SemVer -> Version.SemVer -> IO ()
      performCheck actualPursVersion minPursVersion = do
        let versionList semver = semver ^.. (Version.major <> Version.minor <> Version.patch)
        case (versionList actualPursVersion, versionList minPursVersion) of
          ([0, b, c], [0, y, z]) | b == y && c >= z -> pure ()
          ([a, b, _c], [x, y, _z]) | a /= 0 && a == x && b >= y -> pure ()
          _ -> die $ Messages.pursVersionMismatch
                 (Version.prettySemVer actualPursVersion)
                 (Version.prettySemVer minPursVersion)
