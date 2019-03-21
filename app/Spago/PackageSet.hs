module Spago.PackageSet
  ( upgradePackageSet
  , checkPursIsUpToDate
  , makePackageSetFile
  , freeze
  , ensureFrozen
  , path
  , pathText
  , PackageSet
  , Package (..)
  , PackageName (..)
  , Repo (..)
  ) where

import           Spago.Prelude

import qualified Data.Text       as Text
import qualified Data.Versions   as Version
import qualified Dhall
import           Dhall.Binary    (defaultStandardVersion)
import qualified Dhall.Freeze
import qualified Dhall.Import
import qualified Dhall.Parser    as Parser
import qualified Dhall.TypeCheck
import qualified GitHub
import           Network.URI     (parseURI)

import qualified Spago.Dhall     as Dhall
import           Spago.Messages  as Messages
import qualified Spago.Purs      as Purs
import qualified Spago.Templates as Templates


newtype PackageName = PackageName { packageName :: Text }
  deriving (Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Dhall.Interpret)

-- | A package-set package.
--   Matches the packages definition in Package.dhall from package-sets
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

path :: FilePath
path = pathFromText pathText


-- | Tries to create the `packages.dhall` file. Fails when the file already exists,
--   unless `--force` has been used.
makePackageSetFile :: Bool -> IO ()
makePackageSetFile force = do
  unless force $ do
    hasPackagesDhall <- testfile path
    when hasPackagesDhall $ die $ Messages.foundExistingProject pathText
  writeTextFile path Templates.packagesDhall
  Dhall.format pathText


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
  :: Spago m
  => ReadOnly
  -> (RawPackageSet -> m RawPackageSet)
  -> m ()
withPackageSetAST readOnly transform = do
  -- get a workable configuration
  exists <- testfile path
  unless exists $ liftIO $ makePackageSetFile False
  packageSetText <- readTextFile path

  -- parse the config without resolving imports
  (header, expr) <- case Parser.exprAndHeaderFromText mempty packageSetText of
    Left err -> do
      echo $ Messages.failedToReadFile pathText
      throwM err
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
  resolvedExpr <- liftIO $ Dhall.Import.load expr
  case (Dhall.TypeCheck.typeOf resolvedExpr, readOnly) of
    (Left err, _)     -> throwM err
    (_, ReadOnly)     -> pure ()
    (_, ReadAndWrite) -> do
      echo "Done. Updating the local package-set file.."
      writeTextFile path $ Dhall.prettyWithHeader header expr <> "\n"
      liftIO $ Dhall.format pathText


-- | Tries to upgrade the Package-Sets release of the local package set.
--   It will:
--   - try to read the latest tag from GitHub
--   - try to read the current package-set file
--   - try to replace the git tag to which the package-set imports point to
--     (if they point to the Package-Sets repo. This can be eventually made GitHub generic)
--   - if all of this succeeds, it will regenerate the hashes and write to file
upgradePackageSet :: Spago m => m ()
upgradePackageSet = do
  result <- liftIO $ GitHub.executeRequest' $ GitHub.latestReleaseR "purescript" "package-sets"
  case result of
    Left err -> die $ Messages.failedToReachGitHub err
    Right GitHub.Release{..} -> do
      echo ("Found the most recent tag for \"purescript/package-sets\": " <> surroundQuote releaseTagName)
      withPackageSetAST ReadAndWrite $ \packagesRaw -> do
        maybePackages <- pure $ do
          newMkPackages <- upgradeImport releaseTagName $ mkPackage packagesRaw
          newUpstream   <- upgradeImport releaseTagName $ upstream packagesRaw
          pure $ RawPackageSet newMkPackages newUpstream
        case maybePackages of
          Left wrongImport ->
            throwM $ (Dhall.ImportCannotBeUpdated wrongImport :: Dhall.ReadError Dhall.Import)
          -- If everything is fine, refreeze the imports
          Right RawPackageSet{..} -> liftIO $ do
            echo $ Messages.upgradingPackageSet releaseTagName
            frozenMkPackages <- Dhall.Freeze.freezeImport "." defaultStandardVersion mkPackage
            frozenUpstream   <- Dhall.Freeze.freezeImport "." defaultStandardVersion upstream
            pure $ RawPackageSet frozenMkPackages frozenUpstream
  where
    -- | Given an import and a new purescript/package-sets tag,
    --   upgrades the import to the tag and resets the hash
    upgradeImport :: Text -> Dhall.Import -> Either Dhall.Import Dhall.Import
    upgradeImport newTag imp@Dhall.Import
      { importHashed = Dhall.ImportHashed
        { importType = Dhall.Remote Dhall.URL
          -- Check if we're dealing with the right repo
          { authority = "raw.githubusercontent.com"
          , path = Dhall.File
            { directory = Dhall.Directory
              { components = [ "src", _currentTag, ghRepo, ghOrg ]}
            , ..
            }
          , ..
          }
        , ..
        }
      , ..
      } =
      let components = [ "src", newTag, "package-sets", "purescript" ]
          directory = Dhall.Directory{..}
          newPath = Dhall.File{..}
          authority = "raw.githubusercontent.com"
          importType = Dhall.Remote Dhall.URL { path = newPath, ..}
          newHash = Nothing -- Reset the hash here, as we'll refreeze
          importHashed = Dhall.ImportHashed { hash = newHash, ..}
          newImport = Dhall.Import{..}
      in case (ghOrg, ghRepo) of
        ("spacchetti", "spacchetti")   -> Right newImport
        ("purescript", "package-sets") -> Right newImport
        _                              -> Left imp
    upgradeImport _ imp = Left imp


-- | Given a Dhall.Import, extract the GitHub tag if the upstream is
--   purescript/package-sets or spacchetti
getPackageSetTag :: Dhall.Import -> Maybe Text
getPackageSetTag Dhall.Import
  { importHashed = Dhall.ImportHashed
    { importType = Dhall.Remote Dhall.URL
      -- Check if we're dealing with the right repo
      { authority = "raw.githubusercontent.com"
      , path = Dhall.File
        { directory = Dhall.Directory
          { components = [ "src", tag, ghRepo, ghOrg ]}
        , ..
        }
      , ..
      }
    , ..
    }
  , ..
  } = case (ghOrg, ghRepo) of
        ("spacchetti", "spacchetti")   -> Just tag
        ("purescript", "package-sets") -> Just tag
        _                              -> Nothing
getPackageSetTag _ = Nothing


checkPursIsUpToDate :: Spago m => m ()
checkPursIsUpToDate = do
  withPackageSetAST ReadOnly $ \packageSet@RawPackageSet{..} -> do
    -- Let's talk backwards-compatibility.
    -- At some point we switched Spacchetti from tagging the PackageSet with
    -- something like '20180923'  to something like '0.12.2-20190209' instead
    -- (in order to support this check).
    -- Now, if people are still using the old tag, we should:
    -- - warn them to upgrade with `package-set-upgrade`
    -- - skip this check
    --
    -- Update 2019-02-28: we switched from Spacchetti to package-sets, which
    -- uses a different versioning (with "psc-" in front). We just strip it away
    case fmap (Text.split (=='-') . Text.replace "psc-" "") (getPackageSetTag upstream) of
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
      performCheck :: Spago m => Version.SemVer -> Version.SemVer -> m ()
      performCheck actualPursVersion minPursVersion = do
        let versionList semver = semver ^.. (Version.major <> Version.minor <> Version.patch)
        case (versionList actualPursVersion, versionList minPursVersion) of
          ([0, b, c], [0, y, z]) | b == y && c >= z -> pure ()
          ([a, b, _c], [x, y, _z]) | a /= 0 && a == x && b >= y -> pure ()
          _ -> die $ Messages.pursVersionMismatch
                 (Version.prettySemVer actualPursVersion)
                 (Version.prettySemVer minPursVersion)


-- | Given a Dhall.Import, tell if it's frozen
isFrozen :: Dhall.Import -> Bool
isFrozen Dhall.Import
  { importHashed = Dhall.ImportHashed
    { hash = Just _someHash
    , ..
    }
  , ..
  }        = True
isFrozen _ = False


-- | Freeze the package-set imports so they can be cached
freeze :: Spago m => m ()
freeze = do
  echo Messages.freezePackageSet
  liftIO $ Dhall.Freeze.freeze (Just $ Text.unpack pathText) False defaultStandardVersion


-- | Check that the package-set import is correctly setup with hashes, and freeze it if not
ensureFrozen :: Spago m => m ()
ensureFrozen = do
  withPackageSetAST ReadOnly $ \packageSet@RawPackageSet{..} -> do
    case isFrozen upstream of
      True  -> pure ()
      False -> freeze

    pure packageSet
