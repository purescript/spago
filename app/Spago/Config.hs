module Spago.Config
  ( makeConfig
  , ensureConfig
  , addDependencies
  , upgradeSpacchetti
  , checkPursIsUpToDate
  , Config(..)
  ) where

import           Control.Exception         (throwIO, try)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Aeson                as JSON
import           Data.Either               (lefts, rights)
import           Data.Foldable             (toList)
import qualified Data.List                 as List
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.Map                  as Map
import           Data.Maybe                (mapMaybe)
import qualified Data.Sequence             as Seq
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Text.Prettyprint.Doc (Pretty)
import           Data.Typeable             (Typeable)
import qualified Data.Versions             as Version
import           Dhall.Binary              (defaultStandardVersion)
import qualified Dhall.Format
import qualified Dhall.Freeze
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Parser              as Parser
import qualified Dhall.Pretty
import           Dhall.TypeCheck           (X)
import qualified Dhall.TypeCheck
import           GHC.Generics              (Generic)
import qualified GitHub
import           Lens.Micro                ((^..))
import qualified Turtle                    as T hiding (die, echo)

import qualified PscPackage                as PscPackage
import qualified PscPackage.Types          as PscPackage
import qualified Spago.Config.Dhall        as Dhall
import           Spago.Spacchetti          (Package, PackageName (..), PackageSet)
import qualified Spago.Templates           as Templates
import           Spago.Turtle


pathText :: Text
pathText = "spago.dhall"

-- | Path for the Spago Config
path :: T.FilePath
path = T.fromText pathText


-- | Spago configuration file type
data Config = Config
  { name         :: Text
  , dependencies :: [PackageName]
  , packages     :: PackageSet
  } deriving (Show, Generic)

instance ToJSON Config
instance FromJSON Config


-- | Type to represent the "raw" configuration,
--   which is a configuration which has been parsed from Dhall,
--   but not yet resolved (this is used to manipulate the AST directly)
--
--   Note: not all the values from the configuration are included here.
--
--   Note: this limits the amount of stuff that one can do in Dhall inside
--   the configuration. E.g. you won't be able to have a dependency that
--   is not a string in the list of dependencies of the project.
data RawConfig = RawConfig
  { rawName :: Text
  , rawDeps :: [PackageName]
  -- TODO: add packages if needed
  } deriving (Show, Generic)

data RawPackageSet = RawPackageSet
  { mkPackage :: Dhall.Import
  , upstream  :: Dhall.Import
  -- TODO: add additions and overrides if needed
  } deriving (Show, Generic)


data ReadOnly = ReadOnly | ReadAndWrite


-- | Tries to read in a Spago Config
parseConfig :: Text -> IO Config
parseConfig dhallText = do
  expr <- Dhall.inputExpr dhallText
  case expr of
    Dhall.RecordLit ks -> do
      maybeConfig <- pure $ do
        let packageTyp      = Dhall.genericAuto :: Dhall.Type Package
            packageNamesTyp = Dhall.list (Dhall.auto :: Dhall.Type PackageName)
        name         <- Dhall.requireTypedKey ks "name" Dhall.strictText
        dependencies <- Dhall.requireTypedKey ks "dependencies" packageNamesTyp
        packages     <- Dhall.requireKey ks "packages" $ \case
          Dhall.RecordLit pkgs -> (Map.mapKeys PackageName . Dhall.Map.toMap)
            <$> traverse (Dhall.coerceToType packageTyp) pkgs
          something -> Left $ Dhall.PackagesIsNotRecord something

        Right $ Config{..}

      case maybeConfig of
        Right config -> pure config
        Left err     -> throwIO err
    _ -> case Dhall.TypeCheck.typeOf expr of
      Right e  -> throwIO $ Dhall.ConfigIsNotRecord e
      Left err -> throwIO $ err


-- | Checks that the Spago config is there and readable
ensureConfig :: IO Config
ensureConfig = do
  exists <- T.testfile path
  T.unless exists $ makeConfig False
  configText <- T.readTextFile path
  try (parseConfig configText) >>= \case
    Right config -> pure config
    Left (err :: Dhall.ReadError X) -> throwIO err


-- | Copies over `spago.dhall` to set up a Spago project.
--   Eventually ports an existing `psc-package.json` to the new config.
makeConfig :: Bool -> IO ()
makeConfig force = do
  T.unless force $ do
    hasSpagoDhall <- T.testfile path
    T.when hasSpagoDhall $ die
       $ "Found " <> pathText <> ": there's already a project here. "
      <> "Run `spago init --force` if you're sure you want to overwrite it."
  T.touch path
  T.writeTextFile path Templates.spagoDhall
  Dhall.Format.format Dhall.Pretty.Unicode (Just $ Text.unpack pathText)

  -- We try to find an existing psc-package config, and we migrate the existing
  -- content if we found one, otherwise we copy the default template
  pscfileExists <- T.testfile PscPackage.configPath
  T.when pscfileExists $ do
    -- first, read the psc-package file content
    content <- T.readTextFile PscPackage.configPath
    case JSON.eitherDecodeStrict $ Text.encodeUtf8 content of
      Left _err -> do
        echo ( "Warning: found a \"psc-package.json\" file, "
               <> "but was not able to read it, skipping the conversion..")
        echoStr $ show _err
      Right pscConfig -> do
        echo "Found a \"psc-package.json\" file, migrating to a new Spago config.."
        -- update the project name
        withConfigAST $ \config -> config { rawName = PscPackage.name pscConfig }
        -- try to update the dependencies (will fail if not found in package set)
        let pscPackages = map PackageName $ PscPackage.depends pscConfig
        config <- ensureConfig
        addDependencies config pscPackages


-- | Takes a function that manipulates the Dhall AST of the Config,
--   and tries to run it on the current config.
--   If it succeeds, it writes back to file the result returned.
--   Note: it will pass in the parsed AST, not the resolved one (so
--   e.g. imports will still be in the tree). If you need the resolved
--   one, use `ensureConfig`.
withConfigAST :: (RawConfig -> RawConfig) -> IO ()
withConfigAST transform = do
  -- get a workable configuration
  exists <- T.testfile path
  T.unless exists $ makeConfig False
  configText <- T.readTextFile path

  -- parse the config without resolving imports
  (header, expr) <- case Parser.exprAndHeaderFromText mempty configText of
    Left  err -> throwIO err
    Right (header, ast) -> case Dhall.denote ast of
      -- remove Note constructors, and check if config is a record
      Dhall.RecordLit ks -> do
        rawConfig <- pure $ do
          currentName <- Dhall.requireKey ks "name" Dhall.fromTextLit
          currentDeps <- Dhall.requireKey ks "dependencies" toPkgsList
          Right $ RawConfig currentName currentDeps

        -- apply the transformation if config is valid
        RawConfig{..} <- case rawConfig of
          Right conf -> pure $ transform conf
          Left err -> do
            echo ("Error while trying to parse "
                  <> surroundQuote pathText
                  <> ". Details:\n")
            throwIO $ err

        -- return the new AST from the new config
        let
          mkNewAST "name"         _ = Dhall.toTextLit rawName
          mkNewAST "dependencies" _ = Dhall.ListLit Nothing
            $ Seq.fromList
            $ fmap Dhall.toTextLit
            $ fmap packageName rawDeps
          mkNewAST _ v = v
        pure (header, Dhall.RecordLit $ Dhall.Map.mapWithKey mkNewAST ks)

      e -> throwIO $ Dhall.ConfigIsNotRecord e

  -- After modifying the expression, we have to check if it still typechecks
  -- if it doesn't we don't write to file
  resolvedExpr <- Dhall.Import.load expr
  case Dhall.TypeCheck.typeOf resolvedExpr of
    Left  err -> throwIO err
    Right _   -> T.writeTextFile path $ Dhall.prettyWithHeader header expr <> "\n"

  where
    toPkgsList
      :: (Pretty a, Typeable a)
      => Dhall.Expr Parser.Src a
      -> Either (Dhall.ReadError a) [PackageName]
    toPkgsList (Dhall.ListLit _ pkgs) =
      let
        texts = fmap Dhall.fromTextLit $ toList pkgs
      in
      case (lefts texts) of
        []  -> Right $ fmap PackageName $ rights texts
        e:_ -> Left e
    toPkgsList e = Left $ Dhall.DependenciesIsNotList e


addRawDeps :: [PackageName] -> RawConfig -> RawConfig
addRawDeps newDeps config = config
  { rawDeps = List.sort $ List.nub (newDeps <> (rawDeps config)) }

-- | Try to add the `newPackages` to the "dependencies" list in the Config.
--   It will not add any dependency if any of them is not in the package set.
--   If everything is fine instead, it will add the new deps, sort all the
--   dependencies, and write the Config back to file.
addDependencies :: Config -> [PackageName] -> IO ()
addDependencies config newPackages = do
  let notInPackageSet = mapMaybe
        (\p -> case Map.lookup p (packages config) of
                Just _  -> Nothing
                Nothing -> Just p)
        newPackages
  case notInPackageSet of
    -- If none of the newPackages are outside of the set, add them to existing dependencies
    []   -> withConfigAST $ addRawDeps newPackages
    pkgs -> echo $ "\nSome of the dependencies you tried to add "
                <> "were not found in spacchetti's package set.\n"
                <> "Not adding new dependencies to your new spago config. "
                <> "We didn't find:\n"
                <> (Text.intercalate "\n" $ map (\p -> "- " <> packageName p) pkgs)
                <> "\n"


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
  exists <- T.testfile PscPackage.packagesDhallPath
  T.unless exists $ PscPackage.makePackagesDhall False "" -- TODO pass command here?
  packageSetText <- T.readTextFile PscPackage.packagesDhallPath

  -- parse the config without resolving imports
  (header, expr) <- case Parser.exprAndHeaderFromText mempty packageSetText of
    Left err -> do
      echo $ "Failed to read " <> surroundQuote PscPackage.packagesDhallText
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

      e -> do
        echo $ "Error while trying to parse "
          <> surroundQuote PscPackage.packagesDhallText
          <> ". Details:\n"
        throwIO $ Dhall.CannotParsePackageSet e

  -- After modifying the expression, we have to check if it still typechecks
  -- if it doesn't we don't write to file.
  -- We also don't write to file if we are supposed to only read.
  resolvedExpr <- Dhall.Import.load expr
  case (Dhall.TypeCheck.typeOf resolvedExpr, readOnly) of
    (Left err, _)     -> throwIO err
    (_, ReadOnly)     -> pure ()
    (_, ReadAndWrite) -> do
      echo "Done. Updating the local package-set file.."
      T.writeTextFile PscPackage.packagesDhallPath
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
    Left err -> do
      echo "Could not reach GitHub. Error:\n"
      throwIO err
    Right GitHub.Release{..} -> do
      echo ("Found the most recent tag for \"spacchetti\": " <> releaseTagName)
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
            echo $ "Package-set upgraded to latest tag "
              <> surroundQuote releaseTagName
              <> "\nFetching the new one and generating hashes.. (this might take some time)"
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


-- | Given a Dhall.Import, extract the spacchetti tag if
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

-- | Suppress the 'Left' value of an 'Either'
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

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
        let Just pursVersionFromPackageSet = hush $ Version.semver minPursVersion
        compilerVersion <- readPursVersion
        performCheck compilerVersion pursVersionFromPackageSet
      _ -> echo
          $ "WARNING: the package-set version you're on doesn't check if the version of the\n"
          <> "PureScript compiler installed on your system is compatible.\n"
          <> "If your build fails you might want to upgrade your set by running this command:\n"
          <> "`spago spacchetti-upgrade`\n\n"

    -- We have to return a RawPackageSet, unmodified.
    -- TODO: refactor so we don't have to return it
    pure packageSet

    where
      -- | The chech is successful only when the installed compiler is "slightly"
      --   greater (or equal of course) to the minimum version. E.g. fine cases are:
      --   - current is 0.12.2 and package-set is on 0.12.1
      --   - current is 1.2.3 and package-set is on 1.3.4
      --   Not fine cases are e.g.:
      --   - current is 0.1.2 and package-set is 0.2.3
      --   - current is 1.2.3 and package-set is 0.2.3
      performCheck :: Version.SemVer -> Version.SemVer -> IO ()
      performCheck actualPursVersion minPursVersion = do
        let versionList semver = semver ^.. (Version.major <> Version.minor <> Version.patch)
        case (versionList actualPursVersion, versionList minPursVersion) of
          ([0, b, c], [0, y, z]) | b == y && c >= z -> pure ()
          ([a, b, _c], [x, y, _z]) | a /= 0 && a == x && b >= y -> pure ()
          _ -> die
            $ "Oh noes! It looks like the PureScript version installed on your system is\n"
            <> "outdated for the package-set you're using.\n"
            <> "\ninstalled version:   " <> Version.prettySemVer actualPursVersion
            <> "\npackage-set version: " <> Version.prettySemVer minPursVersion
            <> "\n\nPlease upgrade your `purs` version.\n\n"

      readPursVersion :: IO Version.SemVer
      readPursVersion = do
        versionText <- T.shellStrict "purs --version" T.empty >>= \case
          (T.ExitSuccess, out) -> pure out
          _ -> die "Failed to run 'purs --version'"
        case Version.semver versionText of
          Right parsed -> pure parsed
          Left _ -> die $ "Failed to parse 'purs --version' output: " <> surroundQuote versionText
