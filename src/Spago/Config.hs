{-# LANGUAGE ViewPatterns #-}
module Spago.Config
  ( makeConfig
  , ensureConfig
  , addDependencies
  , parsePackage
  , Config(..)
  , PublishConfig(..)
  ) where

import           Spago.Prelude

import qualified Data.Map              as Map
import qualified Data.SemVer           as SemVer
import qualified Data.Sequence         as Seq
import qualified Data.Set              as Set
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import qualified Data.Versions         as Version
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.TypeCheck
import qualified Web.Bower.PackageMeta as Bower

import qualified Spago.Dhall           as Dhall
import qualified Spago.Messages        as Messages
import qualified Spago.PackageSet      as PackageSet
import qualified Spago.PscPackage      as PscPackage
import qualified Spago.Purs            as Purs
import qualified Spago.Templates       as Templates

import           Spago.PackageSet      (Package (..), PackageLocation (..), PackageName (..),
                                        PackageSet (..))


-- | Path for the Spago Config
path :: IsString t => t
path = "spago.dhall"


-- | Spago configuration file type
data Config = Config
  { name              :: Text
  , dependencies      :: [PackageName]
  , packageSet        :: PackageSet
  , configSourcePaths :: [Purs.SourcePath]
  , publishConfig     :: Either (Dhall.ReadError Dhall.TypeCheck.X) PublishConfig
  } deriving (Show, Generic)

-- | The extra fields that are only needed for publishing libraries.
data PublishConfig = PublishConfig
  { publishLicense    :: Text
  , publishRepository :: Text
  } deriving (Show, Generic)

type Expr = Dhall.DhallExpr Dhall.Import
type ResolvedExpr = Dhall.DhallExpr Dhall.TypeCheck.X


isLocationType :: (Eq s, Eq a) => Dhall.Expr s a -> Bool
isLocationType (Dhall.Union kvs) | locationUnionMap == Dhall.Map.toMap kvs = True
  where
    locationUnionMap = Map.fromList
      [ ("Environment", Just Dhall.Text)
      , ("Remote", Just Dhall.Text)
      , ("Local", Just Dhall.Text)
      , ("Missing", Nothing)
      ]
isLocationType _ = False


dependenciesType :: Dhall.Type [PackageName]
dependenciesType = Dhall.list (Dhall.auto :: Dhall.Type PackageName)


parsePackage :: MonadIO m => MonadThrow m => ResolvedExpr -> m Package
parsePackage (Dhall.RecordLit ks) = do
  repo         <- Dhall.requireTypedKey ks "repo" (Dhall.auto :: Dhall.Type PackageSet.Repo)
  version      <- Dhall.requireTypedKey ks "version" Dhall.strictText
  dependencies <- Dhall.requireTypedKey ks "dependencies" dependenciesType
  let location = PackageSet.Remote{..}
  pure PackageSet.Package{..}
parsePackage (Dhall.App (Dhall.Field union "Local") (Dhall.TextLit (Dhall.Chunks [] spagoConfigPath)))
  | isLocationType union = do
      localPath <- case Text.isSuffixOf "/spago.dhall" spagoConfigPath of
        True  -> pure $ Text.dropEnd 12 spagoConfigPath
        False -> die $ Messages.failedToParseLocalRepo spagoConfigPath
      rawConfig <- liftIO $ Dhall.readRawExpr spagoConfigPath
      dependencies <- case rawConfig of
        Nothing -> die $ Messages.cannotFindConfigLocalPackage spagoConfigPath
        Just (_header, expr) -> do
          newExpr <- transformMExpr (pure . filterDependencies . addSourcePaths) expr
          -- Note: we have to use inputWithSettings here because we're about to resolve
          -- the raw config from the local project. So if that has any imports they
          -- should be relative to the directory of that package
          liftIO $
            Dhall.inputWithSettings
              (set Dhall.rootDirectory (Text.unpack localPath) Dhall.defaultInputSettings)
              dependenciesType
              (Dhall.pretty newExpr)
      let location = PackageSet.Local{..}
      pure PackageSet.Package{..}
parsePackage expr = die $ Messages.failedToParsePackage $ Dhall.pretty expr


-- | Tries to read in a Spago Config
parseConfig :: Spago m => m Config
parseConfig = do
  -- Here we try to migrate any config that is not in the latest format
  withConfigAST $ pure . addSourcePaths

  expr <- liftIO $ Dhall.inputExpr $ "./" <> path
  case expr of
    Dhall.RecordLit ks -> do
      packages :: Map PackageName Package <- Dhall.requireKey ks "packages" (\case
        Dhall.RecordLit pkgs ->
          fmap (Map.mapKeys PackageSet.PackageName . Dhall.Map.toMap)
          $ traverse parsePackage pkgs
        something -> throwM $ Dhall.PackagesIsNotRecord something)

      let sourcesType  = Dhall.list (Dhall.auto :: Dhall.Type Purs.SourcePath)
      name              <- Dhall.requireTypedKey ks "name" Dhall.strictText
      dependencies      <- Dhall.requireTypedKey ks "dependencies" dependenciesType
      configSourcePaths <- Dhall.requireTypedKey ks "sources" sourcesType

      let ensurePublishConfig = do
            publishLicense    <- Dhall.requireTypedKey ks "license" Dhall.strictText
            publishRepository <- Dhall.requireTypedKey ks "repository" Dhall.strictText
            pure PublishConfig{..}
      publishConfig <- try ensurePublishConfig

      let metadataPackageName = PackageSet.PackageName "metadata"
      let (metadataMap, packagesDB) = Map.partitionWithKey (\k _v -> k == metadataPackageName) packages
      let packagesMinPursVersion = join
            $ fmap (hush . Version.semver . (Text.replace "v" "") . PackageSet.version . PackageSet.location)
            $ Map.lookup metadataPackageName metadataMap
      let packageSet = PackageSet.PackageSet{..}

      pure Config{..}
    _ -> case Dhall.TypeCheck.typeOf expr of
      Right e  -> throwM $ Dhall.ConfigIsNotRecord e
      Left err -> throwM $ err


-- | Checks that the Spago config is there and readable
ensureConfig :: Spago m => m Config
ensureConfig = do
  exists <- testfile path
  unless exists $ do
    die $ Messages.cannotFindConfig
  try parseConfig >>= \case
    Right config -> do
      PackageSet.ensureFrozen
      pure config
    Left (err :: Dhall.ReadError Dhall.TypeCheck.X) -> throwM err


-- | Copies over `spago.dhall` to set up a Spago project.
--   Eventually ports an existing `psc-package.json` to the new config.
makeConfig :: Spago m => Bool -> m ()
makeConfig force = do
  unless force $ do
    hasSpagoDhall <- testfile path
    when hasSpagoDhall $ die $ Messages.foundExistingProject path
  writeTextFile path Templates.spagoDhall
  Dhall.format path

  -- We try to find an existing psc-package or Bower config, and if
  -- we find any we migrate the existing content
  -- Otherwise we just keep the default template
  bowerFileExists <- testfile "bower.json"
  pscfileExists <- testfile PscPackage.configPath

  case (pscfileExists, bowerFileExists) of
    (True, _) -> do
      -- first, read the psc-package file content
      content <- readTextFile PscPackage.configPath
      case eitherDecodeStrict $ Text.encodeUtf8 content of
        Left err -> echo $ Messages.failedToReadPscFile err
        Right pscConfig -> do
          echo "Found a \"psc-package.json\" file, migrating to a new Spago config.."
          -- try to update the dependencies (will fail if not found in package set)
          let pscPackages = map PackageSet.PackageName $ PscPackage.depends pscConfig
          config <- ensureConfig
          withConfigAST (\e -> addRawDeps config pscPackages
                               $ updateName (PscPackage.name pscConfig) e)
    (_, True) -> do
      -- read the bowerfile
      content <- readTextFile "bower.json"
      case eitherDecodeStrict $ Text.encodeUtf8 content of
        Left err -> die $ Messages.failedToParseFile path err
        Right packageMeta -> do
          echo "Found a \"bower.json\" file, migrating to a new Spago config.."
          -- then try to update the dependencies. We'll migrates the ones that we can,
          -- and print a message to the user to fix the missing ones
          config@Config{..} <- ensureConfig

          let (bowerName, packageResults) = migrateBower packageMeta packageSet
              (bowerErrors, bowerPackages) = partitionEithers packageResults

          if null bowerErrors
            then echo "All Bower dependencies are in the set! 🎉"
            else do
              echo $ tshow bowerErrors

          withConfigAST (\e -> addRawDeps config bowerPackages
                               $ updateName bowerName e)

    _ -> pure ()


migrateBower :: Bower.PackageMeta -> PackageSet -> (Text, [Either BowerDependencyError PackageName])
migrateBower Bower.PackageMeta{..} PackageSet{..} = (packageName, dependencies)
  where
    dependencies = map migratePackage (bowerDependencies <> bowerDevDependencies)

    -- | For each Bower dependency, we:
    --   * try to parse the range into a SemVer.Range
    --   * then check if it's a purescript package
    --   * then try to search in the Package Set for that package
    --   * then try to match the version there into the Bower range
    migratePackage :: (Bower.PackageName, Bower.VersionRange) -> Either BowerDependencyError PackageName
    migratePackage (Bower.runPackageName -> name, Bower.VersionRange unparsedRange) =
      case SemVer.parseSemVerRange unparsedRange of
        Left err -> Left $ UnparsableRange unparsedRange $ tshow err
        Right range -> case Text.stripPrefix "purescript-" name of
          Nothing -> Left $ NonPureScript name
          Just packageSetName | package <- PackageName packageSetName -> case Map.lookup package packagesDB of
            Nothing -> Left $ MissingFromTheSet package
            Just Package{ location = Local {..} } -> Right package
            Just Package{ location = Remote {..} } -> case SemVer.parseSemVer version of
              Right v | SemVer.matches range v -> Right package
              _                                -> Left $ WrongVersion package range version

    packageName =
      let name = Bower.runPackageName bowerName
      in case Text.isPrefixOf "purescript-" name of
        True  -> Text.drop 11 name
        False -> name

data BowerDependencyError
  = UnparsableRange Text Text
  | NonPureScript Text
  | MissingFromTheSet PackageName
  | WrongVersion PackageName SemVer.SemVerRange Text
  deriving (Show, Eq, Ord)


{-
verifyBower :: Spago m => m ()
verifyBower =  do
  echoDebug "Running `spago verify-bower`"
  Config{ packageSet = PackageSet{..}, ..} <- Config.ensureConfig
  deps <- Bower.ensureBowerFile
  let (warning, success) = List.partition isWarning $ check packagesDB <$> deps
  if null warning
    then echo "All dependencies are in the set!"
    else echo "Some dependencies are missing!"
  traverse_ echo $ "Warnings:" : (display <$> warning)
  traverse_ echo $ "Packages:" : (display <$> success)
  where
    check :: Map PackageName Package -> Bower.Dependency -> BowerDependencyResult
    check packageSet Bower.Dependency{..} = case Text.stripPrefix "purescript-" name of
      Nothing -> NonPureScript name
      Just package -> case Map.lookup (PackageName package) packageSet of
        Just Package{ location = Remote{..}, .. } -> case hush $ SemVer.parseSemVer version of
          Nothing -> WrongVersion package rangeText version
          Just v -> if SemVer.matches range v
                    then Match package rangeText version
                    else WrongVersion package rangeText version
        _ -> Missing package

    display :: BowerDependencyResult -> Text
    display = \case
      Match package range actual -> package <> " " <> actual <> " matches " <> range
      Missing package -> package <> " is not in the package set"
      NonPureScript name -> name <> " is not a PureScript package"
      WrongVersion package range actual -> package <> " " <> actual <> " does not match " <> range

    isWarning = \case
      Match _ _ _ -> False
      _           -> True


-}

updateName :: Text -> Expr -> Expr
updateName newName (Dhall.RecordLit kvs)
  | Just _name <- Dhall.Map.lookup "name" kvs = Dhall.RecordLit
    $ Dhall.Map.insert "name" (Dhall.toTextLit newName) kvs
updateName _ other = other

addRawDeps :: Spago m => Config -> [PackageName] -> Expr -> m Expr
addRawDeps config newPackages r@(Dhall.RecordLit kvs)
  | Just (Dhall.ListLit _ dependencies) <- Dhall.Map.lookup "dependencies" kvs = do
      case notInPackageSet of
        -- If none of the newPackages are outside of the set, add them to existing dependencies
        [] -> do
          oldPackages <- traverse (throws . Dhall.fromTextLit) dependencies
          let newDepsExpr
                = Dhall.ListLit Nothing $ fmap (Dhall.toTextLit . PackageSet.packageName)
                $ Seq.sort $ nubSeq (Seq.fromList newPackages <> fmap PackageSet.PackageName oldPackages)
          pure $ Dhall.RecordLit $ Dhall.Map.insert "dependencies" newDepsExpr kvs
        pkgs -> do
          echo $ Messages.failedToAddDeps $ map PackageSet.packageName pkgs
          pure r
  where
    notInPackageSet = mapMaybe
      (\p -> case Map.lookup p (PackageSet.packagesDB $ packageSet config) of
               Just _  -> Nothing
               Nothing -> Just p)
      newPackages

    -- | Code from https://stackoverflow.com/questions/45757839
    nubSeq :: Ord a => Seq a -> Seq a
    nubSeq xs = (fmap fst . Seq.filter (uncurry notElem)) (Seq.zip xs seens)
      where
        seens = Seq.scanl (flip Set.insert) Set.empty xs
addRawDeps _ _ other = pure other

addSourcePaths :: Expr -> Expr
addSourcePaths (Dhall.RecordLit kvs)
  | isConfigV1 kvs = Dhall.RecordLit
    $ Dhall.Map.insert "sources" (Dhall.ListLit Nothing $ fmap Dhall.toTextLit $ Seq.fromList ["src/**/*.purs", "test/**/*.purs"]) kvs
addSourcePaths expr = expr

isConfigV1, isConfigV2 :: Dhall.Map.Map Text v -> Bool
isConfigV1 (Set.fromList . Dhall.Map.keys -> configKeySet) =
  let configV1Keys = Set.fromList ["name", "dependencies", "packages"]
  in configKeySet == configV1Keys


isConfigV2 (Set.fromList . Dhall.Map.keys -> configKeySet) =
  let configV2Keys = Set.fromList ["name", "dependencies", "packages", "sources"]
  in configKeySet == configV2Keys


filterDependencies :: Expr -> Expr
filterDependencies (Dhall.RecordLit kvs)
  | isConfigV2 kvs, Just deps <- Dhall.Map.lookup "dependencies" kvs = deps
filterDependencies expr = expr


-- | Takes a function that manipulates the Dhall AST of the Config, and tries to run it
--   on the current config. If it succeeds, it writes back to file the result returned.
--   Note: it will pass in the parsed AST, not the resolved one (so e.g. imports will
--   still be in the tree). If you need the resolved one, use `ensureConfig`.
withConfigAST :: Spago m => (Expr -> m Expr) -> m ()
withConfigAST transform = do
  rawConfig <- liftIO $ Dhall.readRawExpr path
  case rawConfig of
    Nothing -> die Messages.cannotFindConfig
    Just (header, expr) -> do
      newExpr <- transformMExpr transform expr
      -- Write the new expression only if it has actually changed
      if (Dhall.Core.denote expr /= newExpr)
        then liftIO $ Dhall.writeRawExpr path (header, newExpr)
        else echoDebug "Transformed config is the same as the read one, not overwriting it"


transformMExpr
  :: MonadIO m
  => (Dhall.Expr s Dhall.Import -> m (Dhall.Expr s Dhall.Import))
  -> Dhall.Expr s Dhall.Import
  -> m (Dhall.Expr s Dhall.Import)
transformMExpr rules =
  transformMOf
    Dhall.subExpressions
    rules
    . Dhall.Core.denote


-- | Try to add the `newPackages` to the "dependencies" list in the Config.
--   It will not add any dependency if any of them is not in the package set.
--   If everything is fine instead, it will add the new deps, sort all the
--   dependencies, and write the Config back to file.
addDependencies :: Spago m => Config -> [PackageName] -> m ()
addDependencies config newPackages = do
  withConfigAST $ addRawDeps config newPackages
