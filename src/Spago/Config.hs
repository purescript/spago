{-# LANGUAGE ViewPatterns #-}
module Spago.Config
  ( makeConfig
  , ensureConfig
  , ensurePublishConfig
  , addDependencies
  , Config(..)
  , PublishConfig(..)
  ) where

import           Spago.Prelude

import qualified Data.Map           as Map
import qualified Data.Sequence      as Seq
import qualified Data.Set           as Set
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Versions      as Version
import qualified Dhall.Core
import qualified Dhall.Map
import qualified Dhall.TypeCheck

import qualified Spago.Dhall        as Dhall
import qualified Spago.Messages     as Messages
import qualified Spago.PackageSet   as PackageSet
import qualified Spago.PscPackage   as PscPackage
import qualified Spago.Purs         as Purs
import qualified Spago.Templates    as Templates

import           Spago.PackageSet   (Package, PackageName, PackageSet)


pathText :: Text
pathText = "spago.dhall"

-- | Path for the Spago Config
path :: FilePath
path = pathFromText pathText


-- | Spago configuration file type
data Config = Config
  { name              :: Text
  , dependencies      :: [PackageName]
  , packageSet        :: PackageSet
  , configSourcePaths :: [Purs.SourcePath]
  } deriving (Show, Generic)

-- | The extra fields that are only needed for publishing libraries.
data PublishConfig = PublishConfig
  { license           :: Text
  , repository        :: Text
  } deriving (Show, Generic)

type Expr = Dhall.DhallExpr Dhall.Import

type ConfigParser a =
  Dhall.Map.Map Text (Dhall.DhallExpr Dhall.TypeCheck.X)
  -> Either (Dhall.ReadError Dhall.TypeCheck.X) a


-- | Tries to read in a Spago Config
parseConfig :: ConfigParser Config
parseConfig ks = do

  let packageTyp      = Dhall.genericAuto :: Dhall.Type Package
      packageNamesTyp = Dhall.list (Dhall.auto :: Dhall.Type PackageName)
      sourcesType     = Dhall.list (Dhall.auto :: Dhall.Type Purs.SourcePath)
  name         <- Dhall.requireTypedKey ks "name" Dhall.strictText
  dependencies <- Dhall.requireTypedKey ks "dependencies" packageNamesTyp
  packages     <- Dhall.requireKey ks "packages" $ \case
    Dhall.RecordLit pkgs -> (Map.mapKeys PackageSet.PackageName . Dhall.Map.toMap)
      <$> traverse (Dhall.coerceToType packageTyp) pkgs
    something -> Left $ Dhall.PackagesIsNotRecord something
  configSourcePaths  <- Dhall.requireTypedKey ks "sources" sourcesType

  let metadataPackageName = PackageSet.PackageName "metadata"
      (metadataMap, packagesDB) = Map.partitionWithKey (\k _v -> k == metadataPackageName) packages
      packagesMinPursVersion = join
        $ fmap (hush . Version.semver . (Text.replace "v" "") . PackageSet.version)
        $ Map.lookup metadataPackageName metadataMap
      packageSet = PackageSet.PackageSet{..}

  Right $ Config{..}


parsePublishConfig :: ConfigParser PublishConfig
parsePublishConfig ks = do
  license <- Dhall.requireTypedKey ks "license" Dhall.strictText
  repository <- Dhall.requireTypedKey ks "repository" Dhall.strictText
  Right $ PublishConfig{..}


-- | Checks that the Spago config is there and readable
ensureConfig :: Spago m => m Config
ensureConfig = ensureConfigInternal parseConfig


-- | Checks that the Spago config is there and has all the fields required for publishing.
ensurePublishConfig :: Spago m => m PublishConfig
ensurePublishConfig = ensureConfigInternal parsePublishConfig


ensureConfigInternal :: Spago m => ConfigParser a -> m a
ensureConfigInternal parseMap = do

  exists <- testfile path
  unless exists $ do
    die $ Messages.cannotFindConfig

  withConfigAST $ pure . addSourcePaths

  expr <- liftIO $ Dhall.inputExpr $ "./" <> pathText
  case expr of
    Dhall.RecordLit ks -> do
      case parseMap ks of
        Right config -> do
          PackageSet.ensureFrozen
          pure config
        Left err     -> throwM err
    _ -> case Dhall.TypeCheck.typeOf expr of
      Right e  -> throwM $ Dhall.ConfigIsNotRecord e
      Left err -> throwM $ err


-- | Copies over `spago.dhall` to set up a Spago project.
--   Eventually ports an existing `psc-package.json` to the new config.
makeConfig :: Spago m => Bool -> m ()
makeConfig force = do
  unless force $ do
    hasSpagoDhall <- testfile path
    when hasSpagoDhall $ die $ Messages.foundExistingProject pathText
  writeTextFile path Templates.spagoDhall
  Dhall.format DoFormat pathText

  -- We try to find an existing psc-package config, and we migrate the existing
  -- content if we found one, otherwise we copy the default template
  pscfileExists <- testfile PscPackage.configPath
  when pscfileExists $ do
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
  where
    isConfigV1 (Set.fromList . Dhall.Map.keys -> configKeySet) =
      let configV1Keys = Set.fromList ["name", "dependencies", "packages"]
      in configKeySet == configV1Keys
addSourcePaths expr = expr

-- | Takes a function that manipulates the Dhall AST of the Config, and tries to run it
--   on the current config. If it succeeds, it writes back to file the result returned.
--   Note: it will pass in the parsed AST, not the resolved one (so e.g. imports will
--   still be in the tree). If you need the resolved one, use `ensureConfig`.
withConfigAST :: Spago m => (Expr -> m Expr) -> m ()
withConfigAST transform = do
  rawConfig <- liftIO $ Dhall.readRawExpr pathText
  shouldFormat <- asks globalDoFormat
  case rawConfig of
    Nothing -> die Messages.cannotFindConfig
    Just (header, expr) -> do
      newExpr <- transformMExpr transform expr
      liftIO $ Dhall.writeRawExpr shouldFormat pathText (header, newExpr)
  where
    transformMExpr
      :: Spago m
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
