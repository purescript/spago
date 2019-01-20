module Spago.Config
  ( makeConfig
  , ensureConfig
  , addDependencies
  , Config(..)
  ) where

import           Control.Exception         (throwIO, try)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Aeson                as JSON
import           Data.Either               (lefts, rights)
import           Data.Foldable             (toList)
import qualified Data.List                 as List
import qualified Data.Map                  as Map
import qualified Data.Sequence             as Seq
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Text.Prettyprint.Doc (Pretty)
import           Data.Typeable             (Typeable)
import qualified Dhall.Format
import qualified Dhall.Import
import qualified Dhall.Map
import qualified Dhall.Parser              as Parser
import qualified Dhall.Pretty
import           Dhall.TypeCheck           (X)
import qualified Dhall.TypeCheck
import           GHC.Generics              (Generic)
import qualified Turtle                    as T hiding (die, echo)

import qualified PscPackage                as PscPkg
import qualified PscPackage.Types          as PscPackage
import qualified Spago.Config.Dhall        as Dhall
import           Spago.Spacchetti          (Package, PackageName (..), Packages)
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
  , packages     :: Packages
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

data RawPackages = RawPackages
  { mkPackage :: Dhall.Import
  , upstream  :: Dhall.Import
  -- TODO: add additions and overrides if needed
  } deriving (Show, Generic)


-- | Tries to read in a Spago Config
parseConfig :: Text -> IO Config
parseConfig dhallText = do
  expr <- Dhall.inputExpr dhallText
  case expr of
    Dhall.RecordLit ks -> do
      name         <- required ks "name" Dhall.strictText
      dependencies <- required ks "dependencies" pkgNamesT

      packages <- case Dhall.Map.lookup "packages" ks of
          Just (Dhall.RecordLit pkgs) -> (Map.mapKeys PackageName . Dhall.Map.toMap)
            <$> Dhall.Map.traverseWithKey toPkg pkgs
          Just something -> throwIO $ Dhall.PackagesIsNotRecord something
          Nothing        -> throwIO $ Dhall.RequiredKeyMissing "packages" ks

      pure Config{..}
    _ -> case Dhall.TypeCheck.typeOf expr of
      Right e  -> throwIO $ Dhall.ConfigIsNotRecord e
      Left err -> throwIO $ err

  where
    required ks name typ = case (Dhall.Map.lookup name ks >>= Dhall.extract typ) of
      Just v  -> pure v
      Nothing -> liftIO $ throwIO $ Dhall.RequiredKeyMissing name ks

    pkgT      = Dhall.genericAuto :: Dhall.Type Package
    pkgNameT  = Dhall.auto :: Dhall.Type PackageName
    pkgNamesT = Dhall.list pkgNameT

    toPkg :: Text -> Dhall.Expr Parser.Src X -> IO Package
    toPkg _packageName pkgExpr = do
      -- we annotate the expression with the type we want,
      -- then typeOf will check the type for us
      let eAnnot = Dhall.Annot pkgExpr $ Dhall.expected pkgT
      -- typeOf only returns the type, which we already know
      let _typ = Dhall.TypeCheck.typeOf eAnnot
      -- the normalize is not strictly needed (we already normalized
      -- the expressions that were given to this function)
      -- but it converts the @Dhall.Expr s a@ @s@ arguments to any @t@,
      -- which is needed for @extract@ to type check with @eAnnot@
      case Dhall.extract pkgT $ Dhall.normalize $ eAnnot of
        Just x  -> pure x
        Nothing -> throwIO $ Dhall.WrongPackageType pkgExpr


-- | Checks that the Spago config is there and readable
ensureConfig :: IO Config
ensureConfig = do
  exists <- T.testfile path
  T.unless exists $ makeConfig False
  configText <- T.readTextFile path
  try (parseConfig configText) >>= \case
    Right config -> pure config
    Left (err :: Dhall.ReadError Dhall.Import) -> throwIO err


-- | Copies over `spago.dhall` to set up a Spago project
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
  pscfileExists <- T.testfile PscPkg.configPath
  T.when pscfileExists $ do
    -- first, read the psc-package file content
    content <- T.readTextFile PscPkg.configPath
    case JSON.eitherDecodeStrict $ Text.encodeUtf8 content of
      Left _err -> echo
                  ( "Warning: found a \"psc-package.json\" file, "
                    <> "but was not able to read it, skipping the conversion..")
      Right pscConfig -> do
        -- update the project name
        withConfigAST $ \config -> config { rawName = (PscPackage.name pscConfig) }
        -- TODO: check if all dependencies are in package set
        -- add the existing dependencies
        withConfigAST $ addRawDeps $ map PackageName $ PscPackage.depends pscConfig


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
  expr <- case Parser.exprFromText mempty configText of
    Left  err -> throwIO err
    Right ast -> case Dhall.denote ast of
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
        pure $ Dhall.RecordLit $ Dhall.Map.mapWithKey mkNewAST ks

      e -> throwIO $ Dhall.ConfigIsNotRecord e

  -- After modifying the expression, we have to check if it still typechecks
  -- if it doesn't we don't write to file
  resolvedExpr <- Dhall.Import.load expr
  case Dhall.TypeCheck.typeOf resolvedExpr of
    Left  err -> throwIO err
    Right _   -> T.writeTextFile path $ Dhall.pretty expr

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

-- | Adds the `name` dependency to the "dependencies" list in the Config,
--   sorts the dependencies, and writes the Config to file.
addDependencies :: [PackageName] -> IO ()
addDependencies = withConfigAST . addRawDeps
