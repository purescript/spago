{-# LANGUAGE GADTs #-}
module Spago.Config
  ( makeConfig
  , ensureConfig
  , addDependency
  , Config(..)
  ) where

import           Control.Exception                     (Exception, throwIO, try)
import           Control.Monad.IO.Class                (liftIO)
import           Data.Aeson
import           Data.Foldable                         (toList)
import qualified Data.List                             as List
import qualified Data.Map                              as Map
import qualified Data.Sequence                         as Seq
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc             (Pretty)
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as PrettyText
import           Data.Typeable                         (Typeable)
import qualified Dhall
import qualified Dhall.Core                            as Dhall
import qualified Dhall.Format
import qualified Dhall.Map
import qualified Dhall.Parser                          as Parser
import qualified Dhall.Pretty
import           Dhall.TypeCheck                       (X)
import qualified Dhall.TypeCheck
import           GHC.Generics                          (Generic)
import qualified Turtle                                as T hiding (die, echo)

import           Spago.Spacchetti                      (Package, PackageName (..), Packages)
import qualified Spago.Templates                       as Templates
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
  }
  deriving (Show, Generic)

instance ToJSON Config
instance FromJSON Config

-- | Spago configuration cannot be read
data ConfigReadError a where
 WrongPackageType      :: Typeable a => Dhall.Expr Parser.Src a -> ConfigReadError a
   -- ^ a package has the wrong type
 ConfigIsNotRecord     :: Typeable a => Dhall.Expr Parser.Src a -> ConfigReadError a
   -- ^ the toplevel value is not a record
 PackagesIsNotRecord   :: Typeable a => Dhall.Expr Parser.Src a -> ConfigReadError a
   -- ^ the "packages" key is not a record
 DependenciesIsNotList :: Typeable a => Dhall.Expr Parser.Src a -> ConfigReadError a
   -- ^ the "dependencies" key is not a list
 ExprIsNotTextLit      :: Typeable a => Dhall.Expr Parser.Src a -> ConfigReadError a
   -- ^ the expression is not a Text Literal
 KeyIsMissing          :: Typeable a => Text -> ConfigReadError a
   -- ^ a key is missing from the config

instance (Pretty a, Typeable a) => Exception (ConfigReadError a)

instance (Pretty a) => Show (ConfigReadError a) where
  show err = Text.unpack $ Text.intercalate "\n" $
    [ _ERROR <> ": Error while reading spago.dhall:"
    , "" ]
    <> msg err

    where
      msg :: ConfigReadError a -> [Dhall.Text]
      msg (WrongPackageType pkg) =
        [ "Explanation: The outermost record must only contain packages."
        , ""
        , "The following field was not a package:"
        , ""
        , "↳ " <> pretty pkg
        ]
      msg (PackagesIsNotRecord tl) =
        [ "Explanation: The \"packages\" key must contain a record of packages."
        , ""
        , "The value was instead:"
        , ""
        , "↳ " <> pretty tl
        ]
      msg (DependenciesIsNotList e) =
        [ "Explanation: The \"dependencies\" key must contain a list of package names."
        , ""
        , "The value was instead:"
        , ""
        , "↳ " <> pretty e
        ]
      msg (ConfigIsNotRecord tl) =
        [ "Explanation: The config should be a record."
        , ""
        , "Its type is instead:"
        , ""
        , "↳ " <> pretty tl
        ]
      msg (KeyIsMissing key) =
        [ "Explanation: the configuration is missing a required key."
        , ""
        , "The key missing is:"
        , ""
        , "↳ " <> key
        ]
      msg (ExprIsNotTextLit e) =
        [ "Explanation: the configuration contained a value that we expected to be"
        , "a string, but wasn't."
        , ""
        , "The value was instead:"
        , ""
        , "↳ " <> pretty e
        ]

      _ERROR :: Dhall.Text
      _ERROR = "\ESC[1;31mError\ESC[0m"

pretty :: Pretty.Pretty a => Dhall.Expr s a -> Dhall.Text
pretty = PrettyText.renderStrict
  . Pretty.layoutPretty Pretty.defaultLayoutOptions
  . Pretty.pretty


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
          Just something -> throwIO $ PackagesIsNotRecord something
          Nothing        -> throwIO $ (KeyIsMissing "packages" :: ConfigReadError Dhall.Import)

      pure Config{..}
    _ -> case Dhall.TypeCheck.typeOf expr of
      Right e  -> throwIO $ ConfigIsNotRecord e
      Left err -> throwIO $ err

  where
    required ks name typ = case (Dhall.Map.lookup name ks >>= Dhall.extract typ) of
      Just v  -> pure v
      Nothing -> liftIO $ throwIO $ (KeyIsMissing name :: ConfigReadError Dhall.Import)

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
        Nothing -> throwIO $ WrongPackageType pkgExpr

-- | Checks that the Spago config is there and readable
ensureConfig :: IO Config
ensureConfig = do
  exists <- T.testfile path
  T.unless exists $ makeConfig False
  configText <- T.readTextFile path
  try (parseConfig configText) >>= \case
    Right config -> pure config
    Left (err :: ConfigReadError Dhall.Import) -> throwIO err

-- | Copies over `spago.dhall` to set up a Spago project
makeConfig :: Bool -> IO ()
makeConfig force = do
  T.unless force $ do
    hasSpagoDhall <- T.testfile path
    T.when hasSpagoDhall $ die
       $ "Found " <> pathText <> ": there's already a project here. "
      <> "Run `spago init --force` if you're sure you want to overwrite it."
  T.touch path
  -- TODO: try to read a psc-package config, so we can migrate automatically
  T.writeTextFile path Templates.spagoDhall

  Dhall.Format.format Dhall.Pretty.Unicode (Just $ Text.unpack pathText)

-- | Takes a function that manipulates the Configuration Dhall AST,
--   and tries to run it on the current config.
--   If it succeeds, it writes back to file the result returned.
--   Note: it will pass in the parsed AST, not the resolved one (so
--   e.g. imports will still be in the tree). If you need the resolved
--   one, use `ensureConfig`.
withConfigAST
  :: (Dhall.Expr Parser.Src Dhall.Import -> IO (Dhall.Expr Parser.Src Dhall.Import))
  -> IO ()
withConfigAST action = do
  exists <- T.testfile path
  T.unless exists $ makeConfig False
  configText <- T.readTextFile path
  expr <- case Parser.exprFromText mempty configText of
    Left err  -> throwIO err
    Right ast -> action $ Dhall.denote ast
  T.writeTextFile path $ pretty expr

-- | Adds the `name` dependency to the "dependencies" list in the Config AST
addDependencyAST
  :: Text
  -> Dhall.Expr Parser.Src Dhall.Import
  -> IO (Dhall.Expr Parser.Src Dhall.Import)
addDependencyAST name expr =
  let
    addToList "dependencies" (Dhall.ListLit typ deps) = do
      texts <- traverse fromTextLit $ toList deps
      pure $ Dhall.ListLit typ
        $ Seq.fromList
        $ fmap toTextLit
        $ List.sort
        $ List.nub
        $ (:) name texts
    addToList "dependencies" e = throwIO $ DependenciesIsNotList e
    addToList _ val = pure val
  in

  case expr of
    Dhall.RecordLit config -> Dhall.RecordLit
      <$> Dhall.Map.traverseWithKey addToList config
    e -> throwIO $ ConfigIsNotRecord e

-- | Returns a Dhall Text literal from a lone string
toTextLit :: Pretty a => Text -> Dhall.Expr Parser.Src a
toTextLit str = Dhall.TextLit (Dhall.Chunks [] str)

-- | Casts a Dhall Text literal to a string, or fails
fromTextLit :: (Pretty a, Typeable a) => Dhall.Expr Parser.Src a -> IO Text
fromTextLit (Dhall.TextLit (Dhall.Chunks [] str)) = pure str
fromTextLit expr                                  = throwIO $ ExprIsNotTextLit expr

-- | Adds the `name` dependency to the "dependencies" list in the Config,
--   sorts the dependencies, and writes the Config to file.
addDependency :: Text -> IO ()
addDependency name = withConfigAST (addDependencyAST name)
