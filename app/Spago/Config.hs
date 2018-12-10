module Spago.Config where

import           Control.Exception                     (Exception, throwIO)
import           Control.Monad.IO.Class                (liftIO)
import           Data.Aeson
import qualified Data.Map                              as Map
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import qualified Data.Text.Prettyprint.Doc             as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as PrettyText
import qualified Dhall
import qualified Dhall.Core                            as Dhall
import qualified Dhall.Map
import           Dhall.Parser                          (Src)
import           Dhall.TypeCheck                       (X)
import qualified Dhall.TypeCheck
import           GHC.Generics                          (Generic)

import           Spago.Spacchetti                      (Package, PackageName (..), Packages)

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
data ConfigReadError
 = WrongPackageType (Dhall.Expr Src X)
   -- ^ a package has the wrong type
 | ConfigIsNotRecord (Dhall.Expr Src X)
   -- ^ the toplevel value is not a record
 | PackagesIsNotRecord (Dhall.Expr Src X)
   -- ^ the "packages" key is not a record
 | KeyIsMissing Text
   -- ^ a key is missing from the config

instance Exception ConfigReadError

instance Show ConfigReadError where
  show err = Text.unpack $ Text.intercalate "\n" $
    [ _ERROR <> ": Error while reading spago.dhall:"
    , "" ]
    <> msg err

    where
      msg :: ConfigReadError -> [Dhall.Text]
      msg (WrongPackageType pkg) =
        [ "Explanation: The outermost record must only contain packages."
        , ""
        , "The following field was not a package:"
        , ""
        , "↳ " <> Dhall.pretty pkg
        ]
      msg (PackagesIsNotRecord tl) =
        [ "Explanation: The outermost value must be a record of packages."
        , ""
        , "The record was:"
        , ""
        , "↳ " <> pretty tl
        ]
      msg (ConfigIsNotRecord tl) =
        [ "Explanation: The config should be a record."
        , ""
        , "Its type is instead:"
        , ""
        , "↳ " <> pretty tl
        ]
      msg (KeyIsMissing key) =
        [ "Explanation: the configuration is missing a required key"
        , ""
        , "The key missing is:"
        , ""
        , "↳ " <> key
        ]

      pretty :: Pretty.Pretty a => Dhall.Expr s a -> Dhall.Text
      pretty = PrettyText.renderStrict
               . Pretty.layoutPretty Pretty.defaultLayoutOptions
               . Pretty.pretty

      _ERROR :: Dhall.Text
      _ERROR = "\ESC[1;31mError\ESC[0m"

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
          Nothing        -> throwIO $ KeyIsMissing "packages"

      pure Config{..}
    _ -> case Dhall.TypeCheck.typeOf expr of
      Right e  -> throwIO $ ConfigIsNotRecord e
      Left err -> throwIO $ err

  where
    required ks name typ = case (Dhall.Map.lookup name ks >>= Dhall.extract typ) of
      Just v -> pure v
      Nothing -> liftIO $ throwIO $ KeyIsMissing name

    pkgT      = Dhall.genericAuto :: Dhall.Type Package
    pkgNameT  = Dhall.auto :: Dhall.Type PackageName
    pkgNamesT = Dhall.list pkgNameT

    toPkg :: Text -> Dhall.Expr Src X -> IO Package
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
