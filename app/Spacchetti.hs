{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Spacchetti where

import           Data.Aeson
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Text       (Text)
import qualified Dhall
import qualified Dhall.Core      as Dhall
import qualified Dhall.Map
import           GHC.Generics (Generic)
import           Dhall.Parser    (Src)
import           Dhall.TypeCheck (X)
import qualified Dhall.TypeCheck
import Control.Monad.IO.Class (liftIO)

-- | Matches the packages definition of Spacchetti Package.dhall/psc-package
newtype PackageName = PackageName { packageName :: Text }
  deriving (Show)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Dhall.Interpret)

-- | A spacchetti package.
data Package = Package
  { dependencies :: [PackageName] -- ^ list of dependency package names
  , repo         :: Text          -- ^ the git repository
  , version      :: Text          -- ^ version string (also functions as a git ref)
  }
  deriving (Show, Generic)

instance ToJSON Package
instance FromJSON Package

type Packages = Map PackageName Package

-- | Spacchetti configuration file type
data Config = Config
  { name         :: Text
  , dependencies :: [PackageName]
  , packages     :: Packages
  , paths        :: [Text]
  }
  deriving (Show, Generic)

instance ToJSON Config
instance FromJSON Config

-- | Given a config, tries to read it into a Spacchetti Config
parseConfig :: Text -> IO Config
parseConfig dhallText = do
  expr <- Dhall.inputExpr dhallText
  Just config <- pure $ case expr of
    --
    Dhall.RecordLit ks -> do
      let pkgType = Dhall.genericAuto :: Dhall.Type Package
          pkgNameType = Dhall.auto :: Dhall.Type PackageName

      Just name <- Dhall.extract Dhall.strictText
        <$> Dhall.Map.lookup "name" ks
      Just dependencies <- Dhall.extract (Dhall.list pkgNameType)
        <$> Dhall.Map.lookup "dependencies" ks
      Just paths <- Dhall.extract (Dhall.list Dhall.strictText)
        <$> Dhall.Map.lookup "paths" ks
      Just packages <- do
        Dhall.Map.lookup "packages" ks >>= \case
          Dhall.RecordLit pkgs -> pure
            $ (Map.mapKeys PackageName . Dhall.Map.toMap)
            <$> Dhall.Map.traverseWithKey toPkg pkgs
            where
              toPkg :: Text -> Dhall.Expr Src X -> Maybe Package
              toPkg _packageName pkgExpr = do
                -- we annotate the expression with the type we want,
                -- then typeOf will check the type for us
                let eAnnot = Dhall.Annot pkgExpr $ Dhall.expected pkgType
                -- typeOf only returns the type, which we already know
                let _typ = Dhall.TypeCheck.typeOf eAnnot
                -- the normalize is not strictly needed (we already normalized
                -- the expressions that were given to this function)
                -- but it converts the @Dhall.Expr s a@ @s@ arguments to any @t@,
                -- which is needed for @extract@ to type check with @eAnnot@
                case Dhall.extract pkgType $ Dhall.normalize $ eAnnot of
                  Just x -> pure x
                  Nothing -> undefined -- TODO throw error here
          _ -> Nothing -- TODO throw error in here
      pure Config{..}
    _ -> Nothing -- TODO throw error in here
  pure config
