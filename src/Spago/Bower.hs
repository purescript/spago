module Spago.Bower
  ( writeBowerJson
  ) where

import Spago.Prelude

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as Pretty
import qualified Data.ByteString.Lazy       as ByteString
import           Web.Bower.PackageMeta      (PackageMeta (..))
import qualified Web.Bower.PackageMeta      as Bower

import           Spago.Config               (Config (..))
import qualified Spago.Config               as Config
import qualified Spago.Packages             as Packages
import           Spago.PackageSet           (PackageName (..), Package (..))
import qualified Spago.Templates            as Templates


writeBowerJson :: Spago m => m ()
writeBowerJson = do
  config@Config{..} <- Config.ensureConfig

  bowerName <- mkPackageName name
  bowerDependencies <- mkDependencies config
  template <- templateBowerJson

  let bowerPkg = template { bowerName, bowerDependencies }
      prettyConfig = Pretty.defConfig
        { Pretty.confCompare = Pretty.keyOrder ["name", "ignore", "dependencies"] <> compare
        , Pretty.confTrailingNewline = True
        }
      bowerJson = Pretty.encodePretty' prettyConfig bowerPkg

  liftIO $ ByteString.writeFile "bower.json" bowerJson
  -- todo: check bower json is versioned


templateBowerJson :: Spago m => m Bower.PackageMeta
templateBowerJson = do
  case Aeson.decodeStrict Templates.bowerJson of
    Just t  ->
      pure t
    Nothing ->
      die "Invalid bower.json template (this is a Spago bug)"


mkPackageName :: Spago m => Text -> m Bower.PackageName
mkPackageName spagoName = do
  let psName = "purescript-" <> spagoName
  case Bower.mkPackageName psName of
    Left err ->
      die $ psName <> " is not a valid bower package name: " <> Bower.showPackageNameError err
    Right name ->
      pure name


mkDependencies :: Spago m => Config -> m [(Bower.PackageName, Bower.VersionRange)]
mkDependencies config = do
  deps <- Packages.getDirectDeps config
  for deps $ \(PackageName{..}, Package{..}) -> do
    bowerName <- mkPackageName packageName
    pure (bowerName, Bower.VersionRange $ "^" <> version)
