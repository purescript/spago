module Spago.BowerMigration where

import           Spago.Prelude          hiding (Success)

import qualified Data.Aeson             as A
import           Data.Bifunctor         (bimap)
import qualified Data.ByteString.Lazy   as B
import           Data.Either.Validation
import qualified Data.SemVer            as SemVer
import qualified Data.Text.Encoding     as Text
import qualified Spago.Messages         as Messages
import           Web.Bower.PackageMeta  (PackageMeta (..))
import qualified Web.Bower.PackageMeta  as Bower

data Dependency = Dependency
  { name      :: Text
  , rangeText :: Text
  , range     :: SemVer.SemVerRange
  } deriving (Show)

parseRange :: (Bower.PackageName, Bower.VersionRange) -> Validation [(Text, Text)] Dependency
parseRange (name', Bower.VersionRange range)
  = bimap (const $ [(name, range)]) (Dependency name range)
      $ eitherToValidation
      $ SemVer.parseSemVerRange range
  where
    name = Bower.runPackageName name'

pathText :: Text
pathText = "bower.json"

-- | Path for the Bower file
path :: FilePath
path = pathFromText pathText

-- | Checks that the Bower file is there and readable
ensureBowerFile :: Spago m => m [Dependency]
ensureBowerFile = do
  exists <- testfile path
  unless exists $ die "Cannot find bower.json"
  file <- B.fromStrict . Text.encodeUtf8 <$> readTextFile path
  case A.eitherDecode file of
    Left err -> die $ Messages.failedToParseFile pathText err
    Right PackageMeta{..} -> case traverse parseRange (bowerDependencies <> bowerDevDependencies) of
      Failure x -> let
        names :: [Text] = fst <$> x
        message = Messages.makeMessage $ "Could not parse range for package(s):" : names
        in die message
      Success x -> pure x
