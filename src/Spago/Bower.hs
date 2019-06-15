module Spago.Bower where

import           Spago.Prelude hiding (Success)

import           Control.Lens         ((^@..))
import qualified Data.Aeson           as A
import           Data.Aeson.Lens
import           Data.Bifunctor       (bimap)
import qualified Data.ByteString.Lazy as B
import           Data.Either.Validation
import qualified Data.SemVer          as SemVer
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import qualified Spago.Messages       as Messages

data Dependency = Dependency
  { name :: Text
  , rangeText :: Text
  , range :: SemVer.SemVerRange
  } deriving (Show)

data RawDependency = RawDependency
  { name :: Text
  , range :: Text
  } deriving (Show)

parseRange :: RawDependency -> Validation [RawDependency] Dependency
parseRange raw@RawDependency{..}
  = bimap (const $ [raw]) (Dependency name range)
      $ eitherToValidation
      $ SemVer.parseSemVerRange range

rawDeps :: A.Value -> [RawDependency]
rawDeps input
  = foldMap (fmap (uncurry RawDependency) . get) ["dependencies", "devDependencies"]
  where
    get x = input ^@.. key x
                     . members
                     . _String

pathText :: Text
pathText = "bower.json"

-- | Path for the Bower file
path :: FilePath
path = pathFromText pathText

-- | Checks that the Bower file is there and readable
ensureBowerFile :: Spago m => m [Dependency]
ensureBowerFile = do
  exists <- testfile path
  unless exists $ do
    die $ Messages.cannotFindBowerFile
  file <- B.fromStrict . Text.encodeUtf8 <$> readTextFile path
  case rawDeps <$> A.eitherDecode file of
    Left err  -> die $ Messages.failedToParseFile pathText err
    Right raw -> case traverse parseRange raw of
      Failure x -> let
        names   = (\RawDependency{..} -> name) <$> x
        message = Messages.makeMessage $ "Could not parse range for package(s):" : names
        in die message
      Success x -> pure x
