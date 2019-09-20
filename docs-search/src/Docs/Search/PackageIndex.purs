module Docs.Search.PackageIndex where

import Docs.Search.Config (config)
import Docs.Search.Extra (stringToList)

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap, wrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Web.Bower.PackageMeta (Dependencies, PackageMeta(..))
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Search.Trie (Trie)
import Data.Search.Trie as Trie
import Data.String.CodeUnits as String


type PackageResult
  = { name :: String
    , description :: Maybe String
    , score :: Int
    , dependencies :: Array String
    , repository :: Maybe String
    }

type Scores = Map String Int

type PackageIndex = Trie Char PackageResult

type PackageInfo = Array PackageResult


-- | Construct a mapping from package names to their scores, based on number
-- of reverse dependencies.
mkScores :: Array PackageMeta -> Scores
mkScores =
  Array.foldr
  (\pm ->
    updateScoresFor (unwrap pm).dependencies >>>
    updateScoresFor (unwrap pm).devDependencies
  )
  mempty

  where
    updateScoresFor :: Dependencies -> Scores -> Scores
    updateScoresFor deps scores =
      Array.foldr
      (\dep -> Map.insertWith add dep 1)
      scores
      (deps # unwrap >>> map (_.packageName))


mkPackageInfo :: Array PackageMeta -> PackageInfo
mkPackageInfo pms =
  Array.fromFoldable $
  Map.values $
  Array.foldr insert mempty pms

  where
    packageScores = mkScores pms


    insert
      :: PackageMeta
      -> Map String PackageResult
      -> Map String PackageResult
    insert
      (PackageMeta { name
                   , description
                   , dependencies
                   , devDependencies
                   , repository }) =

          Map.insert
          name
          { name
          , description: description
          , score: fromMaybe 0 $ Map.lookup name packageScores
          , dependencies: unwrap dependencies <#> (_.packageName)
          , repository: repository <#> (_.url)
          }


loadPackageIndex :: Aff PackageIndex
loadPackageIndex = do
  json <- toAffE (load config.packageInfoLoadPath)
  let packageInfo = fromMaybe mempty $ hush $ decodeJson json
  pure $ mkPackageIndex packageInfo


mkPackageIndex :: PackageInfo -> PackageIndex
mkPackageIndex =

  Array.foldr
  (\package -> Trie.insert (shortNamePath package.name) package)
  mempty

  where
    shortNamePath name =
      stringToList $
      fromMaybe name $
      String.stripPrefix (wrap "purescript-") name


queryPackageIndex
  :: forall m
  .  Monad m
  => PackageIndex
  -> String
  -> m { index :: PackageIndex
       , results :: Array PackageResult
       }
queryPackageIndex index q =
  pure { index
       , results: Array.fromFoldable $ Trie.queryValues (stringToList q) index
       }


foreign import load
  :: String
  -> Effect (Promise Json)
