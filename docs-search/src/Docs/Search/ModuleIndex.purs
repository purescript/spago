module Docs.Search.ModuleIndex where

import Docs.Search.Config (config)
import Docs.Search.Declarations (Declarations(..))
import Docs.Search.SearchResult (SearchResult(..))
import Docs.Search.Types (ModuleName, PackageName)

import Prelude
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (hush)
import Data.Foldable (foldr)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Search.Trie as Trie
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff)


type ModuleIndex = Map PackageName (Set ModuleName)


-- | Constructs a mapping from packages to modules
mkModuleIndex :: Declarations -> ModuleIndex
mkModuleIndex (Declarations trie) =
  foldr (Map.unionWith Set.union) mempty $ extract <$> Trie.values trie
  where
    extract
      :: List SearchResult
      -> Map PackageName (Set ModuleName)
    extract = foldr (Map.unionWith Set.union) mempty <<< map mkEntry
      where
        mkEntry (SearchResult { packageName, moduleName }) =
          Map.singleton packageName (Set.singleton moduleName)


loadModuleIndex :: Aff ModuleIndex
loadModuleIndex = do
  json <- toAffE $ load config.moduleIndexLoadPath
  pure $ fromMaybe mempty $ hush $ decodeJson json


foreign import load
  :: String
  -> Effect (Promise Json)
