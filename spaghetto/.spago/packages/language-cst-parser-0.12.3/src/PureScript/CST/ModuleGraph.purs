module PureScript.CST.ModuleGraph
  ( moduleGraph
  , sortModules
  , ModuleSort(..)
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (all, foldl)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import PureScript.CST.Types (ImportDecl(..), ModuleHeader(..), ModuleName, Name(..))

type Graph a = Map a (Set a)

moduleGraph :: forall e a. (a -> ModuleHeader e) -> Array a -> Graph ModuleName
moduleGraph k = Map.fromFoldable <<< map (go <<< k)
  where
  go (ModuleHeader { name: Name { name }, imports }) =
    Tuple name (Set.fromFoldable (map getImportName imports))

  getImportName (ImportDecl { "module": Name { name } }) = name

data ModuleSort a
  = Sorted (Array a)
  | CycleDetected (Array a)

sortModules :: forall e a. (a -> ModuleHeader e) -> Array a -> ModuleSort a
sortModules k moduleHeaders = do
  let
    getModuleName :: ModuleHeader e -> ModuleName
    getModuleName (ModuleHeader { name: Name { name } }) = name

    knownModuleHeaders :: Map ModuleName a
    knownModuleHeaders =
      moduleHeaders
        # map (\a -> Tuple (getModuleName (k a)) a)
        # Map.fromFoldable

    graph = moduleGraph k moduleHeaders
    lookupModuleHeaders = Array.mapMaybe (flip Map.lookup knownModuleHeaders) <<< List.toUnfoldable

  case topoSort graph of
    Left cycle -> CycleDetected (lookupModuleHeaders cycle)
    Right sorted -> Sorted (lookupModuleHeaders sorted)

type TopoSortArgs a =
  { roots :: Set a
  , sorted :: List a
  , usages :: Map a Int
  }

topoSort :: forall a. Ord a => Graph a -> Either (List a) (List a)
topoSort graph = do
  _.sorted <$> go { roots: startingModules, sorted: Nil, usages: importCounts }
  where
  go :: TopoSortArgs a -> Either (List a) (TopoSortArgs a)
  go { roots, sorted, usages } = case Set.findMin roots of
    Nothing ->
      if all (eq 0) usages then
        Right { roots, sorted, usages }
      else do
        let
          nonLeaf =
            usages
              # Map.filterWithKey (\a count -> count > 0 && not (maybe true Set.isEmpty (Map.lookup a graph)))
              # Map.keys

          detectCycles = foldl (\b a -> if isJust b then b else depthFirst { path: Nil, visited: Set.empty, curr: a }) Nothing nonLeaf

        case detectCycles of
          Just cycle -> Left cycle
          Nothing -> Left Nil

    Just curr -> do
      let
        reachable = fromMaybe Set.empty (Map.lookup curr graph)
        usages' = foldl decrementImport usages reachable
      go
        { roots: foldl (appendRoots usages') (Set.delete curr roots) reachable
        , sorted: Cons curr sorted
        , usages: usages'
        }

  appendRoots :: Map a Int -> Set a -> a -> Set a
  appendRoots usages roots curr = maybe roots (flip Set.insert roots) do
    count <- Map.lookup curr usages
    isRoot (Tuple curr count)

  decrementImport :: Map a Int -> a -> Map a Int
  decrementImport usages k = Map.insertWith add k (-1) usages

  startingModules :: Set a
  startingModules = Map.keys $ Map.filterWithKey (\k v -> isJust (isRoot (Tuple k v))) importCounts

  importCounts :: Map a Int
  importCounts = Map.fromFoldableWith add do
    Tuple a bs <- Map.toUnfoldable graph
    [ Tuple a 0 ] <> map (flip Tuple 1) (Set.toUnfoldable bs)

  isRoot :: Tuple a Int -> Maybe a
  isRoot (Tuple a count) = if count == 0 then Just a else Nothing

  depthFirst :: { path :: List a, visited :: Set a, curr :: a } -> Maybe (List a)
  depthFirst { path, visited, curr } =
    if Set.member curr visited then
      Just (Cons curr path)
    else if maybe true Set.isEmpty (Map.lookup curr graph) then
      Nothing
    else do
      reachable <- Map.lookup curr graph
      foldl (\b a -> if isJust b then b else depthFirst { path: Cons curr path, visited: Set.insert curr visited, curr: a }) Nothing reachable
