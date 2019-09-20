module Docs.Search.Config where

import Prelude

-- | Some magic constants.
config ::
  { declIndexDirectory :: String
  , mkIndexPartLoadPath :: Int -> String
  , mkIndexPartPath :: Int -> String
  , packageInfoPath :: String
  , packageInfoLoadPath :: String
  , mkShapeScriptPath :: String -> String
  , numberOfIndexParts :: Int
  , outputDirectory :: String
  , penalties :: { excessiveConstraint :: Int
                 , generalize :: Int
                 , instantiate :: Int
                 , match :: Int
                 , matchConstraint :: Int
                 , missingConstraint :: Int
                 , rowsMismatch :: Int
                 , typeVars :: Int
                 }
  , requiredDirectories :: Array String
  , resultsCount :: Int
  , typeIndexDirectory :: String
  }
config =
  { outputDirectory: "output"
  , requiredDirectories:
    [ "generated-docs"
    , "generated-docs/html"
    , "output"
    ]
  -- ^ Directories required by IndexBuilder
  , declIndexDirectory: "generated-docs/html/index/declarations"
  , typeIndexDirectory: "generated-docs/html/index/types"
  , mkShapeScriptPath:
    \shape -> "./index/types/" <> shape <> ".js"
  , numberOfIndexParts: 50
  -- ^ In how many parts the index should be splitted?
  , mkIndexPartPath:
    \(partId :: Int) -> "html/index/declarations/" <> show partId <> ".js"
  , mkIndexPartLoadPath:
    \(partId :: Int) -> "./index/declarations/" <> show partId <> ".js"
  , packageInfoPath: "generated-docs/html/index/packages.json"
  -- ^ Path to package index.
  , packageInfoLoadPath: "./index/packages.json"
  -- ^ Used to load package index to the browser scope.
  , resultsCount: 25
  -- ^ How many results to show by default?
  , penalties: { typeVars: 2
               , match: 2
               , matchConstraint: 1
               , instantiate: 2
               , generalize: 2
               , rowsMismatch: 3
               , missingConstraint: 1
               , excessiveConstraint: 1
               }
  -- ^ Penalties used to determine how "far" a type query is from a given type.
  -- See Docs.Search.TypeQuery
  }
