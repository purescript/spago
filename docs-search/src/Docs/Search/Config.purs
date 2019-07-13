module Docs.Search.Config where

import Prelude

config =
  { outputDirectory: "output"
  , requiredDirectories:
    [ "generated-docs"
    , "output"
    ]
  -- ^ Directories required by IndexBuilder
  , indexDirectory: "generated-docs/index"
  , declIndexDirectory: "generated-docs/index/declarations"
  , typeIndexDirectory: "generated-docs/index/types"
  , mkShapeScriptPath:
    \shape -> "../index/types/" <> shape <> ".js"
  , numberOfIndexParts: 50
  -- ^ In how many parts the index should be splitted?
  , mkIndexPartPath:
    \(partId :: Int) -> "generated-docs/index/declarations/" <> show partId <> ".js"
  , mkIndexPartLoadPath:
    \(partId :: Int) -> "../index/declarations/" <> show partId <> ".js"
  , resultsCount: 25
  -- ^ How many results to show by default?
  , penalties: { typeVars: 6
               , match: 2
               , matchConstraint: 1
               , instantiate: 1
               , generalize: 4
               , rowsMismatch: 6
               , mismatch: 10
               , missingConstraint: 1
               , excessiveConstraint: 1
               }
  }
