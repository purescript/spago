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
    \(partId :: Int) -> "/index/declarations/" <> show partId <> ".js"
  , mkIndexPartLoadPath:
    \(partId :: Int) -> "../index/declarations/" <> show partId <> ".js"
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
