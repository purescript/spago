module Docs.Search.Config where

import Prelude

import Docs.Search.Types (PackageName(..))

-- | Some magic constants.
config ::
  { version :: String
  , declIndexDirectory :: String
  , mkIndexPartLoadPath :: Int -> String
  , mkIndexPartPath :: Int -> String
  , moduleIndexPath :: String
  , moduleIndexLoadPath :: String
  , metaPath :: String
  , metaLoadPath :: String
  , metaItem :: String
  , groupModulesItem :: String
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
  , defaultPackageName :: PackageName
  }
config =
  { version: "0.0.9"
  , outputDirectory: "output"
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
  , moduleIndexPath: "generated-docs/html/index/modules.js"
  , moduleIndexLoadPath: "./index/modules.js"
  -- ^ Used to load mode index to the browser scope.
  , metaPath: "generated-docs/html/index/meta.js"
  , metaLoadPath: "./index/meta.js"
  , metaItem: "DocsSearchMeta"
  , groupModulesItem: "PureScriptDocsSearchGroupModules"
  -- ^ localStorage key to save sidebar checkbox value to.
  , packageInfoPath: "generated-docs/html/index/packages.js"
  , packageInfoLoadPath: "./index/packages.js"
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
  , defaultPackageName: PackageName "<local package>"
  }
