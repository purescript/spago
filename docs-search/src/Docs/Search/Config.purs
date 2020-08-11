module Docs.Search.Config where

import Docs.Search.Types (GlobalIdentifier, PackageName, PartId, URL, FilePath)

import Prelude

import Data.Newtype (wrap)

version :: String
version = "0.0.10"

mkShapeScriptPath :: String -> String
mkShapeScriptPath shape = "./index/types/" <> shape <> ".js"

-- | In how many parts the index should be splitted?
numberOfIndexParts :: Int
numberOfIndexParts = 50

mkIndexPartPath :: PartId -> String
mkIndexPartPath partId = "html/index/declarations/" <> show partId <> ".js"

mkIndexPartLoadPath :: PartId -> URL
mkIndexPartLoadPath partId = wrap $ "./index/declarations/" <> show partId <> ".js"

moduleIndexPath :: FilePath
moduleIndexPath = wrap "generated-docs/html/index/modules.js"

-- | Used to load mode index to the browser scope.
moduleIndexLoadPath :: String
moduleIndexLoadPath = "./index/modules.js"

typeIndexDirectory :: FilePath
typeIndexDirectory = wrap "generated-docs/html/index/types"

metaPath :: FilePath
metaPath = wrap "generated-docs/html/index/meta.js"

metaLoadPath :: URL
metaLoadPath = wrap "./index/meta.js"

metaItem :: GlobalIdentifier
metaItem = wrap "DocsSearchMeta"

-- | localStorage key to save sidebar checkbox value to.
groupModulesItem :: String
groupModulesItem = "PureScriptDocsSearchGroupModules"

packageInfoPath :: FilePath
packageInfoPath = wrap "generated-docs/html/index/packages.js"

packageInfoItem :: GlobalIdentifier
packageInfoItem = wrap "DocsSearchPackageIndex"

packageInfoLoadPath :: URL
packageInfoLoadPath = wrap "./index/packages.js"

-- | How many results to show by default?
resultsCount :: Int
resultsCount = 25

-- | Penalties used to determine how "far" a type query is from a given type.
-- See Docs.Search.TypeQuery
penalties ::
  { excessiveConstraint :: Int
  , generalize :: Int
  , instantiate :: Int
  , match :: Int
  , matchConstraint :: Int
  , missingConstraint :: Int
  , rowsMismatch :: Int
  , typeVars :: Int
  }
penalties =
  { typeVars: 2
  , match: 2
  , matchConstraint: 1
  , instantiate: 2
  , generalize: 2
  , rowsMismatch: 3
  , missingConstraint: 1
  , excessiveConstraint: 1
  }

defaultPackageName :: PackageName
defaultPackageName = wrap "<local package>"
