module Spago.Purs.Graph where

import Spago.Prelude
import Spago.Types

newtype ModuleGraph = ModuleGraph { unModuleGraph :: Map ModuleName ModuleGraphNode }
  deriving newtype (FromJSON)

data ModuleGraphNode = ModuleGraphNode
  { path :: Text
  , depends :: [ModuleName]
  } deriving (Generic)

instance FromJSON ModuleGraphNode
