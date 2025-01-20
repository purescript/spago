module Spago.Purs.Types where

import Spago.Core.Prelude

type ModuleGraphWithPackage = Map ModuleName ModuleGraphWithPackageNode

type ModuleGraphWithPackageNode =
  { path :: String
  , depends :: Array ModuleName
  , package :: PackageName
  }

type ModuleName = String

newtype ModuleGraph = ModuleGraph (Map ModuleName ModuleGraphNode)

derive instance Newtype ModuleGraph _

type ModuleGraphNode =
  { path :: String
  , depends :: Array ModuleName
  }
