module Web.DOM.NodeList
  ( module Exports
  , length
  , item
  , toArray
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Web.DOM.Internal.Types (Node, NodeList)
import Web.DOM.Internal.Types (NodeList) as Exports

-- | The number of items in a NodeList.
foreign import length :: NodeList -> Effect Int

-- | The elements of a NodeList represented in an array.
foreign import toArray :: NodeList -> Effect (Array Node)

-- | The item in a NodeList at the specified index, or Nothing if no such node
-- | exists.
item :: Int -> NodeList -> Effect (Maybe Node)
item i = map toMaybe <<< _item i

foreign import _item :: Int -> NodeList -> Effect (Nullable Node)
