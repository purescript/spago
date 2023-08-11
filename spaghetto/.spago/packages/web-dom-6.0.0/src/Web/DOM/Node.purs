module Web.DOM.Node
  ( module Exports
  , fromEventTarget
  , toEventTarget
  , nodeType
  , nodeTypeIndex
  , nodeName
  , baseURI
  , ownerDocument
  , parentNode
  , parentElement
  , hasChildNodes
  , childNodes
  , firstChild
  , lastChild
  , previousSibling
  , nextSibling
  , nodeValue
  , setNodeValue
  , textContent
  , setTextContent
  , normalize
  , clone
  , deepClone
  , isEqualNode
  , compareDocumentPositionBits
  , contains
  , lookupPrefix
  , lookupNamespaceURI
  , isDefaultNamespace
  , insertBefore
  , appendChild
  , replaceChild
  , removeChild
  ) where

import Prelude

import Data.Enum (toEnum)
import Data.Maybe (Maybe, fromJust)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document)
import Web.DOM.Element (Element)
import Web.DOM.Internal.Types (Node) as Exports
import Web.DOM.Internal.Types (Node, NodeList)
import Web.DOM.NodeType (NodeType)
import Web.Event.EventTarget (EventTarget)
import Web.Internal.FFI (unsafeReadProtoTagged)

fromEventTarget :: EventTarget -> Maybe Node
fromEventTarget = unsafeReadProtoTagged "Node"

toEventTarget :: Node -> EventTarget
toEventTarget = unsafeCoerce

-- | The type of a node.
nodeType :: Partial => Node -> NodeType
nodeType = fromJust <<< toEnum <<< nodeTypeIndex

-- | The numeric value for the type of a node.
foreign import nodeTypeIndex :: Node -> Int

-- | For elements this is the tag name, for document types this is the doctype
-- | name, for processing instructions this is the target, for all other nodes
-- | it is a string like `"#text"`, `"#comment", etc. depending on the node
-- | type.
foreign import nodeName :: Node -> String

-- | The node's base URL.
foreign import baseURI :: Node -> Effect String

-- | The document the node belongs to, unless the node is a document in which
-- | case the value is Nothing.
ownerDocument :: Node -> Effect (Maybe Document)
ownerDocument = map toMaybe <<< _ownerDocument

foreign import _ownerDocument :: Node -> Effect (Nullable Document)

-- | The parent node of the node.
parentNode :: Node -> Effect (Maybe Node)
parentNode = map toMaybe <<< _parentNode

foreign import _parentNode :: Node -> Effect (Nullable Node)

-- | The parent element of the node.
parentElement :: Node -> Effect (Maybe Element)
parentElement = map toMaybe <<< _parentElement

foreign import _parentElement :: Node -> Effect (Nullable Element)

-- | Indicates whether the node has any child nodes.
foreign import hasChildNodes :: Node -> Effect Boolean

-- | The children of the node.
foreign import childNodes :: Node -> Effect NodeList

-- | The first child of the node, or Nothing if the node has no children.
firstChild :: Node -> Effect (Maybe Node)
firstChild = map toMaybe <<< _firstChild

foreign import _firstChild :: Node -> Effect (Nullable Node)


-- | The last child of the node, or Nothing if the node has no children.
lastChild :: Node -> Effect (Maybe Node)
lastChild = map toMaybe <<< _lastChild

foreign import _lastChild :: Node -> Effect (Nullable Node)

-- | The previous sibling node, or Nothing if there is no previous sibling.
previousSibling :: Node -> Effect (Maybe Node)
previousSibling = map toMaybe <<< _previousSibling

foreign import _previousSibling :: Node -> Effect (Nullable Node)

-- | The next sibling node, or Nothing if there is no next sibling.
nextSibling :: Node -> Effect (Maybe Node)
nextSibling = map toMaybe <<< _nextSibling

foreign import _nextSibling :: Node -> Effect (Nullable Node)

-- | If the node type is text, comment, or processing instruction this is
-- | `Just` the node's data, or `Nothing` in all other cases.
nodeValue :: Node -> Effect (Maybe String)
nodeValue = map toMaybe <<< _nodeValue

foreign import _nodeValue :: Node -> Effect (Nullable String)

-- | If the node type is text, comment, or processing instruction this allows
-- | the node's data to be changed, or has no effect in all other cases.
foreign import setNodeValue :: String -> Node -> Effect Unit

-- | If the node type is document fragment, element, text, processing
-- | instruction, or comment this is the node's data, or null in all other
-- | cases.
foreign import textContent :: Node -> Effect String

-- | If the node type is document fragment, element, text, processing
-- | instruction, or comment this allows the node's data to be changed, or has
-- | no effect in all other cases.
foreign import setTextContent :: String -> Node -> Effect Unit

-- | Removes empty text nodes and then combines any remaining text nodes that
-- | are contiguous.
foreign import normalize :: Node -> Effect Unit

-- | Clones the node without cloning the node's descendants.
foreign import clone :: Node -> Effect Node

-- | Clones the node and its descendants.
foreign import deepClone :: Node -> Effect Node

-- | Checks whether two nodes are equivalent.
foreign import isEqualNode :: Node -> Node -> Effect Boolean

-- TODO: compareDocumentPosition that returns a semigroup or something instead of the bitmask value

-- | Compares the position of two nodes in the document.
foreign import compareDocumentPositionBits :: Node -> Node -> Effect Int

-- | Checks whether the second node is contained within the first
foreign import contains :: Node -> Node -> Effect Boolean

lookupPrefix :: String -> Node -> Effect (Maybe String)
lookupPrefix p = map toMaybe <<< _lookupPrefix p

foreign import _lookupPrefix :: String -> Node -> Effect (Nullable String)

lookupNamespaceURI :: String -> Node -> Effect (Maybe String)
lookupNamespaceURI ns = map toMaybe <<< _lookupNamespaceURI ns

foreign import _lookupNamespaceURI :: String -> Node -> Effect (Nullable String)

foreign import isDefaultNamespace :: String -> Node -> Effect Boolean

-- | Inserts the first node before the second as a child of the third node.
foreign import insertBefore :: Node -> Node -> Node -> Effect Unit

-- | Appends the first node to the child node list of the second node.
foreign import appendChild :: Node -> Node -> Effect Unit

-- | Uses the first node as a replacement for the second node in the children
-- | of the third node.
foreign import replaceChild :: Node -> Node -> Node -> Effect Unit

-- | Removes the first node from the children of the second node.
foreign import removeChild :: Node -> Node -> Effect Unit
