module Web.DOM.Comment where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.CharacterData (CharacterData)
import Web.DOM.ChildNode (ChildNode)
import Web.DOM.Internal.Types (Node)
import Web.DOM.NonDocumentTypeChildNode (NonDocumentTypeChildNode)
import Web.Event.EventTarget (EventTarget)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data Comment :: Type

fromCharacterData :: CharacterData -> Maybe Comment
fromCharacterData = unsafeReadProtoTagged "Comment"

fromNode :: Node -> Maybe Comment
fromNode = unsafeReadProtoTagged "Comment"

fromChildNode :: ChildNode -> Maybe Comment
fromChildNode = unsafeReadProtoTagged "Comment"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe Comment
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "Comment"

fromEventTarget :: EventTarget -> Maybe Comment
fromEventTarget = unsafeReadProtoTagged "Comment"

toCharacterData :: Comment -> CharacterData
toCharacterData = unsafeCoerce

toNode :: Comment -> Node
toNode = unsafeCoerce

toChildNode :: Comment -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: Comment -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toEventTarget :: Comment -> EventTarget
toEventTarget = unsafeCoerce
