module Web.DOM.DocumentFragment where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.ChildNode (ChildNode)
import Web.DOM.Internal.Types (Node)
import Web.DOM.NonElementParentNode (NonElementParentNode)
import Web.DOM.ParentNode (ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data DocumentFragment :: Type

fromNode :: Node -> Maybe DocumentFragment
fromNode = unsafeReadProtoTagged "DocumentFragment"

fromChildNode :: ChildNode -> Maybe DocumentFragment
fromChildNode = unsafeReadProtoTagged "DocumentFragment"

fromParentNode :: ParentNode -> Maybe DocumentFragment
fromParentNode = unsafeReadProtoTagged "DocumentFragment"

fromNonElementParentNode :: NonElementParentNode -> Maybe DocumentFragment
fromNonElementParentNode = unsafeReadProtoTagged "DocumentFragment"

fromEventTarget :: EventTarget -> Maybe DocumentFragment
fromEventTarget = unsafeReadProtoTagged "DocumentFragment"

toNode :: DocumentFragment -> Node
toNode = unsafeCoerce

toChildNode :: DocumentFragment -> ChildNode
toChildNode = unsafeCoerce

toParentNode :: DocumentFragment -> ParentNode
toParentNode = unsafeCoerce

toNonElementParentNode :: DocumentFragment -> NonElementParentNode
toNonElementParentNode = unsafeCoerce

toEventTarget :: DocumentFragment -> EventTarget
toEventTarget = unsafeCoerce
