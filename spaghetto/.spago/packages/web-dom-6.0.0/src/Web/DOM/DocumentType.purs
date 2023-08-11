module Web.DOM.DocumentType where

import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.ChildNode (ChildNode)
import Web.DOM.Internal.Types (Node)
import Web.Event.EventTarget (EventTarget)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data DocumentType :: Type

fromNode :: Node -> Maybe DocumentType
fromNode = unsafeReadProtoTagged "DocumentType"

fromChildNode :: ChildNode -> Maybe DocumentType
fromChildNode = unsafeReadProtoTagged "DocumentType"

fromEventTarget :: EventTarget -> Maybe DocumentType
fromEventTarget = unsafeReadProtoTagged "DocumentType"

toNode :: DocumentType -> Node
toNode = unsafeCoerce

toChildNode :: DocumentType -> ChildNode
toChildNode = unsafeCoerce

toEventTarget :: DocumentType -> EventTarget
toEventTarget = unsafeCoerce

foreign import name :: DocumentType -> String
foreign import publicId :: DocumentType -> String
foreign import systemId :: DocumentType -> String
