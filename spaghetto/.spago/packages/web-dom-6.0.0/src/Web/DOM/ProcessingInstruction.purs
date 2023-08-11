module Web.DOM.ProcessingInstruction where

import Data.Maybe (Maybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.CharacterData (CharacterData)
import Web.DOM.ChildNode (ChildNode)
import Web.DOM.Internal.Types (Node)
import Web.DOM.NonDocumentTypeChildNode (NonDocumentTypeChildNode)
import Web.Event.EventTarget (EventTarget)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data ProcessingInstruction :: Type

fromCharacterData :: CharacterData -> Maybe ProcessingInstruction
fromCharacterData = unsafeReadProtoTagged "ProcessingInstruction"

fromNode :: Node -> Maybe ProcessingInstruction
fromNode = unsafeReadProtoTagged "ProcessingInstruction"

fromChildNode :: ChildNode -> Maybe ProcessingInstruction
fromChildNode = unsafeReadProtoTagged "ProcessingInstruction"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe ProcessingInstruction
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "ProcessingInstruction"

fromEventTarget :: EventTarget -> Maybe ProcessingInstruction
fromEventTarget = unsafeReadProtoTagged "ProcessingInstruction"

toNode :: ProcessingInstruction -> Node
toNode = unsafeCoerce

toCharacterData :: ProcessingInstruction -> CharacterData
toCharacterData = unsafeCoerce

toChildNode :: ProcessingInstruction -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: ProcessingInstruction -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toEventTarget :: ProcessingInstruction -> EventTarget
toEventTarget = unsafeCoerce

foreign import target :: ProcessingInstruction -> Effect String
