module Web.DOM.Text where

import Data.Maybe (Maybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.CharacterData (CharacterData)
import Web.DOM.ChildNode (ChildNode)
import Web.DOM.Internal.Types (Node)
import Web.DOM.NonDocumentTypeChildNode (NonDocumentTypeChildNode)
import Web.Event.EventTarget (EventTarget)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data Text :: Type

fromCharacterData :: CharacterData -> Maybe Text
fromCharacterData = unsafeReadProtoTagged "Text"

fromNode :: Node -> Maybe Text
fromNode = unsafeReadProtoTagged "Text"

fromChildNode :: ChildNode -> Maybe Text
fromChildNode = unsafeReadProtoTagged "Text"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe Text
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "Text"

fromEventTarget :: EventTarget -> Maybe Text
fromEventTarget = unsafeReadProtoTagged "Text"

toNode :: Text -> Node
toNode = unsafeCoerce

toCharacterData :: Text -> CharacterData
toCharacterData = unsafeCoerce

toChildNode :: Text -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: Text -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toEventTarget :: Text -> EventTarget
toEventTarget = unsafeCoerce

foreign import splitText :: Int -> Text -> Effect Text

foreign import wholeText :: Text -> Effect String
