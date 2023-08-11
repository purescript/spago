module Web.DOM.CharacterData where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.ChildNode (ChildNode)
import Web.DOM.Internal.Types (Node)
import Web.DOM.NonDocumentTypeChildNode (NonDocumentTypeChildNode)
import Web.Event.EventTarget (EventTarget)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data CharacterData :: Type

fromNode :: Node -> Maybe CharacterData
fromNode = unsafeReadProtoTagged "CharacterData"

fromChildNode :: ChildNode -> Maybe CharacterData
fromChildNode = unsafeReadProtoTagged "CharacterData"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe CharacterData
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "CharacterData"

fromEventTarget :: EventTarget -> Maybe CharacterData
fromEventTarget = unsafeReadProtoTagged "CharacterData"

toNode :: CharacterData -> Node
toNode = unsafeCoerce

toChildNode :: CharacterData -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: CharacterData -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toEventTarget :: CharacterData -> EventTarget
toEventTarget = unsafeCoerce

foreign import data_ :: CharacterData -> Effect String

foreign import length :: CharacterData -> Effect Int

foreign import substringData :: Int -> Int -> CharacterData -> Effect String

foreign import appendData :: String -> CharacterData -> Effect Unit

foreign import insertData :: Int -> String -> CharacterData -> Effect Unit

foreign import deleteData :: Int -> Int -> CharacterData -> Effect Unit

foreign import replaceData :: Int -> Int -> String -> CharacterData -> Effect Unit
