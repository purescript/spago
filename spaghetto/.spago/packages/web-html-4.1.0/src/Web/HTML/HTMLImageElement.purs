module Web.HTML.HTMLImageElement
  ( HTMLImageElement
  , fromHTMLElement
  , fromElement
  , fromNode
  , fromChildNode
  , fromNonDocumentTypeChildNode
  , fromParentNode
  , fromEventTarget
  , toHTMLElement
  , toElement
  , toNode
  , toChildNode
  , toNonDocumentTypeChildNode
  , toParentNode
  , toEventTarget
  , create
  , create'
  , alt
  , setAlt
  , src
  , setSrc
  , srcset
  , setSrcset
  , currentSrc
  , sizes
  , setSizes
  , crossOrigin
  , setCrossOrigin
  , useMap
  , setUseMap
  , isMap
  , setIsMap
  , width
  , setWidth
  , height
  , setHeight
  , naturalWidth
  , naturalHeight
  , referrerPolicy
  , setReferrerPolicy
  , decoding
  , setDecoding
  , loading
  , setLoading
  , complete
  ) where

import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Prelude (Unit, map, (<<<), (<=<))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLImageElement.CORSMode (CORSMode)
import Web.HTML.HTMLImageElement.CORSMode as CORSMode
import Web.HTML.HTMLImageElement.DecodingHint (DecodingHint)
import Web.HTML.HTMLImageElement.DecodingHint as DecodingHint
import Web.HTML.HTMLImageElement.Laziness (Laziness)
import Web.HTML.HTMLImageElement.Laziness as Laziness
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLImageElement :: Type

fromHTMLElement :: HTMLElement -> Maybe HTMLImageElement
fromHTMLElement = unsafeReadProtoTagged "HTMLImageElement"

fromElement :: Element -> Maybe HTMLImageElement
fromElement = unsafeReadProtoTagged "HTMLImageElement"

fromNode :: Node -> Maybe HTMLImageElement
fromNode = unsafeReadProtoTagged "HTMLImageElement"

fromChildNode :: ChildNode -> Maybe HTMLImageElement
fromChildNode = unsafeReadProtoTagged "HTMLImageElement"

fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLImageElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLImageElement"

fromParentNode :: ParentNode -> Maybe HTMLImageElement
fromParentNode = unsafeReadProtoTagged "HTMLImageElement"

fromEventTarget :: EventTarget -> Maybe HTMLImageElement
fromEventTarget = unsafeReadProtoTagged "HTMLImageElement"

toHTMLElement :: HTMLImageElement -> HTMLElement
toHTMLElement = unsafeCoerce

toElement :: HTMLImageElement -> Element
toElement = unsafeCoerce

toNode :: HTMLImageElement -> Node
toNode = unsafeCoerce

toChildNode :: HTMLImageElement -> ChildNode
toChildNode = unsafeCoerce

toNonDocumentTypeChildNode :: HTMLImageElement -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

toParentNode :: HTMLImageElement -> ParentNode
toParentNode = unsafeCoerce

toEventTarget :: HTMLImageElement -> EventTarget
toEventTarget = unsafeCoerce

foreign import create :: Effect HTMLImageElement
foreign import createWithDimensions :: Int -> Int -> Effect HTMLImageElement

create' :: Int -> Int -> Effect HTMLImageElement
create' = createWithDimensions

foreign import alt :: HTMLImageElement -> Effect String
foreign import setAlt :: String -> HTMLImageElement -> Effect Unit

foreign import src :: HTMLImageElement -> Effect String
foreign import setSrc :: String -> HTMLImageElement -> Effect Unit

foreign import srcset :: HTMLImageElement -> Effect String
foreign import setSrcset :: String -> HTMLImageElement -> Effect Unit

foreign import currentSrc :: HTMLImageElement -> Effect String

foreign import sizes :: HTMLImageElement -> Effect String
foreign import setSizes :: String -> HTMLImageElement -> Effect Unit

foreign import _crossOrigin :: EffectFn1 HTMLImageElement (Nullable String)

crossOrigin :: HTMLImageElement -> Effect (Maybe CORSMode)
crossOrigin = map (CORSMode.parse <=< Nullable.toMaybe) <<< runEffectFn1 _crossOrigin

foreign import _setCrossOrigin :: EffectFn2 String HTMLImageElement Unit

setCrossOrigin :: CORSMode -> HTMLImageElement -> Effect Unit
setCrossOrigin mode = runEffectFn2 _setCrossOrigin (CORSMode.print mode)

foreign import useMap :: HTMLImageElement -> Effect String
foreign import setUseMap :: String -> HTMLImageElement -> Effect Unit

foreign import isMap :: HTMLImageElement -> Effect Boolean
foreign import setIsMap :: Boolean -> HTMLImageElement -> Effect Unit

foreign import width :: HTMLImageElement -> Effect Int
foreign import setWidth :: Int -> HTMLImageElement -> Effect Unit

foreign import height :: HTMLImageElement -> Effect Int
foreign import setHeight :: Int -> HTMLImageElement -> Effect Unit

foreign import naturalWidth :: HTMLImageElement -> Effect Int
foreign import naturalHeight :: HTMLImageElement -> Effect Int

foreign import referrerPolicy :: HTMLImageElement -> Effect String
foreign import setReferrerPolicy :: String -> HTMLImageElement -> Effect Unit

foreign import _decoding :: EffectFn1 HTMLImageElement String

decoding :: HTMLImageElement -> Effect DecodingHint
decoding = map (fromMaybe DecodingHint.Auto <<< DecodingHint.parse) <<< runEffectFn1 _decoding

foreign import _setDecoding :: EffectFn2 String HTMLImageElement Unit

setDecoding :: DecodingHint -> HTMLImageElement -> Effect Unit
setDecoding hint = runEffectFn2 _setDecoding (DecodingHint.print hint)

foreign import _loading :: EffectFn1 HTMLImageElement String

loading :: HTMLImageElement -> Effect Laziness
loading = map (fromMaybe Laziness.Eager <<< Laziness.parse) <<< runEffectFn1 _loading

foreign import _setLoading :: EffectFn2 String HTMLImageElement Unit

setLoading :: Laziness -> HTMLImageElement -> Effect Unit
setLoading laziness = runEffectFn2 _setLoading (Laziness.print laziness)

foreign import complete :: HTMLImageElement -> Effect Boolean
