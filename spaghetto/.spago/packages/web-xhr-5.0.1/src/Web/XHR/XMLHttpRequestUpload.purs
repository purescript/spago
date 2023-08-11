module Web.XHR.XMLHttpRequestUpload where

import Web.Event.EventTarget (EventTarget)
import Unsafe.Coerce (unsafeCoerce)

foreign import data XMLHttpRequestUpload :: Type

toEventTarget :: XMLHttpRequestUpload -> EventTarget
toEventTarget = unsafeCoerce
