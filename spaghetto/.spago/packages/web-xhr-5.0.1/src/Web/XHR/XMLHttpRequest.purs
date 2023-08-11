module Web.XHR.XMLHttpRequest
  ( XMLHttpRequest
  , toEventTarget
  , xmlHttpRequest
  , abort
  , getAllResponseHeaders
  , getResponseHeader
  , open
  , open'
  , overrideMimeType
  , send
  , sendString
  , sendDocument
  , sendBlob
  , sendArrayView
  , sendFormData
  , setRequestHeader
  , readyState
  , response
  , responseURL
  , status
  , statusText
  , timeout
  , setTimeout
  , upload
  , withCredentials
  , setWithCredentials
  ) where

import Prelude

import Data.ArrayBuffer.Types (ArrayView)
import Data.Either (Either)
import Data.Enum (toEnum)
import Data.HTTP.Method (CustomMethod, Method)
import Data.HTTP.Method as Method
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Newtype (un)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Uncurried as Fn
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document)
import Web.Event.EventTarget (EventTarget)
import Web.File.Blob (Blob)
import Web.XHR.FormData (FormData)
import Web.XHR.ReadyState (ReadyState(..))
import Web.XHR.ResponseType (ResponseType)
import Web.XHR.XMLHttpRequestUpload (XMLHttpRequestUpload)

-- | `XMLHttpRequest`s are indexed by their `ResponseType`
foreign import data XMLHttpRequest :: Type -> Type

toEventTarget :: forall res. XMLHttpRequest res -> EventTarget
toEventTarget = unsafeCoerce

xmlHttpRequest :: forall res. ResponseType res -> Effect (XMLHttpRequest res)
xmlHttpRequest = Fn.runEffectFn1 _xmlHttpRequest

abort :: forall res. XMLHttpRequest res -> Effect Unit
abort = Fn.runEffectFn1 _abort

getAllResponseHeaders :: forall res. XMLHttpRequest res -> Effect (Maybe String)
getAllResponseHeaders xhr = toMaybe <$> Fn.runEffectFn1 _getAllResponseHeaders xhr

getResponseHeader :: forall res. String -> XMLHttpRequest res -> Effect (Maybe String)
getResponseHeader header xhr = toMaybe <$> Fn.runEffectFn2 _getResponseHeader header xhr

open :: forall res. Either Method CustomMethod -> String -> XMLHttpRequest res -> Effect Unit
open method url xhr = Fn.runEffectFn5 _open (Method.print method) url (toNullable Nothing) (toNullable Nothing) xhr

open' :: forall res. { method :: Either Method CustomMethod, url :: String, username :: Maybe String, password :: Maybe String } -> XMLHttpRequest res -> Effect Unit
open' options xhr = Fn.runEffectFn5 _open (Method.print options.method) options.url (toNullable options.username) (toNullable options.password) xhr

overrideMimeType :: forall res. MediaType -> XMLHttpRequest res -> Effect Unit
overrideMimeType ty req = Fn.runEffectFn2 _overrideMimeType (un MediaType ty) req

send :: forall res. XMLHttpRequest res -> Effect Unit
send xhr = Fn.runEffectFn2 _send (toNullable Nothing) xhr

sendString :: forall res. String -> XMLHttpRequest res -> Effect Unit
sendString payload xhr = Fn.runEffectFn2 _send payload xhr

sendBlob :: forall res. Blob -> XMLHttpRequest res -> Effect Unit
sendBlob payload xhr = Fn.runEffectFn2 _send payload xhr

sendArrayView :: forall a res. ArrayView a -> XMLHttpRequest res -> Effect Unit
sendArrayView payload xhr = Fn.runEffectFn2 _send payload xhr

sendFormData :: forall res. FormData -> XMLHttpRequest res -> Effect Unit
sendFormData payload xhr = Fn.runEffectFn2 _send payload xhr

sendDocument :: forall res. Document -> XMLHttpRequest res -> Effect Unit
sendDocument payload xhr = Fn.runEffectFn2 _send payload xhr

setRequestHeader :: forall res. String -> String -> XMLHttpRequest res -> Effect Unit
setRequestHeader header value xhr = Fn.runEffectFn3 _setRequestHeader header value xhr

readyState :: forall res. XMLHttpRequest res -> Effect ReadyState
readyState xhr = toReadyState <$> Fn.runEffectFn2 _getProperty "readyState" xhr
  where
  toReadyState :: Foreign -> ReadyState
  toReadyState rs = fromMaybe Unsent $ toEnum (unsafeCoerce rs :: Int)

response :: forall res. XMLHttpRequest res -> Effect (Maybe res)
response xhr = toMaybe <$> Fn.runEffectFn2 _getProperty "response" xhr

responseURL :: forall res. XMLHttpRequest res -> Effect String
responseURL xhr = Fn.runEffectFn2 _getProperty "responseURL" xhr

status :: forall res. XMLHttpRequest res -> Effect Int
status xhr = Fn.runEffectFn2 _getProperty "status" xhr

statusText :: forall res. XMLHttpRequest res -> Effect String
statusText xhr = Fn.runEffectFn2 _getProperty "statusText" xhr

timeout :: forall res. XMLHttpRequest res -> Effect Milliseconds
timeout xhr = Fn.runEffectFn2 _getProperty "timeout" xhr

setTimeout :: forall res. Milliseconds -> XMLHttpRequest res -> Effect Unit
setTimeout ms xhr = Fn.runEffectFn3 _setProperty "timeout" ms xhr

upload :: forall res. XMLHttpRequest res -> Effect XMLHttpRequestUpload
upload xhr = Fn.runEffectFn2 _getProperty "upload" xhr

withCredentials :: forall res. XMLHttpRequest res -> Effect Boolean
withCredentials xhr = Fn.runEffectFn2 _getProperty "withCredentials" xhr

setWithCredentials :: forall res. Boolean -> XMLHttpRequest res -> Effect Unit
setWithCredentials wc xhr = Fn.runEffectFn3 _setProperty "withCredentials" wc xhr

foreign import _xmlHttpRequest
  :: forall res
   . Fn.EffectFn1 (ResponseType res) (XMLHttpRequest res)

foreign import _abort
  :: forall res
   . Fn.EffectFn1 (XMLHttpRequest res) Unit

foreign import _getAllResponseHeaders
  :: forall res
   . Fn.EffectFn1 (XMLHttpRequest res) (Nullable String)

foreign import _getResponseHeader
  :: forall res
   . Fn.EffectFn2 String (XMLHttpRequest res) (Nullable String)

foreign import _open
  :: forall res
   . Fn.EffectFn5 String String (Nullable String) (Nullable String) (XMLHttpRequest res) Unit

foreign import _overrideMimeType
  :: forall res
   . Fn.EffectFn2 String (XMLHttpRequest res) Unit

foreign import _send
  :: forall res a
   . Fn.EffectFn2 a (XMLHttpRequest res) Unit

foreign import _setRequestHeader
  :: forall res
   . Fn.EffectFn3 String String (XMLHttpRequest res) Unit

foreign import _setProperty
  :: forall res a
   . Fn.EffectFn3 String a (XMLHttpRequest res) Unit

foreign import _getProperty
  :: forall res a
   . Fn.EffectFn2 String (XMLHttpRequest res) a
