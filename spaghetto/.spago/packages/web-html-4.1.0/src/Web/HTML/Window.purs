module Web.HTML.Window
  ( Window
  , toEventTarget
  , fromEventTarget
  , document
  , navigator
  , location
  , history
  , innerWidth
  , innerHeight
  , alert
  , confirm
  , moveBy
  , moveTo
  , open
  , close
  , outerHeight
  , outerWidth
  , print
  , prompt
  , promptDefault
  , resizeBy
  , resizeTo
  , screenX
  , screenY
  , scroll
  , scrollBy
  , scrollX
  , scrollY
  , localStorage
  , sessionStorage
  , requestAnimationFrame
  , cancelAnimationFrame
  , RequestAnimationFrameId
  , requestIdleCallback
  , cancelIdleCallback
  , RequestIdleCallbackId
  , parent
  , opener
  ) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Prelude (class Eq, class Ord, Unit, (<$>))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.History (History)
import Web.HTML.Location (Location)
import Web.HTML.Navigator (Navigator)
import Web.Internal.FFI (unsafeReadProtoTagged)
import Web.Storage.Storage (Storage)

foreign import data Window :: Type

toEventTarget :: Window -> EventTarget
toEventTarget = unsafeCoerce

fromEventTarget :: EventTarget -> Maybe Window
fromEventTarget = unsafeReadProtoTagged "Window"

foreign import document :: Window -> Effect HTMLDocument

foreign import navigator :: Window -> Effect Navigator

foreign import location :: Window -> Effect Location

foreign import history :: Window -> Effect History

foreign import innerWidth :: Window -> Effect Int

foreign import innerHeight :: Window -> Effect Int

foreign import alert :: String -> Window -> Effect Unit

foreign import confirm :: String -> Window -> Effect Boolean

foreign import moveBy :: Int -> Int -> Window -> Effect Unit

foreign import moveTo :: Int -> Int -> Window -> Effect Unit

open :: String -> String -> String -> Window -> Effect (Maybe Window)
open url' name features window = toMaybe <$> _open url' name features window

foreign import _open
  :: String
  -> String
  -> String
  -> Window
  -> Effect (Nullable Window)

foreign import close :: Window -> Effect Unit

foreign import outerHeight :: Window -> Effect Int

foreign import outerWidth :: Window -> Effect Int

foreign import print :: Window -> Effect Unit

prompt :: String -> Window -> Effect (Maybe String)
prompt msg window = toMaybe <$> _prompt msg "" window

promptDefault :: String -> String -> Window -> Effect (Maybe String)
promptDefault msg defaultText window = toMaybe <$> _prompt msg defaultText window

foreign import _prompt :: String -> String -> Window -> Effect (Nullable String)

foreign import resizeBy :: Int -> Int -> Window -> Effect Unit

foreign import resizeTo :: Int -> Int -> Window -> Effect Unit

foreign import screenX :: Window -> Effect Int

foreign import screenY :: Window -> Effect Int

foreign import scroll :: Int -> Int -> Window -> Effect Unit

foreign import scrollBy :: Int -> Int -> Window -> Effect Unit

foreign import scrollX :: Window -> Effect Number

foreign import scrollY :: Window -> Effect Number

foreign import localStorage :: Window -> Effect Storage

foreign import sessionStorage :: Window -> Effect Storage

newtype RequestAnimationFrameId = RequestAnimationFrameId Int

derive instance eqRequestAnimationFrameId :: Eq RequestAnimationFrameId
derive instance ordRequestAnimationFrameId :: Ord RequestAnimationFrameId

foreign import requestAnimationFrame :: Effect Unit -> Window -> Effect RequestAnimationFrameId

foreign import cancelAnimationFrame :: RequestAnimationFrameId -> Window -> Effect Unit

newtype RequestIdleCallbackId = RequestIdleCallbackId Int

derive instance eqRequestIdleCallbackId :: Eq RequestIdleCallbackId
derive instance ordRequestIdleCallbackId :: Ord RequestIdleCallbackId

-- | Set timeout to `0` to get the same behaviour as when it is `undefined` in
-- | [JavaScript](https://w3c.github.io/requestidlecallback/#h-the-requestidle-callback-method).
foreign import requestIdleCallback :: { timeout :: Int } -> Effect Unit -> Window -> Effect RequestIdleCallbackId

foreign import cancelIdleCallback :: RequestIdleCallbackId -> Window -> Effect Unit

foreign import parent :: Window -> Effect Window

foreign import _opener :: Window -> Effect (Nullable Window)

opener :: Window -> Effect (Maybe Window)
opener window = toMaybe <$> _opener window
