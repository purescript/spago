module Affjax.Node
  ( driver
  , request
  , get
  , post
  , post_
  , put
  , put_
  , delete
  , delete_
  , patch
  , patch_
  , module Exports
  ) where

import Prelude

import Affjax (Request, defaultRequest, Response, Error(..), printError, URL) as Exports
import Affjax as AX
import Affjax (Error, Request, Response, URL, AffjaxDriver)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

-- | Provides an Affjax driver that only works on Node.js.
-- | Using this in a browser environment will cause errors.
foreign import driver :: AffjaxDriver

-- | Makes a `GET` request to the specified URL.
get :: forall a. ResponseFormat.ResponseFormat a -> URL -> Aff (Either Error (Response a))
get = AX.get driver

-- | Makes a `POST` request to the specified URL with the option to send data.
post :: forall a. ResponseFormat.ResponseFormat a -> URL -> Maybe RequestBody.RequestBody -> Aff (Either Error (Response a))
post = AX.post driver

-- | Makes a `POST` request to the specified URL with the option to send data
-- | and ignores the response body.
post_ :: URL -> Maybe RequestBody.RequestBody -> Aff (Either Error Unit)
post_ = AX.post_ driver

-- | Makes a `PUT` request to the specified URL with the option to send data.
put :: forall a. ResponseFormat.ResponseFormat a -> URL -> Maybe RequestBody.RequestBody -> Aff (Either Error (Response a))
put = AX.put driver

-- | Makes a `PUT` request to the specified URL with the option to send data
-- | and ignores the response body.
put_ :: URL -> Maybe RequestBody.RequestBody -> Aff (Either Error Unit)
put_ = AX.put_ driver

-- | Makes a `DELETE` request to the specified URL.
delete :: forall a. ResponseFormat.ResponseFormat a -> URL -> Aff (Either Error (Response a))
delete = AX.delete driver

-- | Makes a `DELETE` request to the specified URL and ignores the response
-- | body.
delete_ :: URL -> Aff (Either Error Unit)
delete_ = AX.delete_ driver

-- | Makes a `PATCH` request to the specified URL with the option to send data.
patch :: forall a. ResponseFormat.ResponseFormat a -> URL -> RequestBody.RequestBody -> Aff (Either Error (Response a))
patch = AX.patch driver

-- | Makes a `PATCH` request to the specified URL with the option to send data
-- | and ignores the response body.
patch_ :: URL -> RequestBody.RequestBody -> Aff (Either Error Unit)
patch_ = AX.patch_ driver

-- | Makes an HTTP request.
-- |
-- | The example below performs a `GET` request to the URL `/resource` and
-- | interprets the response body as JSON.
-- |
-- | ```purescript
-- | import Affjax.ResponseFormat (json)
-- | ...
-- | request (defaultRequest { url = "/resource", method = Left GET, responseFormat = json})
-- | ```
-- |
-- | For common cases helper functions can often be used insteAX of `request` .
-- | For instance, the above example is equivalent to the following.
-- |
-- | ```purescript
-- | get json "/resource"
-- | ```
request :: forall a. Request a -> Aff (Either Error (Response a))
request = AX.request driver
