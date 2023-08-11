-- | Note: this module is not intended to be used by end-users.
-- | Rather, use the environment-specific library instead:
-- | - [`purescript-affjax-node`](https://github.com/purescript-contrib/purescript-affjax-node)
-- | - [`purescript-affjax-web`](https://github.com/purescript-contrib/purescript-affjax-web)
-- |
-- | You should use this module if you are writing a driver for a specific environment.
-- | See this module's source code for more details.
-- If you want to write a driver for an environment, see the comments
-- for the `AffjaxDriver` type and look at the `node` and `web` libraries
-- (linked above) as examples.
module Affjax
  ( Request
  , defaultRequest
  , Response
  , Error(..)
  , printError
  , URL
  , AffjaxDriver
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
  ) where

import Prelude

import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.RequestHeader as RequestHeader
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader(..))
import Affjax.StatusCode (StatusCode)
import Control.Alt ((<|>))
import Control.Monad.Except (Except, runExcept)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Arr
import Data.ArrayBuffer.Types (ArrayView)
import Data.Either (Either(..), either, note)
import Data.Foldable (any)
import Data.FormURLEncoded as FormURLEncoded
import Data.Function (on)
import Data.Function.Uncurried (Fn5, runFn5)
import Data.HTTP.Method (Method(..), CustomMethod)
import Data.HTTP.Method as Method
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toNullable)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, try)
import Effect.Aff.Compat as AC
import Effect.Exception as Exn
import Foreign (Foreign, ForeignError(..), fail, renderForeignError, unsafeReadTagged, unsafeToForeign)
import Web.DOM (Document)
import Web.File.Blob (Blob)
import Web.XHR.FormData (FormData)

-- | A record that contains all the information to perform an HTTP request.
-- | Instead of constructing the record from scratch it is often easier to build
-- | one based on `defaultRequest`.
type Request a =
  { method :: Either Method CustomMethod
  , url :: URL
  , headers :: Array RequestHeader
  , content :: Maybe RequestBody.RequestBody
  , username :: Maybe String
  , password :: Maybe String
  , withCredentials :: Boolean
  , responseFormat :: ResponseFormat.ResponseFormat a
  , timeout :: Maybe Milliseconds
  }

-- | A record of the type `Request` that has all fields set to default
-- | values. This record can be used as the foundation for constructing
-- | custom requests.
-- |
-- | As an example:
-- |
-- | ```purescript
-- | defaultRequest { url = "/api/user", method = Left POST }
-- | ```
-- |
-- | Would represents a POST request to the URL `/api/user`.
defaultRequest :: Request Unit
defaultRequest =
  { method: Left GET
  , url: "/"
  , headers: []
  , content: Nothing
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: ResponseFormat.ignore
  , timeout: Nothing
  }

-- | The possible errors that can occur when making an Affjax request.
data Error
  = RequestContentError String
  | ResponseBodyError ForeignError (Response Foreign)
  | TimeoutError
  | RequestFailedError
  | XHROtherError Exn.Error

printError :: Error -> String
printError = case _ of
  RequestContentError err ->
    "There was a problem with the request content: " <> err
  ResponseBodyError err _ ->
    "There was a problem with the response body: " <> renderForeignError err
  TimeoutError ->
    "There was a problem making the request: timeout"
  RequestFailedError ->
    "There was a problem making the request: request failed"
  XHROtherError err ->
    "There was a problem making the request: " <> Exn.message err

-- | The type of records that represents a received HTTP response.
type Response a =
  { status :: StatusCode
  , statusText :: String
  , headers :: Array ResponseHeader
  , body :: a
  }

-- | Type alias for URL strings to aid readability of types.
type URL = String

-- | Makes a `GET` request to the specified URL.
get :: forall a. AffjaxDriver -> ResponseFormat.ResponseFormat a -> URL -> Aff (Either Error (Response a))
get driver rf u = request driver (defaultRequest { url = u, responseFormat = rf })

-- | Makes a `POST` request to the specified URL with the option to send data.
post :: forall a. AffjaxDriver -> ResponseFormat.ResponseFormat a -> URL -> Maybe RequestBody.RequestBody -> Aff (Either Error (Response a))
post driver rf u c = request driver (defaultRequest { method = Left POST, url = u, content = c, responseFormat = rf })

-- | Makes a `POST` request to the specified URL with the option to send data
-- | and ignores the response body.
post_ :: AffjaxDriver -> URL -> Maybe RequestBody.RequestBody -> Aff (Either Error Unit)
post_ driver url = map void <<< post driver ResponseFormat.ignore url

-- | Makes a `PUT` request to the specified URL with the option to send data.
put :: forall a. AffjaxDriver -> ResponseFormat.ResponseFormat a -> URL -> Maybe RequestBody.RequestBody -> Aff (Either Error (Response a))
put driver rf u c = request driver (defaultRequest { method = Left PUT, url = u, content = c, responseFormat = rf })

-- | Makes a `PUT` request to the specified URL with the option to send data
-- | and ignores the response body.
put_ :: AffjaxDriver -> URL -> Maybe RequestBody.RequestBody -> Aff (Either Error Unit)
put_ driver url = map void <<< put driver ResponseFormat.ignore url

-- | Makes a `DELETE` request to the specified URL.
delete :: forall a. AffjaxDriver -> ResponseFormat.ResponseFormat a -> URL -> Aff (Either Error (Response a))
delete driver rf u = request driver (defaultRequest { method = Left DELETE, url = u, responseFormat = rf })

-- | Makes a `DELETE` request to the specified URL and ignores the response
-- | body.
delete_ :: AffjaxDriver -> URL -> Aff (Either Error Unit)
delete_ driver = map void <<< delete driver ResponseFormat.ignore

-- | Makes a `PATCH` request to the specified URL with the option to send data.
patch :: forall a. AffjaxDriver -> ResponseFormat.ResponseFormat a -> URL -> RequestBody.RequestBody -> Aff (Either Error (Response a))
patch driver rf u c = request driver (defaultRequest { method = Left PATCH, url = u, content = Just c, responseFormat = rf })

-- | Makes a `PATCH` request to the specified URL with the option to send data
-- | and ignores the response body.
patch_ :: AffjaxDriver -> URL -> RequestBody.RequestBody -> Aff (Either Error Unit)
patch_ driver url = map void <<< patch driver ResponseFormat.ignore url

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
-- | For common cases helper functions can often be used instead of `request` .
-- | For instance, the above example is equivalent to the following.
-- |
-- | ```purescript
-- | get json "/resource"
-- | ```
request :: forall a. AffjaxDriver -> Request a -> Aff (Either Error (Response a))
request driver req =
  case req.content of
    Nothing ->
      send (toNullable Nothing)
    Just content ->
      case extractContent content of
        Right c ->
          send (toNullable (Just c))
        Left err ->
          pure $ Left (RequestContentError err)
  where
  send :: Nullable Foreign -> Aff (Either Error (Response a))
  send content =
    try (AC.fromEffectFnAff (runFn5 _ajax driver timeoutErrorMessageIdent requestFailedMessageIdent ResponseHeader (ajaxRequest content))) <#> case _ of
      Right res ->
        case runExcept (fromResponse res.body) of
          Left err -> Left (ResponseBodyError (NEL.head err) res)
          Right body -> Right (res { body = body })
      Left err -> Left do
        let message = Exn.message err
        if message == timeoutErrorMessageIdent then
          TimeoutError
        else if message == requestFailedMessageIdent then
          RequestFailedError
        else
          XHROtherError err

  ajaxRequest :: Nullable Foreign -> AjaxRequest a
  ajaxRequest =
    { method: Method.print req.method
    , url: req.url
    , headers: (\h -> { field: RequestHeader.name h, value: RequestHeader.value h }) <$> headers req.content
    , content: _
    , responseType: ResponseFormat.toResponseType req.responseFormat
    , username: toNullable req.username
    , password: toNullable req.password
    , withCredentials: req.withCredentials
    , timeout: fromMaybe 0.0 $ (\(Milliseconds x) -> x) <$> req.timeout
    }

  extractContent :: RequestBody.RequestBody -> Either String Foreign
  extractContent = case _ of
    RequestBody.ArrayView f ->
      Right $ f (unsafeToForeign :: forall b. ArrayView b -> Foreign)
    RequestBody.Blob x ->
      Right $ (unsafeToForeign :: Blob -> Foreign) x
    RequestBody.Document x ->
      Right $ (unsafeToForeign :: Document -> Foreign) x
    RequestBody.String x ->
      Right $ (unsafeToForeign :: String -> Foreign) x
    RequestBody.FormData x ->
      Right $ (unsafeToForeign :: FormData -> Foreign) x
    RequestBody.FormURLEncoded x -> do
      note "Body contains values that cannot be encoded as application/x-www-form-urlencoded"
        $ (unsafeToForeign :: String -> Foreign) <$> FormURLEncoded.encode x
    RequestBody.Json x ->
      Right $ (unsafeToForeign :: String -> Foreign) (J.stringify x)

  headers :: Maybe RequestBody.RequestBody -> Array RequestHeader
  headers reqContent =
    addHeader (ContentType <$> (RequestBody.toMediaType =<< reqContent))
      $ addHeader (Accept <$> ResponseFormat.toMediaType req.responseFormat) req.headers

  timeoutErrorMessageIdent :: String
  timeoutErrorMessageIdent = "AffjaxTimeoutErrorMessageIdent"

  requestFailedMessageIdent :: String
  requestFailedMessageIdent = "AffjaxRequestFailedMessageIdent"

  addHeader :: Maybe RequestHeader -> Array RequestHeader -> Array RequestHeader
  addHeader mh hs = case mh of
    Just h | not $ any (on eq RequestHeader.name h) hs -> hs `Arr.snoc` h
    _ -> hs

  parseJSON :: String -> Except (NEL.NonEmptyList ForeignError) Json
  parseJSON = case _ of
    "" -> pure J.jsonEmptyObject
    str -> either (fail <<< ForeignError) pure (jsonParser str)

  fromResponse :: Foreign -> Except (NEL.NonEmptyList ForeignError) a
  fromResponse = case req.responseFormat of
    ResponseFormat.ArrayBuffer _ -> unsafeReadTagged "ArrayBuffer"
    ResponseFormat.Blob _ -> unsafeReadTagged "Blob"
    ResponseFormat.Document _ -> \x ->
      unsafeReadTagged "Document" x
        <|> unsafeReadTagged "XMLDocument" x
        <|> unsafeReadTagged "HTMLDocument" x
    ResponseFormat.Json coe -> coe <<< parseJSON <=< unsafeReadTagged "String"
    ResponseFormat.String _ -> unsafeReadTagged "String"
    ResponseFormat.Ignore coe -> const $ coe (pure unit)

type AjaxRequest :: Type -> Type
type AjaxRequest a =
  { method :: String
  , url :: URL
  , headers :: Array { field :: String, value :: String }
  , content :: Nullable Foreign
  , responseType :: String
  , username :: Nullable String
  , password :: Nullable String
  , withCredentials :: Boolean
  , timeout :: Number
  }

-- Drivers should have the following 'shape':
-- ```
-- { newXHR :: Effect Xhr
-- , fixupUrl :: Fn2 Xhr String String
-- }
-- ```
-- If you're adding a new environment (e.g. Electron),
-- be sure to re-export all the types and functions
-- in the `Affjax` module, so people can easily
-- update their code by changing the imported module
foreign import data AffjaxDriver :: Type

foreign import _ajax
  :: forall a
   . Fn5
       AffjaxDriver
       String
       String
       (String -> String -> ResponseHeader)
       (AjaxRequest a)
       (AC.EffectFnAff (Response Foreign))
