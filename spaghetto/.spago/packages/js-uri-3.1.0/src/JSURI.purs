module JSURI
  ( encodeURIComponent
  , encodeFormURLComponent
  , encodeURI
  , decodeURIComponent
  , decodeFormURLComponent
  , decodeURI
  ) where

import Prelude

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))

foreign import _encodeURIComponent :: Fn3 (String -> Maybe String) (String -> Maybe String) String (Maybe String)

-- | URI-encode a string according to RFC3896. Implemented using JavaScript's
-- | `encodeURIComponent`.
-- |
-- | ```purs
-- | > encodeURIComponent "https://purescript.org"
-- | Just "https%3A%2F%2Fpurescript.org"
-- | ```
-- |
-- | Encoding a URI can fail with a `URIError` if the string contains malformed
-- | characters. If you are confident you are encoding a well-formed string then
-- | you can run this function unsafely:
-- |
-- | ```purs
-- | import Partial.Unsafe (unsafePartial)
-- | import Data.Maybe (fromJust)
-- |
-- | unsafeEncode :: String -> String
-- | unsafeEncode str = unsafePartial $ fromJust $ encodeURIComponent str
-- | ```
encodeURIComponent :: String -> Maybe String
encodeURIComponent = runFn3 _encodeURIComponent (const Nothing) Just

foreign import _encodeFormURLComponent :: Fn3 (String -> Maybe String) (String -> Maybe String) String (Maybe String)

-- | URI-encode a string according to RFC3896, except with spaces encoded using
-- | '+' instead of '%20' to comply with application/x-www-form-urlencoded.
-- |
-- | ```purs
-- | > encodeURIComponent "abc ABC"
-- | Just "abc%20ABC"
-- |
-- | > encodeFormURLComponent "abc ABC"
-- | Just "abc+ABC"
-- | ```
encodeFormURLComponent :: String -> Maybe String
encodeFormURLComponent = runFn3 _encodeFormURLComponent (const Nothing) Just

foreign import _decodeURIComponent :: Fn3 (String -> Maybe String) (String -> Maybe String) String (Maybe String)

-- | Decode a URI string according to RFC3896. Implemented using JavaScript's
-- | `decodeURIComponent`.
-- |
-- | ```purs
-- | > decodeURIComponent "https%3A%2F%2Fpurescript.org"
-- | Just "https://purescript.org"
-- | ```
-- |
-- | Decoding a URI can fail with a `URIError` if the string contains malformed
-- | characters. If you are confident you are encoding a well-formed string then
-- | you can run this function unsafely:
-- |
-- | ```purs
-- | import Partial.Unsafe (unsafePartial)
-- | import Data.Maybe (fromJust)
-- |
-- | unsafeDecode :: String -> String
-- | unsafeDecode str = unsafePartial $ fromJust $ decodeURIComponent str
-- | ```
decodeURIComponent :: String -> Maybe String
decodeURIComponent = runFn3 _decodeURIComponent (const Nothing) Just

foreign import _decodeFormURLComponent :: Fn3 (String -> Maybe String) (String -> Maybe String) String (Maybe String)

-- | Decode a URI according to application/x-www-form-urlencoded (for example,
-- | a string containing '+' for spaces or query parameters).
-- |
-- | ```purs
-- | > decodeURIComponent "https%3A%2F%2Fpurescript.org?search+query"
-- | Just "https://purescript.org?search+query"
-- |
-- | > decodeFormURLComponent "https%3A%2F%2Fpurescript.org?search+query"
-- | Just "https://purescript.org?search query"
-- | ```
decodeFormURLComponent :: String -> Maybe String
decodeFormURLComponent = runFn3 _decodeFormURLComponent (const Nothing) Just

foreign import _encodeURI :: Fn3 (String -> Maybe String) (String -> Maybe String) String (Maybe String)

encodeURI :: String -> Maybe String
encodeURI = runFn3 _encodeURI (const Nothing) Just

foreign import _decodeURI :: Fn3 (String -> Maybe String) (String -> Maybe String) String (Maybe String)

decodeURI :: String -> Maybe String
decodeURI = runFn3 _decodeURI (const Nothing) Just
