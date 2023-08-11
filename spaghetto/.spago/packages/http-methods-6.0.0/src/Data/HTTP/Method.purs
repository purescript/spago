module Data.HTTP.Method
  ( Method(..)
  , CustomMethod
  , unCustomMethod
  , fromString
  , print
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.String as Str

-- | The definition of the type is based on HTTP/1.1 with
-- | [RFC 2518](https://tools.ietf.org/html/rfc2518) and
-- | [RFC 5789](https://tools.ietf.org/html/rfc5789).
data Method
  -- HTTP/1.1
  = OPTIONS
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | TRACE
  | CONNECT

  -- RFC 2518
  | PROPFIND
  | PROPPATCH
  | MKCOL
  | COPY
  | MOVE
  | LOCK
  | UNLOCK

  -- RFC5789
  | PATCH

derive instance eqMethod :: Eq Method
derive instance ordMethod :: Ord Method

instance showMethod :: Show Method where
  show OPTIONS = "OPTIONS"
  show GET = "GET"
  show HEAD = "HEAD"
  show POST = "POST"
  show PUT = "PUT"
  show DELETE = "DELETE"
  show TRACE = "TRACE"
  show CONNECT = "CONNECT"
  show PROPFIND = "PROPFIND"
  show PROPPATCH = "PROPPATCH"
  show MKCOL = "MKCOL"
  show COPY = "COPY"
  show MOVE = "MOVE"
  show LOCK = "LOCK"
  show UNLOCK = "UNLOCK"
  show PATCH = "PATCH"

newtype CustomMethod = CustomMethod String

unCustomMethod :: CustomMethod -> String
unCustomMethod (CustomMethod m) = m

derive instance eqCustomMethod :: Eq CustomMethod
derive instance ordCustomMethod :: Ord CustomMethod

instance showCustomMethod :: Show CustomMethod where
  show (CustomMethod m) = "(CustomMethod " <> show m <> ")"

-- | Parses a `String` into a `Method` and pass the result into the first
-- | handler. If the string does not match a known method,
-- | passes itself into the second handler.
parse :: forall c. (Method -> c) -> (String -> c) -> String -> c
parse handleMethod handleUnknown s =
  case Str.toUpper s of
    "OPTIONS" -> handleMethod OPTIONS
    "GET" -> handleMethod GET
    "HEAD" -> handleMethod HEAD
    "POST" -> handleMethod POST
    "PUT" -> handleMethod PUT
    "DELETE" -> handleMethod DELETE
    "TRACE" -> handleMethod TRACE
    "CONNECT" -> handleMethod CONNECT
    "PROPFIND" -> handleMethod PROPFIND
    "PROPPATCH" -> handleMethod PROPPATCH
    "MKCOL" -> handleMethod MKCOL
    "COPY" -> handleMethod COPY
    "MOVE" -> handleMethod MOVE
    "LOCK" -> handleMethod LOCK
    "UNLOCK" -> handleMethod UNLOCK
    "PATCH" -> handleMethod PATCH
    m -> handleUnknown m

fromString :: String -> Either Method CustomMethod
fromString = parse Left (Right <<< CustomMethod)

print :: Either Method CustomMethod -> String
print = either show unCustomMethod
