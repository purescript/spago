module Affjax.RequestHeader where

import Prelude

import Data.MediaType (MediaType)
import Data.Newtype (unwrap)

data RequestHeader
  = Accept MediaType
  | ContentType MediaType
  | RequestHeader String String

derive instance eqRequestHeader :: Eq RequestHeader
derive instance ordRequestHeader :: Ord RequestHeader

instance showRequestHeader :: Show RequestHeader where
  show (Accept m) = "(Accept " <> show m <> ")"
  show (ContentType m) = "(ContentType " <> show m <> ")"
  show (RequestHeader h v) = "(RequestHeader " <> show h <> " " <> show v <> ")"

name :: RequestHeader -> String
name (Accept _) = "Accept"
name (ContentType _) = "Content-Type"
name (RequestHeader h _) = h

value :: RequestHeader -> String
value (Accept m) = unwrap m
value (ContentType m) = unwrap m
value (RequestHeader _ v) = v
