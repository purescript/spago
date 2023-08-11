module Affjax.ResponseFormat where

import Prelude

import Data.Argonaut.Core (Json)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON)
import Web.DOM.Document (Document)
import Web.File.Blob (Blob)

-- | Used to represent how a HTTP response body should be interpreted.
data ResponseFormat a
  = ArrayBuffer (forall f. f ArrayBuffer -> f a)
  | Blob (forall f. f Blob -> f a)
  | Document (forall f. f Document -> f a)
  | Json (forall f. f Json -> f a)
  | String (forall f. f String -> f a)
  | Ignore (forall f. f Unit -> f a)

arrayBuffer :: ResponseFormat ArrayBuffer
arrayBuffer = ArrayBuffer identity

blob :: ResponseFormat Blob
blob = Blob identity

document :: ResponseFormat Document
document = Document identity

json :: ResponseFormat Json
json = Json identity

string :: ResponseFormat String
string = String identity

ignore :: ResponseFormat Unit
ignore = Ignore identity

-- | Converts a `Response a` into a string representation of the response type
-- | that it represents.
toResponseType :: forall a. ResponseFormat a -> String
toResponseType =
  case _ of
    ArrayBuffer _ -> "arraybuffer"
    Blob _ -> "blob"
    Document _ -> "document"
    Json _ -> "text" -- IE doesn't support "json" ResponseFormat
    String _ -> "text"
    Ignore _ -> ""

toMediaType :: forall a. ResponseFormat a -> Maybe MediaType
toMediaType =
  case _ of
    Json _ -> Just applicationJSON
    _ -> Nothing
