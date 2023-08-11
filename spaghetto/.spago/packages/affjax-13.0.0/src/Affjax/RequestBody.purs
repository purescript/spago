module Affjax.RequestBody where

import Data.Argonaut.Core (Json)
import Data.ArrayBuffer.Types as A
import Data.FormURLEncoded (FormURLEncoded)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.MediaType.Common (applicationJSON, applicationFormURLEncoded)
import Web.DOM.Document (Document)
import Web.File.Blob (Blob)
import Web.XHR.FormData (FormData)

-- | Represents data for an HTTP request that will be included in the request
-- | body.
data RequestBody
  = ArrayView (forall r. (forall a. A.ArrayView a -> r) -> r)
  | Blob Blob
  | Document Document
  | String String
  | FormData FormData
  | FormURLEncoded FormURLEncoded
  | Json Json

arrayView :: forall a. A.ArrayView a -> RequestBody
arrayView av = ArrayView \f -> f av

blob :: Blob -> RequestBody
blob = Blob

document :: Document -> RequestBody
document = Document

string :: String -> RequestBody
string = String

formData :: FormData -> RequestBody
formData = FormData

formURLEncoded :: FormURLEncoded -> RequestBody
formURLEncoded = FormURLEncoded

json :: Json -> RequestBody
json = Json

toMediaType :: RequestBody -> Maybe MediaType
toMediaType = case _ of
  FormURLEncoded _ -> Just applicationFormURLEncoded
  Json _ -> Just applicationJSON
  _ -> Nothing
