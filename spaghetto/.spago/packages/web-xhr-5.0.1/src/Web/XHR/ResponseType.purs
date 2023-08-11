module Web.XHR.ResponseType
  ( ResponseType
  , arrayBuffer
  , blob
  , document
  , string
  ) where

import Web.File.Blob (Blob)
import Web.DOM.Document (Document)
import Data.ArrayBuffer.Types (ArrayBuffer)

newtype ResponseType :: Type -> Type
newtype ResponseType res = ResponseType String

type role ResponseType nominal

arrayBuffer :: ResponseType ArrayBuffer
arrayBuffer = ResponseType "arraybuffer"

blob :: ResponseType Blob
blob = ResponseType "blob"

document :: ResponseType Document
document = ResponseType "document"

string :: ResponseType String
string = ResponseType ""
