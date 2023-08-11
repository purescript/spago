module Data.MediaType.Common where

import Data.MediaType (MediaType(..))

-- | The `application/x-www-form-urlencoded` media type.
applicationFormURLEncoded :: MediaType
applicationFormURLEncoded = MediaType "application/x-www-form-urlencoded"

-- | The `application/json` media type.
applicationJSON :: MediaType
applicationJSON = MediaType "application/json"

-- | The `application/javascript` media type.
applicationJavascript :: MediaType
applicationJavascript = MediaType "application/javascript"

-- | The `application/octet-stream` media type.
applicationOctetStream :: MediaType
applicationOctetStream = MediaType "application/octet-stream"

-- | The `application/xml` media type.
applicationXML :: MediaType
applicationXML = MediaType "application/xml"

-- | The `image/gif` media type.
imageGIF :: MediaType
imageGIF = MediaType "image/gif"

-- | The `image/jpeg` media type.
imageJPEG :: MediaType
imageJPEG = MediaType "image/jpeg"

-- | The `image/png` media type.
imagePNG :: MediaType
imagePNG = MediaType "image/png"

-- | The `multipart/form-data` media type.
multipartFormData :: MediaType
multipartFormData = MediaType "multipart/form-data"

-- | The `text/csv` media type.
textCSV :: MediaType
textCSV = MediaType "text/csv"

-- | The `text/html` media type.
textHTML :: MediaType
textHTML = MediaType "text/html"

-- | The `text/plain` media type.
textPlain :: MediaType
textPlain = MediaType "text/plain"

-- | The `text/xml` media type.
textXML :: MediaType
textXML = MediaType "text/xml"

-- | The `text/css` media type.
textCSS :: MediaType
textCSS = MediaType "text/css"
