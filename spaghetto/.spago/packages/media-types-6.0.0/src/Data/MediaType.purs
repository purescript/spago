module Data.MediaType where

import Prelude

import Data.Newtype (class Newtype)

-- | A media type (also known as a **Multipurpose Internet Mail Extensions or
-- | MIME type**) is a standard that indicates the nature and format of a
-- | document, file, or assortment of bytes. It is defined and standardized in
-- | IETF's [RFC 6838](https://tools.ietf.org/html/rfc6838).
newtype MediaType = MediaType String

derive instance newtypeMediaType :: Newtype MediaType _
derive instance eqMediaType :: Eq MediaType
derive instance ordMediaType :: Ord MediaType

instance showMediaType :: Show MediaType where
  show (MediaType h) = "(MediaType " <> show h <> ")"
