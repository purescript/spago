module Affjax.ResponseHeader where

import Prelude

data ResponseHeader = ResponseHeader String String

derive instance eqResponseHeader :: Eq ResponseHeader
derive instance ordResponseHeader :: Ord ResponseHeader

instance showResponseHeader :: Show ResponseHeader where
  show (ResponseHeader h v) = "(ResponseHeader " <> show h <> " " <> show v <> ")"

name :: ResponseHeader -> String
name (ResponseHeader h _) = h

value :: ResponseHeader -> String
value (ResponseHeader _ v) = v
