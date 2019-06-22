module Spago.Search.Declarations where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, unwrap)

newtype IndexEntry
  = IndexEntry { title :: String
               , info :: { declType :: String
                         }
               , sourceSpan :: { start :: Array Int
                               , end :: Array Int
                               , name :: String
                               }
               }

derive instance eqIndexEntry :: Eq IndexEntry
derive instance genericIndexEntry :: Generic IndexEntry _
derive instance newtypeIndexEntry :: Newtype IndexEntry _

instance showIndexEntry :: Show IndexEntry where
  show = genericShow

instance decodeJsonIndexEntry :: DecodeJson IndexEntry where
  decodeJson json = IndexEntry <$> decodeJson json

instance encodeJsonIndexEntry :: EncodeJson IndexEntry where
  encodeJson = encodeJson <<< unwrap

newtype Declarations
  = Declarations { name :: String
                 , declarations :: Array IndexEntry
                 }

derive instance eqDeclarations :: Eq Declarations
derive instance genericDeclarations :: Generic Declarations _
derive instance newtypeDeclarations :: Newtype Declarations _

instance showDeclarations :: Show Declarations where
  show = genericShow

instance decodeJsonDeclarations :: DecodeJson Declarations where
  decodeJson json = Declarations <$> decodeJson json

instance encodeJsonDeclarations :: EncodeJson Declarations where
  encodeJson = encodeJson <<< unwrap
