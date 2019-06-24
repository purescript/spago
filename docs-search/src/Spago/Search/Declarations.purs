module Spago.Search.Declarations where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Aff (Aff)

newtype IndexEntry
  = IndexEntry { title :: String
               , comments :: Maybe String
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
  decodeJson json = IndexEntry <$> do
    handle     <- decodeJson json
    title      <- handle .:  "title"
    comments   <- handle .:? "comments"
    info       <- handle .:  "info"
    sourceSpan <- handle .:  "sourceSpan"
    pure { title, comments, info, sourceSpan }

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

loadDeclarations :: String -> Aff (Either String (Array Declarations))
loadDeclarations string = decodeJson <$> toAffE (loadDeclarations_ string)

foreign import loadDeclarations_ :: String -> Effect (Promise Json)
