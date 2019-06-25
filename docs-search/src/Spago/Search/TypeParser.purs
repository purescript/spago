module Spago.Search.TypeParser where

import Prelude
import Data.Argonaut.Core -- (Json, fromString, stringify, toString)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array as Array
import Data.Either
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow, genericShow')
import Data.Maybe
import Data.Newtype
import Control.Alt
import Data.Traversable

newtype QualifiedName = QualifiedName { moduleName :: Array String
                                      , name :: String
                                      }

derive instance eqQualifiedName :: Eq QualifiedName
derive instance genericQualifiedName :: Generic QualifiedName _
derive instance newtypeQualifiedName :: Newtype QualifiedName _

instance showQualifiedName :: Show QualifiedName where
  show = genericShow

instance decodeJsonQualifiedName :: DecodeJson QualifiedName where
  decodeJson json = do
    case toArray json of
      Nothing -> mkJsonError "QualifiedName" json
      Just arrOfJsons -> do
        let arrOfVariants =
              arrOfJsons <#> \variantJson ->
                               Left  <$> decodeJson variantJson <|>
                               Right <$> decodeJson variantJson
        case arrOfVariants of
          [ Right (Left moduleName), Right (Right name)] ->
            pure $ QualifiedName { moduleName, name }
          _ -> mkJsonError "QualifiedName" json

mkJsonError :: forall a. String -> Json -> Either String a
mkJsonError name json =
  Left $ "Couldn't parse " <> name <> " from " <> stringify json

-- | The data type of kinds
data Kind
  -- | Kinds for labelled, unordered rows without duplicates
  = Row Kind
  -- | Function kinds
  | FunKind Kind Kind
  -- | A named kind
  | NamedKind QualifiedName

derive instance eqKind :: Eq Kind
derive instance genericKind :: Generic Kind _

instance showKind :: Show Kind where
  show = case _ of
    Row k -> "(Row " <> show k <> ")"
    FunKind k1 k2 -> "(FunKind " <> show k1 <> " " <> show k2 <> ")"
    NamedKind name -> "(NamedKind " <> show name <> ")"

newtype MaybeSingle a = MaybeSingle (Either a (Array a))

derive instance eqMaybeSingle :: Eq a => Eq (MaybeSingle a)
derive instance genericMaybeSingle :: Generic (MaybeSingle a) _
derive instance newtypeMaybeSingle :: Newtype (MaybeSingle a) _

instance showMaybeSingle :: Show a => Show (MaybeSingle a) where
  show = genericShow

instance decodeJsonMaybeSingle :: DecodeJson a => DecodeJson (MaybeSingle a) where
  decodeJson json =
    MaybeSingle <$> (Left <$> decodeJson json <|> Right <$> decodeJson json)

fromMaybeSingle :: forall a. MaybeSingle a -> Array a
fromMaybeSingle (MaybeSingle (Left a)) = Array.singleton a
fromMaybeSingle (MaybeSingle (Right a)) = a

instance decodeJsonKind :: DecodeJson Kind where
  decodeJson json = do
    handle <- decodeJson json
    tag <- handle .: "tag"
    case tag of
      "NamedKind" -> do
        -- annotation <- handle .: "annotation"
        contents <- handle .: "contents"
        pure $ NamedKind contents
      "Row" -> do
        contents <- handle .: "contents"
        pure $ Row contents
      "FunKind" -> do
        contents <- handle .: "contents"
        case contents of
          [k1, k2] ->
            pure $ FunKind k1 k2
          _ -> mkJsonError "FunKind" json
      _ -> mkJsonError "Kind" json
