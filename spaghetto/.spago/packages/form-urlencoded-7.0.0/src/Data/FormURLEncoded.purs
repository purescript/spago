module Data.FormURLEncoded where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (joinWith, split) as String
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import JSURI (decodeFormURLComponent, encodeFormURLComponent)

-- | `FormURLEncoded` is an ordered list of key-value pairs with possible duplicates.
newtype FormURLEncoded = FormURLEncoded (Array (Tuple String (Maybe String)))

-- | Construct `FormURLEncoded` from an `Array` of key-value pairs.
fromArray :: Array (Tuple String (Maybe String)) -> FormURLEncoded
fromArray = FormURLEncoded

-- | View `FormURLEncoded` as an `Array` of key-value pairs.
toArray :: FormURLEncoded -> Array (Tuple String (Maybe String))
toArray (FormURLEncoded a) = a

derive instance newtypeFormUrlEncoded :: Newtype FormURLEncoded _
derive newtype instance eqFormUrlEncoded :: Eq FormURLEncoded
derive newtype instance ordFormUrlEncoded :: Ord FormURLEncoded
derive newtype instance semigroupFormUrlEncoded :: Semigroup FormURLEncoded
derive newtype instance monoidFormUrlEncoded :: Monoid FormURLEncoded

instance showFormUrlEncoded :: Show FormURLEncoded where
  show (FormURLEncoded kvs) = "(FormURLEncoded " <> show kvs <> ")"

-- | Encode `FormURLEncoded` as `application/x-www-form-urlencoded`.
encode :: FormURLEncoded -> Maybe String
encode = map (String.joinWith "&") <<< traverse encodePart <<< toArray
  where
  encodePart = case _ of
    Tuple k Nothing -> encodeFormURLComponent k
    Tuple k (Just v) -> (\key val -> key <> "=" <> val) <$> encodeFormURLComponent k <*> encodeFormURLComponent v

-- | Decode `FormURLEncoded` from `application/x-www-form-urlencoded`.
decode :: String -> Maybe FormURLEncoded
decode = map FormURLEncoded <<< traverse decodePart <<< String.split (Pattern "&")
  where
  decodePart = String.split (Pattern "=") >>> case _ of
    [ k, v ] -> (\key val -> Tuple key $ Just val) <$> decodeFormURLComponent k <*> decodeFormURLComponent v
    [ k ] -> Tuple <$> decodeFormURLComponent k <*> pure Nothing
    _ -> Nothing
