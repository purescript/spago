module Data.Codec.Argonaut.Generic where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core as J
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), from, to)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

-- | Encodes nullary sums with a Generic instance as strings that match the constructor names.
-- |
-- | ```purescript
-- | import Data.Argonaut as J
-- |
-- | data MySum = Ctor1 | Ctor2 | MoarCtors
-- | derive instance genericMySum ∷ Generic MySum _
-- |
-- | encode (nullarySum "MySum") Ctor1 == J.fromString "Ctor1"
-- | decode (nullarySum "MySum") (J.fromString "MoarCtors") == Right MoarCtors
-- |```
nullarySum ∷ ∀ a r. Generic a r ⇒ NullarySumCodec r ⇒ String → CA.JsonCodec a
nullarySum name =
  C.codec'
    (map to <<< nullarySumDecode name)
    (nullarySumEncode <<< from)

class NullarySumCodec r where
  nullarySumEncode ∷ r → J.Json
  nullarySumDecode ∷ String → J.Json → Either CA.JsonDecodeError r

instance nullarySumCodecSum ∷ (NullarySumCodec a, NullarySumCodec b) ⇒ NullarySumCodec (Sum a b) where
  nullarySumEncode = case _ of
    Inl a → nullarySumEncode a
    Inr b → nullarySumEncode b
  nullarySumDecode name j = Inl <$> nullarySumDecode name j
    <|> Inr <$> nullarySumDecode name j

instance nullarySumCodecCtor ∷ IsSymbol name ⇒ NullarySumCodec (Constructor name NoArguments) where
  nullarySumEncode _ =
    J.fromString $ reflectSymbol (Proxy ∷ Proxy name)
  nullarySumDecode name j = do
    tag ← note (CA.Named name (CA.TypeMismatch "String")) (J.toString j)
    if tag /= reflectSymbol (Proxy ∷ Proxy name) then
      Left (CA.Named name (CA.UnexpectedValue j))
    else
      Right (Constructor NoArguments)
