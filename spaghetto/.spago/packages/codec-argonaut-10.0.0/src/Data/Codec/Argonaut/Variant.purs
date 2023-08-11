module Data.Codec.Argonaut.Variant where

import Prelude

import Data.Argonaut.Core as J
import Data.Codec (Codec(..))
import Data.Codec as Codec
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..), decode, encode, jobject, json, prop, string)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import Foreign.Object as FO
import Foreign.Object.ST as FOST
import Prim.Row as R
import Prim.RowList as RL
import Record as Rec
import Type.Equality as TE
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Builds a codec for a variant from a record, similar to the way
-- | `Variant.match` works to pattern match on a variant.
-- |
-- | Commonly used to write decoders for sum-types, by providing a mapping from
-- | and to a Variant from that type and then using `dimap`.
-- |
-- | Each field in the record accepts an `Either`, where `Right` is used to
-- | specify a codec used for the constructor, and `Left` is used to specify a
-- | static value (generally as `Left unit` for nullary constructors).
-- |
-- | The variant will be encoded as a JSON object of the form
-- | `{ "tag": <name>, "value": <value> }`, where `<name>` is the name of the
-- | variant case, and `<value>` is the associated value (omitted in the case
-- | of static `Left`-defined values).
-- |
-- |```purescript
-- | codecMaybeMatch ∷ ∀ a. JA.JsonCodec a → JA.JsonCodec (Maybe a)
-- | codecMaybeMatch codecA =
-- |   dimap toVariant fromVariant
-- |     (JAV.variantMatch
-- |       { just: Right codecA
-- |       , nothing: Left unit
-- |       })
-- |   where
-- |   toVariant = case _ of
-- |     Just a → V.inj (Proxy ∷ _ "just") a
-- |     Nothing → V.inj (Proxy ∷ _ "nothing") unit
-- |   fromVariant = V.match
-- |     { just: Just
-- |     , nothing: \_ → Nothing
-- |     }
-- |```
variantMatch
  ∷ ∀ rl ri ro
  . RL.RowToList ri rl
  ⇒ VariantCodec rl ri ro
  ⇒ Record ri
  → JsonCodec (Variant ro)
variantMatch = variantCodec (Proxy ∷ Proxy rl)

-- | Builds codecs for variants in combination with `variantCase`.
-- |
-- | Provides an alternative means of building variant codecs to that of
-- | `variantMatch`, often for cases where the codec is being constructed
-- | with a fold or some other similar technique.
-- |
-- |```purescript
-- | codecMaybe ∷ ∀ a. JA.JsonCodec a → JA.JsonCodec (Maybe a)
-- | codecMaybe codecA =
-- |   dimap toVariant fromVariant
-- |     (JAV.variant
-- |       # JAV.variantCase _Just (Right codecA)
-- |       # JAV.variantCase _Nothing (Left unit))
-- |   where
-- |   toVariant = case _ of
-- |     Just a → V.inj _Just a
-- |     Nothing → V.inj _Nothing unit
-- |   fromVariant = V.case_
-- |     # V.on _Just Just
-- |     # V.on _Nothing (const Nothing)
-- |   _Just = Proxy ∷ Proxy "just"
-- |   _Nothing = Proxy ∷ Proxy "nothing"
-- |```
variant ∷ JsonCodec (Variant ())
variant = Codec (Left <<< UnexpectedValue) case_

variantCase
  ∷ ∀ l a r r'
  . IsSymbol l
  ⇒ R.Cons l a r r'
  ⇒ Proxy l
  → Either a (JsonCodec a)
  → JsonCodec (Variant r)
  → JsonCodec (Variant r')
variantCase proxy eacodec (Codec dec enc) = Codec.Codec dec' enc'
  where

  dec' ∷ J.Json → Either JsonDecodeError (Variant r')
  dec' j = do
    obj ← decode jobject j
    tag ← decode (prop "tag" string) obj
    if tag == reflectSymbol proxy then
      case eacodec of
        Left a → pure (inj proxy a)
        Right codec → do
          value ← decode (prop "value" json) obj
          inj proxy <$> decode codec value
    else
      coerceR <$> dec j

  enc' ∷ Variant r' → Tuple J.Json (Variant r')
  enc' v =
    on proxy
      ( \v' → flip Tuple v $ encode jobject $
          FO.runST do
            obj ← FOST.new
            _ ← FOST.poke "tag" (encode string (reflectSymbol proxy)) obj
            case eacodec of
              Left _ → pure obj
              Right codec → FOST.poke "value" (encode codec v') obj
      )
      (\v' → enc v' $> v)
      v

  coerceR ∷ Variant r → Variant r'
  coerceR = unsafeCoerce

-- | The class used to enable the building of `Variant` codecs from a record of
-- | codecs.
class VariantCodec (rl ∷ RL.RowList Type) (ri ∷ Row Type) (ro ∷ Row Type) | rl → ri ro where
  variantCodec ∷ ∀ proxy. proxy rl → Record ri → JsonCodec (Variant ro)

instance variantCodecNil ∷ VariantCodec RL.Nil () () where
  variantCodec _ _ = variant

instance variantCodecCons ∷
  ( VariantCodec rs ri' ro'
  , R.Cons sym (Either a (JsonCodec a)) ri' ri
  , R.Cons sym a ro' ro
  , IsSymbol sym
  , TE.TypeEquals co (Either a (JsonCodec a))
  ) ⇒
  VariantCodec (RL.Cons sym co rs) ri ro where
  variantCodec _ codecs =
    variantCase (Proxy ∷ Proxy sym) codec tail
    where
    codec ∷ Either a (JsonCodec a)
    codec = TE.from (Rec.get (Proxy ∷ Proxy sym) codecs)

    tail ∷ JsonCodec (Variant ro')
    tail = variantCodec (Proxy ∷ Proxy rs) ((unsafeCoerce ∷ Record ri → Record ri') codecs)
