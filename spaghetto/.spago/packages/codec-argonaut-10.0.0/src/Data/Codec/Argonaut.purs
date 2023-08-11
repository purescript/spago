module Data.Codec.Argonaut
  ( JsonCodec
  , JsonDecodeError(..)
  , printJsonDecodeError
  , json
  , null
  , boolean
  , number
  , int
  , string
  , codePoint
  , char
  , jarray
  , jobject
  , void
  , array
  , JIndexedCodec
  , indexedArray
  , index
  , JPropCodec
  , object
  , prop
  , record
  , recordProp
  , recordPropOptional
  , fix
  , named
  , coercible
  , prismaticCodec
  , module Codec
  ) where

import Prelude

import Data.Argonaut.Core as J
import Data.Array as A
import Data.Bifunctor (bimap, lmap)
import Data.Bifunctor as BF
import Data.Codec (Codec(..), Codec')
import Data.Codec (Codec(..), Codec', codec, codec', decode, encode, hoist, identity, (<~<), (>~>), (~)) as Codec
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Int as I
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.String as S
import Data.String.CodeUnits as SCU
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Record.Unsafe as Record
import Safe.Coerce (coerce)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

-- | Codec type for `Json` values.
type JsonCodec a = Codec' (Either JsonDecodeError) J.Json a

-- | Error type for failures while decoding.
data JsonDecodeError
  = TypeMismatch String
  | UnexpectedValue J.Json
  | AtIndex Int JsonDecodeError
  | AtKey String JsonDecodeError
  | Named String JsonDecodeError
  | MissingValue

derive instance eqJsonDecodeError ∷ Eq JsonDecodeError
derive instance ordJsonDecodeError ∷ Ord JsonDecodeError
derive instance genericJsonDecodeError ∷ Generic JsonDecodeError _

instance showJsonDecodeError ∷ Show JsonDecodeError where
  show = case _ of
    TypeMismatch s → "(TypeMismatch " <> show s <> ")"
    UnexpectedValue j → "(UnexpectedValue " <> J.stringify j <> ")"
    AtIndex i e → "(AtIndex " <> show i <> " " <> show e <> ")"
    AtKey k e → "(AtKey " <> show k <> " " <> show e <> ")"
    Named s e → "(Named " <> show s <> " " <> show e <> ")"
    MissingValue → "MissingValue"

-- | Prints a `JsonDecodeError` as a somewhat readable error message.
printJsonDecodeError ∷ JsonDecodeError → String
printJsonDecodeError err =
  "An error occurred while decoding a JSON value:\n" <> go err
  where
  go = case _ of
    TypeMismatch ty → "  Expected value of type '" <> ty <> "'."
    UnexpectedValue val → "  Unexpected value " <> J.stringify val <> "."
    AtIndex ix inner → "  At array index " <> show ix <> ":\n" <> go inner
    AtKey key inner → "  At object key " <> key <> ":\n" <> go inner
    Named name inner → "  Under '" <> name <> "':\n" <> go inner
    MissingValue → "  No value was found."

-- | The "identity codec" for `Json` values.
json ∷ JsonCodec J.Json
json = Codec.codec' pure identity

-- | A codec for `null` values in `Json`.
null ∷ JsonCodec Unit
null = jsonPrimCodec "Null" J.toNull (const J.jsonNull)

-- | A codec for `Boolean` values in `Json`.
boolean ∷ JsonCodec Boolean
boolean = jsonPrimCodec "Boolean" J.toBoolean J.fromBoolean

-- | A codec for `Number` values in `Json`.
number ∷ JsonCodec Number
number = jsonPrimCodec "Number" J.toNumber J.fromNumber

-- | A codec for `Int` values in `Json`.
int ∷ JsonCodec Int
int = jsonPrimCodec "Int" (I.fromNumber <=< J.toNumber) (J.fromNumber <<< I.toNumber)

-- | A codec for `String` values in `Json`.
string ∷ JsonCodec String
string = jsonPrimCodec "String" J.toString J.fromString

-- | A codec for `Codepoint` values in `Json`.
codePoint ∷ JsonCodec S.CodePoint
codePoint = jsonPrimCodec "CodePoint" (S.codePointAt 0 <=< J.toString) (J.fromString <<< S.singleton)

-- | A codec for `Char` values in `Json`.
char ∷ JsonCodec Char
char = jsonPrimCodec "Char" (SCU.toChar <=< J.toString) (J.fromString <<< SCU.singleton)

-- | A codec for `Void` values.
void ∷ JsonCodec Void
void = jsonPrimCodec "Void" (const Nothing) absurd

-- | A codec for `Array Json` values in `Json`. This does not decode the values
-- | of the array, for that use `array` for a general array decoder, or
-- | `indexedArray` with `index` to decode fixed length array encodings.
jarray ∷ JsonCodec (Array J.Json)
jarray = jsonPrimCodec "Array" J.toArray J.fromArray

-- | A codec for `JObject` values in `Json`.
jobject ∷ JsonCodec (FO.Object J.Json)
jobject = jsonPrimCodec "Object" J.toObject J.fromObject

-- | A codec for arbitrary length `Array`s where every item in the array
-- | shares the same type.
-- |
-- | ``` purescript
-- | import Data.Codec.Argonaut as CA
-- |
-- | codecIntArray ∷ CA.JsonCodec (Array Int)
-- | codecIntArray = CA.array CA.int
-- | ```
array ∷ ∀ a. JsonCodec a → JsonCodec (Array a)
array codec =
  Codec.codec'
    (\j → traverseWithIndex (\ix j' → BF.lmap (AtIndex ix) (Codec.decode codec j')) =<< Codec.decode jarray j)
    (\a → J.fromArray (map (Codec.encode codec) a))

-- | Codec type for specifically indexed `JArray` elements.
type JIndexedCodec a =
  Codec
    (Either JsonDecodeError)
    (Array J.Json)
    (L.List J.Json)
    a
    a

-- | A codec for types that are encoded as an array with a specific layout.
-- |
-- | For example, if we'd like to encode a `Person` as a 2-element array, like
-- | `["Rashida", 37]`, we could write the following codec:
-- |
-- | ```purescript
-- | import Data.Codec ((~))
-- | import Data.Codec.Argonaut as CA
-- |
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | codecPerson ∷ CA.JsonCodec Person
-- | codecPerson = CA.indexedArray "Test Object" $
-- |   { name: _, age: _ }
-- |     <$> _.name ~ CA.index 0 CA.string
-- |     <*> _.age ~ CA.index 1 CA.int
-- | ```
indexedArray ∷ ∀ a. String → JIndexedCodec a → JsonCodec a
indexedArray name codec =
  Codec.codec'
    (\j → lmap (Named name) (Codec.decode codec =<< Codec.decode jarray j))
    (\a → Codec.encode jarray (A.fromFoldable (Codec.encode codec a)))

-- | A codec for an item in an `indexedArray`.
index ∷ ∀ a. Int → JsonCodec a → JIndexedCodec a
index ix codec =
  Codec.codec
    (\xs → BF.lmap (AtIndex ix) (maybe (Left MissingValue) (Codec.decode codec) (A.index xs ix)))
    (pure <<< Codec.encode codec)

-- | Codec type for `JObject` prop/value pairs.
type JPropCodec a =
  Codec
    (Either JsonDecodeError)
    (FO.Object J.Json)
    (L.List (Tuple String J.Json))
    a
    a

-- | A codec for objects that are encoded with specific properties.
-- |
-- | See also `Data.Codec.Argonaut.Record.object` for a more commonly useful
-- | version of this function.
object ∷ ∀ a. String → JPropCodec a → JsonCodec a
object name codec =
  Codec.codec'
    (\j → lmap (Named name) (Codec.decode codec =<< Codec.decode jobject j))
    (\a → Codec.encode jobject (FO.fromFoldable (Codec.encode codec a)))

-- | A codec for a property of an object.
prop ∷ ∀ a. String → JsonCodec a → JPropCodec a
prop key codec =
  Codec.codec
    (\obj → BF.lmap (AtKey key) (maybe (Left MissingValue) (Codec.decode codec) (FO.lookup key obj)))
    (pure <<< Tuple key <<< Codec.encode codec)

-- | The starting value for a object-record codec. Used with `recordProp` it
-- | provides a convenient method for defining codecs for record types that
-- | encode into JSON objects of the same shape.
-- |
-- | For example, to encode a record as the JSON object
-- | `{ "name": "Karl", "age": 25 }` we would define a codec like this:
-- | ```
-- | import Data.Codec.Argonaut as CA
-- | import Type.Proxy (Proxy(..))
-- |
-- | type Person = { name ∷ String, age ∷ Int }
-- |
-- | codecPerson ∷ CA.JsonCodec Person
-- | codecPerson =
-- |   CA.object "Person" $ CA.record
-- |     # CA.recordProp (Proxy :: _ "name") CA.string
-- |     # CA.recordProp (Proxy :: _ "age") CA.int
-- | ```
-- |
-- | See also `Data.Codec.Argonaut.Record.object` for a more commonly useful
-- | version of this function.
record ∷ JPropCodec {}
record = Codec (const (pure {})) pure

-- | Used with `record` to define codecs for record types that encode into JSON
-- | objects of the same shape. See the comment on `record` for an example.
recordProp
  ∷ ∀ p a r r'
  . IsSymbol p
  ⇒ Row.Cons p a r r'
  ⇒ Proxy p
  → JsonCodec a
  → JPropCodec (Record r)
  → JPropCodec (Record r')
recordProp p codecA codecR =
  let key = reflectSymbol p in Codec.codec (dec' key) (enc' key)
  where
  dec' ∷ String → FO.Object J.Json → Either JsonDecodeError (Record r')
  dec' key obj = do
    r ← Codec.decode codecR obj
    a ← BF.lmap (AtKey key) case FO.lookup key obj of
      Just val → Codec.decode codecA val
      Nothing → Left MissingValue
    pure $ Record.unsafeSet key a r

  enc' ∷ String → Record r' → L.List (Tuple String J.Json)
  enc' key val =
    Tuple key (Codec.encode codecA (Record.unsafeGet key val))
      : Codec.encode codecR (unsafeForget val)

  unsafeForget ∷ Record r' → Record r
  unsafeForget = unsafeCoerce

-- | Used with `record` to define an optional field.
-- |
-- | This will only decode the property as `Nothing` if the field does not exist
-- | in the object - having a values such as `null` assigned will need handling
-- | separately.
-- |
-- | The property will be omitted when encoding and the value is `Nothing`.
recordPropOptional
  ∷ ∀ p a r r'
  . IsSymbol p
  ⇒ Row.Cons p (Maybe a) r r'
  ⇒ Proxy p
  → JsonCodec a
  → JPropCodec (Record r)
  → JPropCodec (Record r')
recordPropOptional p codecA codecR = Codec.codec dec' enc'
  where
  key ∷ String
  key = reflectSymbol p

  dec' ∷ FO.Object J.Json → Either JsonDecodeError (Record r')
  dec' obj = do
    r ← Codec.decode codecR obj
    a ← BF.lmap (AtKey key) case FO.lookup key obj of
      Just val → Just <$> Codec.decode codecA val
      _ → Right Nothing
    pure $ Record.unsafeSet key a r

  enc' ∷ Record r' → L.List (Tuple String J.Json)
  enc' val = do
    let w = Codec.encode codecR (unsafeForget val)
    case Record.unsafeGet key val of
      Just a → Tuple key (Codec.encode codecA a) : w
      Nothing → w

  unsafeForget ∷ Record r' → Record r
  unsafeForget = unsafeCoerce

jsonPrimCodec ∷ ∀ a. String → (J.Json → Maybe a) → (a → J.Json) → JsonCodec a
jsonPrimCodec ty f = Codec.codec' (maybe (Left (TypeMismatch ty)) pure <<< f)

-- | Helper function for defining recursive codecs in situations where the codec
-- | definition causes a _"The value of <codec> is undefined here"_ error.
-- |
-- | ```purescript
-- | import Data.Codec.Argonaut as CA
-- | import Data.Codec.Argonaut.Common as CAC
-- | import Data.Codec.Argonaut.Record as CAR
-- | import Data.Maybe (Maybe)
-- | import Data.Newtype (class Newtype)
-- | import Data.Profunctor (wrapIso)
-- |
-- | newtype IntList = IntList { cell ∷ Int, rest ∷ Maybe IntList }
-- |
-- | derive instance newtypeLoopyList ∷ Newtype IntList _
-- |
-- | codecIntList ∷ CA.JsonCodec IntList
-- | codecIntList =
-- |   CA.fix \codec →
-- |     wrapIso IntList $
-- |       CAR.object "IntList" { cell: CA.int, rest: CAC.maybe codec }
-- | ```
fix ∷ ∀ a. (JsonCodec a → JsonCodec a) → JsonCodec a
fix f =
  Codec.codec'
    (\x → Codec.decode (f (fix f)) x)
    (\x → Codec.encode (f (fix f)) x)

-- | A codec for introducing names into error messages - useful when definiting a codec for a type
-- | synonym for a record, for instance.
named ∷ ∀ a. String → JsonCodec a → JsonCodec a
named name codec =
  Codec.codec'
    (lmap (Named name) <<< Codec.decode codec)
    (Codec.encode codec)

-- | A codec for types that can be safely coerced.
-- |
-- | Accepts the name of the target type as an argument to improve error messaging when the inner
-- | codec fails.
coercible ∷ ∀ a b. Coercible a b ⇒ String → JsonCodec a → JsonCodec b
coercible name codec =
  Codec.codec'
    (bimap (Named name) coerce <<< Codec.decode codec)
    (coerce (Codec.encode codec))

-- | Adapts an existing codec with a pair of functions to allow a value to be
-- | further refined. If the inner decoder fails an `UnexpectedValue` error will
-- | be raised for JSON input.
-- |
-- | This function is named as such as the pair of functions it accepts
-- | correspond with the `preview` and `review` functions of a `Prism`-style lens.
-- |
-- | An example of this would be a codec for `Data.String.NonEmpty.NonEmptyString`:
-- |
-- | ```purescript
-- | nonEmptyString ∷ CA.JsonCodec NES.NonEmptyString
-- | nonEmptyString = CA.prismaticCodec "NonEmptyString" NES.fromString NES.toString CA.string
-- | ```
-- |
-- | Another example might be to handle a mapping from a small sum type to
-- | strings:
-- |
-- | ```purescript
-- | data Direction = North | South | West | East
-- |
-- | directionCodec :: JsonCodec Direction
-- | directionCodec = CA.prismaticCodec "Direction" dec enc string
-- |   where
-- |     dec = case _ of
-- |       "N" -> Just North
-- |       "S" -> Just South
-- |       "W" -> Just West
-- |       "E" -> Just East
-- |       _ -> Nothing
-- |
-- |     enc = case _ of
-- |       North -> "N"
-- |       South -> "S"
-- |       West -> "W"
-- |       East -> "E"
-- | ```
-- |
-- | Although for this latter case there are some other options too, in the form
-- | of `Data.Codec.Argonaut.Generic.nullarySum` and `Data.Codec.Argonaut.Sum.enumSum`.
prismaticCodec ∷ ∀ a b. String → (a → Maybe b) → (b → a) → JsonCodec a → JsonCodec b
prismaticCodec name f g codec =
  Codec.codec'
    (\j → note (Named name (UnexpectedValue j)) <<< f =<< Codec.decode codec j)
    (Codec.encode codec <<< g)
