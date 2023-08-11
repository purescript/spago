# purescript-codec-argonaut

[![Latest release](http://img.shields.io/github/release/garyb/purescript-codec-argonaut.svg)](https://github.com/garyb/purescript-codec-argonaut/releases)
![Build Status](https://github.com/garyb/purescript-codec-argonaut/actions/workflows/ci.yml/badge.svg)

Bi-directional codecs for [argonaut](https://github.com/purescript-contrib/purescript-argonaut-core).

This library is built on [`purescript-codec`](https://github.com/garyb/purescript-codec) and offers a different approach to dealing with JSON encoding/decoding than [`purescript-argonaut-codecs`](https://github.com/purescript-contrib/purescript-argonaut-codecs). Instead of using type classes, codecs are constructed as values explicitly. As long as the basic codec values provided by this library are used, the codecs are guaranteed to roundtrip successfully.

The errors reported from this library are a little better than those provided by `purescript-argonaut-codecs` too - they contain the full JSON structure to the point of failure, and the error can be inspected as a value before being printed as a string.

For more information on the motivation behind this library, I [wrote a bit about my problems with typeclass codecs](http://code.slipthrough.net/2018/03/13/thoughts-on-typeclass-codecs/) previously.

## Installation

```
bower install purescript-codec-argonaut
```

## Usage

As [`JsonCodec`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#t:JsonCodec)s are values, they need to be fed into the [`encode`](https://pursuit.purescript.org/packages/purescript-codec/docs/Data.Codec/#v:encode) or [`decode`](https://pursuit.purescript.org/packages/purescript-codec/docs/Data.Codec/#v:decode) function provided by [`Data.Codec`](https://pursuit.purescript.org/packages/purescript-codec/docs/Data.Codec) (and re-exported by [`Data.Codec.Argonaut`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut)):

```purescript
import Data.Argonaut.Core as J
import Data.Codec.Argonaut as CA
import Data.Either (Either)

codec = CA.array CA.string

encodeStringArray ∷ Array String → J.Json
encodeStringArray = CA.encode codec

decodeStringArray ∷ J.Json → Either CA.JsonDecodeError (Array String)
decodeStringArray = CA.decode codec
```

To parse a serialized `String` into a `J.Json` structure use the [`Parser.jsonParser`](https://pursuit.purescript.org/packages/purescript-argonaut-core/docs/Data.Argonaut.Parser).

To "stringify" (serialize) your `Array String` to a serialized JSON `String` we would use the [`stringify`](https://pursuit.purescript.org/packages/purescript-argonaut-core/docs/Data.Argonaut.Core#v:stringify) like so:

```purescript
import Control.Category ((>>>))

serialize :: Array String -> String
serialize = encodeStringArray >>> J.stringify
```

### Basic codecs

A number of codecs are provided for basic types such as `Boolean`, `Number`, `Int`, `String`, `CodePoint`, `Char`, and are named as such but starting lowercase. So [`CA.boolean`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#v:boolean), [`CA.number`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#v:number), and so on.

There is also a `Json` "identity" codec called [`CA.json`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#v:json) that just passes the value through either way. This is sometimes useful when building up a larger codec. More on that in a moment.

The final two basic codecs are [`CA.null`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#v:null), which decodes to `Unit` in PureScript and encodes to `null` in JSON, and [`CA.void`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#v:void), which is an eliminator for `Void` in PureScript and will never actualy encode or decode anything since `Void` is uninhabited. This is another codec that is primarily intended for use in larger codecs.

So far so boring. Things only start getting interesting or useful when we can build up larger codecs for our data model or serialization format, which is where compound codecs come in to play.

### Arrays

The simplest compound codec provided by the library is [`CA.array`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#v:array), which accepts another codec, and encodes/decodes an arbitrary length array where all the items match the inner codec. For example:

```purescript
import Data.Codec.Argonaut as CA

codec ∷ CA.JsonCodec (Array String)
codec = CA.array CA.string
```

### Objects

Probably the most useful compound codec is for `Record`, this will generally be the building block of most codecs. There are a few different ways to define these codecs, but the most convenient is the [`record`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut.Record#v:record) function provided by [`Data.Codec.Argonaut.Record`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut.Record):

```purescript
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

type Person = { name ∷ String, age ∷ Int, active ∷ Boolean }

codec ∷ CA.JsonCodec Person
codec =
  CA.object "Person"
    (CAR.record
      { name: CA.string
      , age: CA.int
      , active: CA.boolean
      })
```

Note we also used a [`CA.object`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#v:object) wrapping this [`CAR.record`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut.Record#v:record). This allows us to name the record, for help when debugging decode failures, but is also because [`CAR.record`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut.Record#v:record) produces a [`JPropCodec`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#t:JPropCodec) rather than a [`JsonCodec`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#t:JsonCodec) directly. There are some other options for constructing and working with [`JPropCodec`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#t:JPropCodec) values, but that's out of the scope of this README.

The codec will encode/decode JSON objects of the same shape as the defining record. For example:

```json
{ "name": "Rashida", "age": 37, "active": true }
```

It's possible to encode/decode records that include properties with spaces and/or symbols in the name, or reserved names, by quoting the fields in the type and definition:

```purescript
type Person = { "Name" ∷ String, age ∷ Int, "is active" ∷ Boolean }

codec ∷ CA.JsonCodec Person
codec =
  CA.object "Person"
    (CAR.record
      { "Name": CA.string
      , age: CA.int
      , "is active": CA.boolean
      })
```

#### Optional properties

Objects with optional properties can be defined using the [`CAR.optional`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut.Record#v:optional): 

```purescript
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)

type Person =
  { name ∷ String
  , age ∷ Int
  , active ∷ Boolean
  , email ∷ Maybe String
  }

codec ∷ CA.JsonCodec Person
codec =
  CA.object "Person"
    (CAR.record
      { name: CA.string
      , age: CA.int
      , active: CA.boolean
      , email: CAR.optional CA.string
      })
```

If the value being decoded has no `email` field, the resulting `Person` will have `Nothing` for `email` now rather than failing to decode. When encoding, if an optional value is `Nothing`, the field will be omitted from the resulting JSON object.

This combinator only deals with entirely missing properties, so values like `null` will still need to be handled explicitly.

### Sum types and variants

This library comes with codec support for [`purescript-variant`](https://github.com/natefaubion/purescript-variant) out of the box and codecs for sums are often based on the variant codec.

First of all, variants. Similar to the object/record case there are a few options for defining variant codecs, but most commonly they will be defined with [`variantMatch`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut.Variant#v:variantMatch) provided by [`Data.Codec.Argonaut.Variant`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut.Variant):

```purescript
import Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Variant as V

type SomeValue = V.Variant
  ( str ∷ String
  , int ∷ Int
  , neither ∷ Unit
  )

codec ∷ CA.JsonCodec SomeValue
codec = CAV.variantMatch
  { str: Right CA.string
  , int: Right CA.int
  , neither: Left unit
  }
```

The fields in the record passed to [`CAV.variantMatch`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut.Variant#v:variantMatch) correspond with the variant constructors. Each one accepts an `Either` carrying either a codec or a static value - `Right` with a codec for when there's a value that needs encoding for the constructor, `Left` with a static value for nullary constructors.

The variant codec is a little opinionated since there's no exactly corresponding JSON structure for sums. The encoding looks something like:

```json
{ "tag": <constructorName>, "value": <value> }
```

`value` will be omitted for nullary / `Left`-defined constructors. At the moment it is not possible to customise the encoding for variant types, so they may not be suitable if you are not in control of the serialization format.

Sum type encoding is usually handled by building a variant codec, and then using [`dimap`](https://pursuit.purescript.org/packages/purescript-profunctor/docs/Data.Profunctor#v:dimap) to inject into/project out of a corresponding sum type:

```purescript
import Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Profunctor (dimap)
import Data.Variant as V
import Type.Proxy (Proxy(..))

data SomeValue2 = Str String | Int Int | Neither

codec ∷ CA.JsonCodec SomeValue2
codec =
  dimap toVariant fromVariant $ CAV.variantMatch
    { str: Right CA.string
    , int: Right CA.int
    , neither: Left unit
    }
  where
    toVariant = case _ of
      Str s → V.inj (Proxy ∷ _ "str") s
      Int i → V.inj (Proxy ∷ _ "int") i
      Neither → V.inj (Proxy ∷ _ "neither") unit
    fromVariant = V.match
      { str: Str
      , int: Int
      , neither: \_ → Neither
      }
```

This certainly is a little boilerplate-y, but at least when defining codecs this way you do gain the benefits of having a single definition that aligns the encoding and decoding behaviour. This means, assuming there are no mixups in `toVariant`/`fromVariant`, the guaranteed roundtripping is preserved. Often it's not even possible to have mixups during `dimap`, since the sum constructor types will all differ.

If you have a sum type that only consists of nullary constructors and it has a [`Generic`](https://pursuit.purescript.org/packages/purescript-generics-rep/docs/Data.Generic.Rep#t:Generic) instance defined, [`nullarySum`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut.Generic#v:nullarySum) provided by [`Data.Codec.Argonaut.Generic`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut.Generic) can generate a codec that will encode the constructors as string values matching the constructor names in the JSON.

The story for sum type codecs outside of these options isn't great just now. There are some functions provided in [`Data.Codec.Argonaut.Sum`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut.Sum) for defining them, but these are more error prone than the variant method, and use the same encoding methods described above. Often it's just as easy to construct a codec from scratch with [`basicCodec`](https://pursuit.purescript.org/packages/purescript-codec/docs/Data.Codec#v:basicCodec) from [`Data.Codec`](https://pursuit.purescript.org/packages/purescript-codec/docs/Data.Codec), although means it's up to you to ensure the roundtrip succeeds.

### Other common types

The library provides a [`Data.Codec.Argonaut.Common`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut.Common) module with codecs for `Maybe`, `Either`, `Tuple`, and so on. These codecs are somewhat opinionated, so only suitable for cases when you are in control of the serialization format.

There is also a [`Data.Codec.Argonaut.Compat`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut.Compat) module provided for codecs that need to preserve compatibility with the encoding using by [`purescript-argonaut-codecs`](https://github.com/purescript-contrib/purescript-argonaut-codecs). These codecs have some issues, like the inability to accurately encode nested `Maybe`s, so if possible, `Common` should be preferred.

### Newtypes

If you have a codec for a `newtype` with a [`Newtype`](https://pursuit.purescript.org/packages/purescript-newtype/docs/Data.Newtype#t:Newtype) instance, you can use the [`wrapIso`](https://pursuit.purescript.org/packages/purescript-profunctor/docs/Data.Profunctor#v:wrapIso) function from [`purescript-profunctor`](https://github.com/purescript/purescript-profunctor) to adapt a codec to work with the `newtype`. For example:

```purescript
import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)

type PersonRec = { "Name" ∷ String, age ∷ Int, "is active" ∷ Boolean }

newtype Person = Person PersonRec

derive instance newtypePerson ∷ Newtype Person _

codec ∷ CA.JsonCodec Person
codec =
  wrapIso Person
    (CAR.object "Person"
      { "Name": CA.string
      , age: CA.int
      , "is active": CA.boolean
      })
```

### "Prismatic" codecs

If you have a type with a pair of functions like the `preview` and `review` that make up a prism (`preview :: a -> Maybe b`, `review :: b -> a`), you can use these to adapt an existing codec to further refine it.

For example, to adapt the [`CA.string`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#v:string) codec to only work for `NonEmptyString`s:

```purescript
import Data.Codec.Argonaut as CA
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES

codec ∷ CA.JsonCodec NonEmptyString
codec = CA.prismaticCodec "NonEmptyString" NES.fromString NES.toString CA.string
```

See the documentation for [another example](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#v:prismaticCodec) of how [`CA.prismaticCodec`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#v:prismaticCodec) might be used. The main downside to [`CA.prismaticCodec`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#v:prismaticCodec) is the error reporting for the `Nothing` case might not be good as it otherwise could be, since [`UnexpectedValue`](https://pursuit.purescript.org/packages/purescript-codec-argonaut/docs/Data.Codec.Argonaut#t:JsonDecodeError) is the only information we have at that point.

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-codec-argonaut).
