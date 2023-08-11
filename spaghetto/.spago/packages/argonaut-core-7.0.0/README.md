# Argonaut Core

[![CI](https://github.com/purescript-contrib/purescript-argonaut-core/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-argonaut-core/actions?query=workflow%3ACI+branch%3Amain)
[![Release](http://img.shields.io/github/release/purescript-contrib/purescript-argonaut-core.svg)](https://github.com/purescript-contrib/purescript-argonaut-core/releases)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-argonaut-core/badge)](http://pursuit.purescript.org/packages/purescript-argonaut-core)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-teal.svg)](http://github.com/garyb)

The core `Json` type for the [Argonaut](https://github.com/purescript-contrib/purescript-argonaut) libraries, along with basic parsing, printing, and folding functions which operate on it.

## Installation

Install `argonaut-core` with [Spago](https://github.com/purescript/spago):

```sh
spago install argonaut-core
```

or install it as part of the [Argonaut](https://github.com/purescript-contrib/purescript-argonaut) bundle:

```sh
spago install argonaut
```

## Quick start

Use the `Json` type to represent JSON data in PureScript. You can produce a value of `Json` via the FFI or by functions from `Data.Argonaut.Core`.

For example, via the FFI:

```js
// In an FFI module
exports.someNumber = 23.6;
exports.someObject = { people: [{ name: "John" }, { name: "Jane" }] };
```

```purs
foreign import someNumber :: Json
foreign import someObject :: Json
```

In general, if a JavaScript value could be returned from a call to `JSON.parse` then you can import it via the FFI as `Json`. That includes objects, booleans, numbers, strings, and arrays (but not things like functions).

You can also use the construction functions which follow the naming convention `fromX` or `jsonX`:

```purs
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Data.Argonaut.Core as A

someNumber :: Json
someNumber = A.fromNumber 23.6

someObject :: Json
someObject =
  A.fromObject
    ( Object.fromFoldable
        [ Tuple "people"
            ( A.fromArray
                [ A.jsonSingletonObject "name" (A.fromString "John")
                , A.jsonSingletonObject "name" (A.fromString "Jane")
                ]
            )
        ]
    )
```

Finally, you can parse JSON from a string using the `jsonParser` function. However, this isn't guaranteed to produce a correct value, so it returns an `Either` value, where a parsing error is represented with `Left` containing an error message.

```purs
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe (Maybe(..))

someObject :: Either String Json
someObject = jsonParser
  """
  { people: [{ name: "John" }, { name: "Jane" }] };
  """
```

See the [docs](./docs) for an in-depth overview of the rest of the Argonaut Core library. You may also be interested in other libraries in the Argonaut ecosystem:

- [purescript-argonaut-codecs](https://github.com/purescript-contrib/purescript-argonaut-codecs) provides codecs based on `EncodeJson` and `DecodeJson` type classes, along with instances for common data types and combinators for encoding and decoding `Json` values.
- [purescript-codec-argonaut](https://github.com/garyb/purescript-codec-argonaut) supports an alternative approach for codecs, which are based on profunctors instead of type classes.
- [purescript-argonaut-traversals](https://github.com/purescript-contrib/purescript-argonaut-traversals) defines prisms, traversals, and zippers for the `Json` type.
- [purescript-argonaut-generic](https://github.com/purescript-contrib/purescript-argonaut-generic) supports generic encoding and decoding for any type with a `Generic` instance.

## Documentation

`argonaut-core` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-argonaut-core).
2. Written documentation is kept in [the docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-argonaut-core/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `argonaut-core` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-argonaut-core/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
