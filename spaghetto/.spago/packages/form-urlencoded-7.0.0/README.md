# Form URLEncoded

[![CI](https://github.com/purescript-contrib/purescript-form-urlencoded/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-form-urlencoded/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-form-urlencoded.svg)](https://github.com/purescript-contrib/purescript-form-urlencoded/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-form-urlencoded/badge)](https://pursuit.purescript.org/packages/purescript-form-urlencoded)

A `FormURLEncoded` datatype represents an ordered list of key-value pairs with possible duplicates. The `encode` function allows to transform `FormURLEncoded` into a `Maybe String` according to `application/x-www-form-urlencoded`. The `decode` function transforms a string into a `Maybe FormURLEncoded` structure.

## Installation

Install `form-urlencoded` with [Spago](https://github.com/purescript/spago):

```sh
spago install form-urlencoded
```

## Quick start

Use the `encode` function to properly encode an array of key-value pairs, and the `decode` function to decode an encoded string.

```purs
> import Data.FormURLEncoded (fromArray, encode)
> import Data.Maybe (Maybe(..))
> import Data.Tuple (Tuple(..))

> encode $ fromArray 
>   [ Tuple "hey" Nothing
>   , Tuple "Oh" (Just "Let's go!")
>   ]
Just "hey&Oh=Let's+go!"

> decode "a=aa&b=bb"
Just (FormURLEncoded [(Tuple "a" (Just "aa")),(Tuple "b" (Just "bb"))])
```

## Documentation

`form-urlencoded` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-form-urlencoded).
2. Written documentation is kept in [the docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-form-urlencoded/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `form-urlencoded` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-form-urlencoded/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
