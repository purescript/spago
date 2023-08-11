# HTTP Methods

[![CI](https://github.com/purescript-contrib/purescript-http-methods/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-http-methods/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-http-methods.svg)](https://github.com/purescript-contrib/purescript-http-methods/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-http-methods/badge)](https://pursuit.purescript.org/packages/purescript-http-methods)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-teal.svg)](https://github.com/garyb)

HTTP method type. The definition of the type is based on HTTP/1.1 with [RFC 2518](https://tools.ietf.org/html/rfc2518) and [RFC 5789](https://tools.ietf.org/html/rfc5789).

## Installation

Install `http-methods` with [Spago](https://github.com/purescript/spago):

```sh
spago install http-methods
```

## Quick start

```purescript
module PrintingExample where

import Data.HTTP.Method (Method(..), print)
import Data.Either (Either(..))

-- To print an HTTP method, use the `print` function:

-- same as "GET"
getMethod :: String
getMethod = print (Left GET)
```

```purescript
module ParsingExample where

import Data.HTTP.Method (Method(..), CustomMethod(..), parse, fromString)
import Data.Either (Either(..))

-- To parse an HTTP method, use the `fromString` function:

-- same as `Left GET`.
getMethod :: Either Method CustomMethod
getMethod = fromString "GET"

-- Any methods not defined by `Method` will be parsed as a custom method:

-- same as `Right (CustomMethod "FOO")`
fooMethod :: Either Method CustomMethod
fooMethod = fromString "FOO"

-- If you want to handle the parsing in a more controlled way,
-- use `parse` directly.

-- Only accept methods defined in the Method type
-- and ignore all other custom methods
maybeMethod :: String -> Maybe Method
maybeMethod = parse Just (\_ -> Nothing)
```

## Documentation

`http-methods` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-http-methods).
2. See [the changelog](./CHANGELOG.md).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-http-methods/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `http-methods` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-http-methods/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
