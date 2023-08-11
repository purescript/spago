# JS URI

[![CI](https://github.com/purescript-contrib/purescript-js-uri/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-js-uri/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-js-uri.svg)](https://github.com/purescript-contrib/purescript-js-uri/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-js-uri/badge)](https://pursuit.purescript.org/packages/purescript-js-uri)

URI encoding and decoding functions according to [RFC 3986](https://tools.ietf.org/html/rfc3986), implemented on top of JavaScript's builtin `encodeURIComponent` and `decodeURIComponent`. This library also contains support for encoding and decoding according to `application/x-www-form-urlencoded`.

## Installation

Install `js-uri` with [Spago](https://github.com/purescript/spago):

```sh
spago install js-uri
```

## Quick start

This library provides functions for encoding and decoding URIs and additional functions that match `application/x-www-form-urlencoded`. To encode or decode a URI according to RFC3896:

```purs
import JSURI (encodeURIComponent, decodeURIComponent)

> encodeURIComponent "https://purescript.org"
Just "https%3A%2F%2Fpurescript.org"

> encodeURIComponent "abc ABC"
Just "abc%20ABC"

> decodeURIComponent "https%3A%2F%2Fpurescript.org" == Just "https://purescript.org"
> decodeURIComponent "https%3A%2F%2Fpurescript.org?search+query" == Just "https://purescript.org?search+query"
```

To use `form-urlencoding` instead, which uses `+` for spaces instead of `%20`:

```purs
import JSURI (encodeFormURLComponent, decodeFormURLComponent)

> encodeFormURLComponent "abc ABC" == Just "abc+ABC"
> decodeFormURLComponent "https%3A%2F%2Fpurescript.org?search+query" == Just "https://purescript.org?search query"
```

## Documentation

`js-uri` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-js-uri).
2. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-js-uri/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `js-uri` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-js-uri/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
