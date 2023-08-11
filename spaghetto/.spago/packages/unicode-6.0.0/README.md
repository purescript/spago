# Unicode

[![CI](https://github.com/purescript-contrib/purescript-unicode/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-unicode/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-unicode.svg)](https://github.com/purescript-contrib/purescript-unicode/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-unicode/badge)](https://pursuit.purescript.org/packages/purescript-unicode)
[![Maintainer: cdepillabout](https://img.shields.io/badge/maintainer-cdepillabout-teal.svg)](https://github.com/cdepillabout)

A library for working with the properties of Unicode code points, including the general category of a code point, predicates for determining whether a code point is a letter or number character (`isLetter` and `isNumber`, for example), and case conversion functions (`toUpper`, `toLower`, and `toTitle`, as well as `caseFold` for caseless matching).

General functions for working with `String`s in terms of `CodePoint`s are found in the `Data.String.CodePoints` module in the [`strings` library](https://github.com/purescript/purescript-strings).

The version of the [Unicode standard](https://unicode.org/standard/standard.html) supported by this library can be found in the [`unicode-version`](./unicode-version) file.

## Installation

Install `unicode` with [Spago](https://github.com/purescript/spago):

```sh
spago install unicode
```

## Quick start

The quick start hasn't been written yet (contributions are welcome!). The quick start covers a common, minimal use case for the library, whereas longer examples and tutorials are kept in the [docs directory](./docs).

## Documentation

`unicode` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-unicode).
2. Written documentation is kept in the [docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-unicode/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `unicode` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-unicode/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
