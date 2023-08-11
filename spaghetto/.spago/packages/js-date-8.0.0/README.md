# JS Date

[![CI](https://github.com/purescript-contrib/purescript-js-date/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-js-date/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-js-date.svg)](https://github.com/purescript-contrib/purescript-js-date/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-js-date/badge)](https://pursuit.purescript.org/packages/purescript-js-date)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-teal.svg)](https://github.com/garyb)

JavaScript's native date type and corresponding functions.

This library provides the `JSDate` type and associated functions for interop purposes with JavaScript, but for working with dates in PureScript it is recommended that `DateTime` representation is used - `DateTime` offers greater type safety, a more PureScript-friendly interface, and has a `Generic` instance. There is a `toDateTime` provided for this conversion.

## Installation

Install `js-date` with [Spago](https://github.com/purescript/spago):

```sh
spago install js-date
```

## Quick start

The quick start hasn't been written yet (contributions are welcome!). The quick start covers a common, minimal use case for the library, whereas longer examples and tutorials are kept in the [docs directory](./docs).

## Documentation

`js-date` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-js-date).
2. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-js-date/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `js-date` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-js-date/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
