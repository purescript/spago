# Affjax

[![CI](https://github.com/purescript-contrib/purescript-affjax/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-affjax/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-affjax.svg)](https://github.com/purescript-contrib/purescript-affjax/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-affjax/badge)](https://pursuit.purescript.org/packages/purescript-affjax)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-teal.svg)](https://github.com/garyb)

A library taking advantage of [`aff`](https://github.com/purescript-contrib/purescript-aff) to enable pain-free asynchronous AJAX requests and response handling.

This library provides types and common functionality that work across environments (e.g. Node, browser), but **it does not work out-of-box**. Rather, use the environment-specific library instead:
- Browser environment: [`purescript-affjax-web`](https://github.com/purescript-contrib/purescript-affjax-web)
- Node environment: [`purescript-affjax-node`](https://github.com/purescript-contrib/purescript-affjax-node)

## Documentation

`affjax` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-affjax).
2. Written documentation is kept in the [docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-affjax/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `affjax` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-affjax/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
