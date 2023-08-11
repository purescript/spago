# Profunctor Lenses

[![CI](https://github.com/purescript-contrib/purescript-profunctor-lenses/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-profunctor-lenses/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-profunctor-lenses.svg)](https://github.com/purescript-contrib/purescript-profunctor-lenses/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-profunctor-lenses/badge)](https://pursuit.purescript.org/packages/purescript-profunctor-lenses)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-teal.svg)](https://github.com/garyb)

Pure profunctor lenses: a mechanism for updating, viewing, and setting values within nested data structures.

Learn more about profunctor lenses with:

- [Practical Profunctor Optics & Lenses in PureScript](https://thomashoneyman.com/articles/practical-profunctor-lenses-optics/), a practical introduction to profunctor optics in PureScript
- [Lenses for the Mere Mortal](https://leanpub.com/lenses), a book about lenses in PureScript

## Installation

Install `profunctor-lenses` with [Spago](https://github.com/purescript/spago):

```sh
spago install profunctor-lenses
```

## Quick start

```purescript
> structure = Tuple (Tuple (Tuple "hi!" 3) 2) 1

> import Data.Lens
> _leftmost = _1 <<< _1 <<< _1

> view _leftmost structure
"hi!"

> set _leftmost "Bye!" structure
(Tuple (Tuple (Tuple "Bye!" 3) 2) 1)

> over _leftmost String.toUpper structure
(Tuple (Tuple (Tuple "HI!" 3) 2) 1)
```

You can try out the [examples](./examples) in the REPL by running:

```
spago -x examples.dhall repl
```

## Documentation

`profunctor-lenses` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-profunctor-lenses).
1. Usage examples can be found in [the test suite](./test) and [`examples`](./examples) directory.
1. [Practical Profunctor Optics & Lenses in PureScript](https://thomashoneyman.com/articles/practical-profunctor-lenses-optics/), a practical introduction to profunctor optics in PureScript
1. [Lenses for the Mere Mortal](https://leanpub.com/lenses), a book about lenses in PureScript

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-profunctor-lenses/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `profunctor-lenses` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-profunctor-lenses/issues). We'll do our best to work with you to resolve or answer it.

1. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

1. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
