# Now

[![CI](https://github.com/purescript-contrib/purescript-now/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-now/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-now.svg)](https://github.com/purescript-contrib/purescript-now/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-now/badge)](https://pursuit.purescript.org/packages/purescript-now)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-teal.svg)](https://github.com/garyb)

Effect type and function for accessing the current machine's date and time.

## Installation

Install `now` with [Spago](https://github.com/purescript/spago):

```sh
spago install now
```

## Quick start

The functions provided by `now` can be used to get the current date and time from the current machine's system clock.

Here are two example functions that print out the current year and time:

```purs
import Prelude
import Data.DateTime
import Data.Enum
import Effect
import Effect.Console
import Effect.Now

printCurrentYear :: Effect Unit
printCurrentYear = do
  currentDate <- nowDate
  let
    currentYear = fromEnum $ year currentDate :: Int
  log $ "The current year is " <> show currentYear

printCurrentTime :: Effect Unit
printCurrentTime = do
  currentTime <- nowTime
  let
    currentHour = fromEnum $ hour currentTime :: Int
    currentMinute = fromEnum $ minute currentTime :: Int
  log $ "The current time is " <> show currentHour <> ":" <> show currentMinute
```

We can then try these functions out in a REPL:

```
> printCurrentYear
The current year is 2021

> printCurrentTime
The current time is 1:54
```

## Documentation

`now` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-now).
2. Written documentation is kept in the [docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-now/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `now` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-now/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
