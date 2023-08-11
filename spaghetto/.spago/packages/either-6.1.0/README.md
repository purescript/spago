# purescript-either

[![Latest release](http://img.shields.io/github/release/purescript/purescript-either.svg)](https://github.com/purescript/purescript-either/releases)
[![Build status](https://github.com/purescript/purescript-either/workflows/CI/badge.svg?branch=master)](https://github.com/purescript/purescript-either/actions?query=workflow%3ACI+branch%3Amaster)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-either/badge)](https://pursuit.purescript.org/packages/purescript-either)

The `Either` type provides is used to represent values that can be one of two possibilities. For example, `Either Int Number` can be used in a place where either integers or floating point numbers are acceptable.

A common use for this type is error handling, where by convention the `Left` constructor carries error messages and the `Right` constructor carries expected values (“`Right` is right”). This convention is used as the `Functor`, `Applicative`, `Monad`, etc. instances all operate on the `Right` part of the `Either`.

## Installation

```
spago install either
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-either).
